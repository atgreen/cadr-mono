# SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: OFL-1.1
"""Emit a CycloneDX SBOM for a release.

A font is not usually thought of as having a supply chain, and this one
mostly does not: every outline in the base font is computed by code in
this repository, so there is nothing to declare but the toolchain.

The Nerd Font build is the opposite case and the reason this exists.
It merges eleven third-party icon sets under four different licences,
and anyone redistributing it inherits those obligations. An SBOM is how
that travels with the artifact instead of living in a README nobody
reads.

    python3 build/release/sbom.py --version 1.0.0 --out sbom.cdx.json
"""
import argparse, datetime, hashlib, json, pathlib, re, subprocess, sys, uuid

ROOT = pathlib.Path(__file__).resolve().parents[2]


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for block in iter(lambda: fh.read(1 << 20), b""):
            h.update(block)
    return h.hexdigest()


def tool_versions(venv):
    """What actually built this, with versions, read from the venv."""
    out = []
    pip = venv / "bin" / "pip"
    if pip.exists():
        try:
            listing = json.loads(subprocess.run(
                [str(pip), "list", "--format=json"], capture_output=True,
                text=True, check=True).stdout)
            keep = {"fontmake", "fonttools", "skia-pathops", "ttfautohint-py",
                    "freetype-py", "pillow", "ufo2ft", "cu2qu", "booleanoperations"}
            for p in listing:
                if p["name"].lower() in keep:
                    out.append((p["name"], p["version"], "pypi"))
        except Exception:
            pass
    try:
        v = subprocess.run(["sbcl", "--version"], capture_output=True, text=True).stdout.strip()
        out.append(("sbcl", v.split()[-1] if v else "unknown", "generic"))
    except Exception:
        pass
    return out


def vendored_icons():
    """The icon sets, parsed from the provenance table we already keep.

    Read rather than restated: a second copy of this list would be a
    second thing to forget to update."""
    notice = ROOT / "src" / "nerdfont" / "LICENSES" / "NOTICE.md"
    if not notice.exists():
        return []
    rows = []
    for line in notice.read_text().splitlines():
        m = re.match(r"\|\s*`([^`]+)`\s*\|\s*([^|]+?)\s*\|\s*([^|]+?)\s*\|\s*([^|]+?)\s*\|", line)
        if m and not m[1].startswith("file"):
            rows.append(dict(file=m[1], name=m[2], upstream=m[3], licence=m[4]))
    return rows


SPDX = {"MIT": "MIT", "CC BY 4.0": "CC-BY-4.0", "Apache 2.0": "Apache-2.0",
        "OFL 1.1": "OFL-1.1"}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--version", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--fonts", default="fonts", help="directory of shipped TTFs")
    ap.add_argument("--nerd-fonts", default=None, help="directory of NF TTFs, if built")
    a = ap.parse_args()

    now = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    components = []

    for d, note in ((a.fonts, "base"), (a.nerd_fonts, "nerd-font")):
        if not d:
            continue
        for ttf in sorted(pathlib.Path(d).glob("*.ttf")):
            components.append({
                "type": "file", "bom-ref": f"font/{ttf.name}", "name": ttf.name,
                "version": a.version, "scope": "required",
                "hashes": [{"alg": "SHA-256", "content": sha256(ttf)}],
                "licenses": [{"license": {"id": "OFL-1.1"}}],
                "properties": [{"name": "cadr-mono:build", "value": note}],
            })

    for name, version, kind in tool_versions(ROOT / "build" / "venv"):
        components.append({
            "type": "library", "bom-ref": f"tool/{name}", "name": name,
            "version": version, "scope": "excluded",
            "purl": f"pkg:{kind}/{name}@{version}",
            "properties": [{"name": "cadr-mono:role", "value": "build-tool"}],
        })

    for row in vendored_icons():
        lic = SPDX.get(row["licence"].replace("**", "").strip())
        components.append({
            "type": "library", "bom-ref": f"icons/{row['file']}",
            "name": row["name"], "version": "nerd-fonts-v3.5.1",
            "scope": "required" if a.nerd_fonts else "excluded",
            "externalReferences": [{"type": "distribution", "url": row["upstream"].split()[0]}],
            **({"licenses": [{"license": {"id": lic}}]} if lic else {}),
            "properties": [
                {"name": "cadr-mono:role", "value": "vendored-icon-set"},
                {"name": "cadr-mono:file", "value": f"src/nerdfont/{row['file']}"},
            ],
        })

    bom = {
        "bomFormat": "CycloneDX", "specVersion": "1.5",
        "serialNumber": f"urn:uuid:{uuid.uuid4()}", "version": 1,
        "metadata": {
            "timestamp": now,
            "tools": {"components": [{"type": "application", "name": "cadr-mono sbom.py",
                                      "version": a.version}]},
            "component": {
                "type": "library", "bom-ref": "cadr-mono", "name": "Cadr Mono",
                "version": a.version,
                "description": "A monospace typeface for terminals and code, generated from Common Lisp.",
                "licenses": [{"license": {"id": "OFL-1.1"}}],
                "externalReferences": [
                    {"type": "vcs", "url": "https://github.com/atgreen/cadr-mono"}],
            },
            "authors": [{"name": "Anthony Green"}],
        },
        "components": components,
    }
    pathlib.Path(a.out).write_text(json.dumps(bom, indent=2) + "\n")
    n = len(components)
    print(f"wrote {a.out}: {n} components "
          f"({sum(1 for c in components if c['type']=='file')} fonts, "
          f"{sum(1 for c in components if c.get('properties',[{}])[0].get('value')=='build-tool')} tools, "
          f"{sum(1 for c in components if 'icons/' in c['bom-ref'])} icon sets)")


if __name__ == "__main__":
    main()
