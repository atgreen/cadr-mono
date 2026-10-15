# SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: OFL-1.1
"""Extract one release's notes from CHANGELOG.md.

The releases page should say what changed, and the only way that stays
true is to generate it from the file the change was written in.  Typing
the notes a second time into `gh release create` is how they drift.

A section is a level-two heading matching the version exactly, or the
version followed by a date.  Headings inside fenced code blocks are
ignored, because a changelog that documents shell usage will contain
lines starting with ## that are not headings.

    python3 build/release/notes.py 1.0.0 > NOTES.md
"""
import argparse, re, sys, pathlib


def section(changelog: str, wanted: str) -> str:
    lines = changelog.splitlines(keepends=True)
    headings, fence = [], None
    for i, line in enumerate(lines):
        m = re.match(r"^ {0,3}(`{3,}|~{3,})", line)
        if fence:
            if m and m[1][0] == fence[0] and len(m[1]) >= len(fence):
                fence = None
            continue
        if m:
            fence = m[1]
            continue
        h = re.fullmatch(r"##[ \t]+(.+?)\s*", line)
        if h:
            headings.append((i, h[1]))

    pat = re.compile(re.escape(wanted) + r"(?: +- +\d{4}-\d{2}-\d{2})?")
    hits = [(n, i) for n, (i, title) in enumerate(headings) if pat.fullmatch(title)]
    if len(hits) != 1:
        raise SystemExit(
            f"CHANGELOG.md must have exactly one '## {wanted}' section, found {len(hits)}")
    n, start = hits[0]
    end = headings[n + 1][0] if n + 1 < len(headings) else len(lines)
    body = "".join(lines[start + 1:end]).strip("\r\n")
    if not body.strip():
        raise SystemExit(f"CHANGELOG.md section '{wanted}' is empty")
    return body


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("version", help="e.g. 1.0.0, or v1.0.0")
    ap.add_argument("--changelog", default="CHANGELOG.md")
    a = ap.parse_args()
    text = pathlib.Path(a.changelog).read_text()
    sys.stdout.write(section(text, a.version.lstrip("v")) + "\n")
