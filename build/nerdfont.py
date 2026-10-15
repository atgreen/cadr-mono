"""Merge the Nerd Font icon sets into the generated UFOs.

This is the job font-patcher does, done against fontTools instead of
FontForge, and done BEFORE fontmake rather than to a finished TTF --
so the icons go through ttfautohint with everything else, and the Lisp
generator stays untouched.  Nothing drawn enters src/; the icons are
vendored artwork that a build step merges in, and the base font never
sees them.

WHAT THE SOURCES ARE.  Thirteen fonts in src/nerdfont/, pinned to Nerd
Fonts v3.5.1 (commit b894ea78), in units-per-em from 300 to 2048 and
in both glyf and CFF flavours -- which is why fontTools does the
reading and why a hand-written Lisp parser was rejected.

WHAT THE MAPPING IS.  src/nerdfont/patchset.json is font-patcher's own
table, extracted verbatim: 28 ranges saying which source codepoints
land at which destination codepoints, and which groups of icons must
be scaled together.  That last part is the whole reason not to invent
our own fitting: the star and the half-star have different bounding
boxes and must still come out the same size, and only a group scale
gets that right.

WHAT THE FITTING IS.  Measured off a reference patched font rather
than guessed: icons occupy a square of side ADVANCE, centred on the
line-box centre, aspect preserved.  In the reference that is a 600x600
box centred at y=360 in a 600-advance, 1320-tall cell -- median icon
ink width exactly 600, band exactly 600 tall.

WHAT WE SKIP, and why:
  box drawing U+2500-259F  we compute it from the cell, better
  braille    U+2800-28FF   likewise, all 256
  U+E0B0-E0BF              our generated powerline separators win
  extraglyphs.sfd          FontForge's native format, unreadable here;
                           costs the heavy angle brackets and the
                           progress bars
"""

# SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: OFL-1.1
import json, os, pathlib, sys, xml.sax.saxutils as sx
from fontTools.ttLib import TTFont
from fontTools.pens.recordingPen import RecordingPen
from fontTools.pens.boundsPen import BoundsPen

ROOT = pathlib.Path(__file__).resolve().parent.parent
SRC = ROOT / "src" / "nerdfont"

# Ours wins over upstream's: we generate these from the cell, and
# better -- box drawing and braille are computed to meet across the
# cell boundary, which no scaled artwork can promise.
#
# The powerline separators are NOT in this list, deliberately.  We
# generate U+E0B0-E0BF for the base font, where they are the whole
# point: a prompt that renders with no third-party artwork and no
# dependency.  But upstream ships the surrounding block too -- E0A0-A3
# and E0C0-D7 -- and taking sixteen of those codepoints from us and
# the rest from them would mix two sets of drawing decisions inside
# one prompt.  In the NF variant upstream owns the whole block.
OURS = [(0x2500, 0x259F), (0x2800, 0x28FF)]


def ours(cp):
    return any(a <= cp <= b for a, b in OURS)


def fontinfo(ufo):
    """advance, line-box centre and upm, read from the UFO itself."""
    text = (ufo / "fontinfo.plist").read_text()

    def val(key, kind="integer"):
        i = text.index(f"<key>{key}</key>")
        j = text.index(f"<{kind}>", i) + len(kind) + 2
        return int(text[j:text.index(f"</{kind}>", j)])

    upm = val("unitsPerEm")
    asc, desc = val("openTypeHheaAscender"), val("openTypeHheaDescender")
    # advance: every glyph has the same one, so take it off any glif
    g = next((ufo / "glyphs").glob("uni2500.glif"), None) or \
        next((ufo / "glyphs").glob("*.glif"))
    t = g.read_text()
    i = t.index('<advance width="') + len('<advance width="')
    adv = int(t[i:t.index('"', i)])
    return upm, adv, (asc + desc) // 2


def group_of(scale_rules):
    """codepoint -> group id, from a ScaleRules block."""
    out = {}
    if not scale_rules:
        return out
    for n, grp in enumerate(scale_rules.get("ScaleGroups") or []):
        if isinstance(grp, dict) and "range" in grp:
            grp = range(grp["range"][0], grp["range"][1])
        for cp in grp:
            out[cp] = n
    return out


def glif(name, cp, adv, contours):
    """A UFO glif.  Quadratic points come straight through as qcurve/
    offcurve -- the same vocabulary src/ufo.lisp already writes."""
    out = [f'<?xml version="1.0" encoding="UTF-8"?>',
           f'<glyph name="{sx.escape(name)}" format="2">',
           f'  <advance width="{adv}"/>',
           f'  <unicode hex="{cp:04X}"/>']
    if contours:
        out.append("  <outline>")
        for c in contours:
            out.append("    <contour>")
            for kind, x, y in c:
                k = f' type="{kind}"' if kind else ""
                out.append(f'      <point x="{round(x)}" y="{round(y)}"{k}/>')
            out.append("    </contour>")
        out.append("  </outline>")
    out.append("</glyph>")
    return "\n".join(out) + "\n"


def contours_of(pen_value):
    """RecordingPen output -> UFO point lists."""
    out, cur = [], None
    for op, args in pen_value:
        if op == "moveTo":
            cur = [("move", *args[0])]
        elif op == "lineTo":
            cur.append(("line", *args[0]))
        elif op == "qCurveTo":
            # a pen call passes its points as separate arguments, so
            # ARGS is the point list itself -- not args[0]
            if cur is None:
                # TrueType contour with NO on-curve point anywhere: the
                # pen emits this with no moveTo in front of it
                cur = []
            pts = list(args)
            if pts and pts[-1] is None:
                # TrueType's all-offcurve contour: no on-curve point at
                # all, the start is implied midway between the last and
                # first control points
                pts = pts[:-1]
                for q in pts:
                    cur.append((None, *q))
            else:
                for q in pts[:-1]:
                    cur.append((None, *q))
                cur.append(("qcurve", *pts[-1]))
        elif op == "curveTo":
            pts = list(args)
            for q in pts[:-1]:
                cur.append((None, *q))
            cur.append(("curve", *pts[-1]))
        elif op == "closePath" and cur:
            # a UFO contour is implicitly closed; the leading moveTo
            # becomes an ordinary on-curve point
            cur[0] = ("line" if cur[0][0] == "move" else cur[0][0],
                      cur[0][1], cur[0][2])
            out.append(cur)
            cur = None
        elif op == "endPath" and cur:
            out.append(cur)
            cur = None
    return out


def rename(ufo):
    """CADR Mono -> CADR Mono NF.  Two fonts cannot share a family name
    and differ by ten thousand glyphs: a user installing both would get
    whichever fontconfig picked.  The grade stays in the family, as it
    does for the base font, so the four shipped families are
    CADR Mono NF and CADR Mono NF Dark, each regular and dark."""
    fi = ufo / "fontinfo.plist"
    t = fi.read_text()
    for old, new in (("<string>CADR Mono Dark</string>",
                      "<string>CADR Mono NF Dark</string>"),
                     ("<string>CADR Mono</string>",
                      "<string>CADR Mono NF</string>")):
        if old in t:
            t = t.replace(old, new, 1)
            break
    fi.write_text(t)


def merge(ufo, verbose=False):
    upm, adv, midy = fontinfo(ufo)
    box = adv                                   # icons fill a square of the advance
    gdir = ufo / "glyphs"
    patch = json.loads((SRC / "patchset.json").read_text())
    added, skipped_ours, missing = 0, 0, 0
    newnames = {}

    for entry in patch:
        fn = entry.get("Filename")
        if not fn:
            continue
        path = SRC / os.path.basename(fn)
        if not path.exists():                   # .sfd and friends
            continue
        s = entry.get("SymStart")
        e = entry.get("SymEnd")
        if s is None or e is None or (s == 0 and e == 0):
            continue
        try:
            sf = TTFont(path, fontNumber=0, lazy=True)
        except Exception:
            continue
        scmap = sf.getBestCmap()
        sgs = sf.getGlyphSet()
        supm = sf["head"].unitsPerEm
        src_start = entry.get("SrcStart")
        exact = entry.get("Exact")
        groups = group_of(entry.get("ScaleRules"))

        # one scale per group: union bbox over the whole family
        gbox = {}
        for srccp in range(s, e + 1):
            dest = srccp if exact or src_start is None else srccp - s + src_start
            g = scmap.get(srccp)
            if g is None:
                continue
            gid = groups.get(srccp)
            if gid is None:
                continue
            bp = BoundsPen(sgs)
            try:
                sgs[g].draw(bp)
            except Exception:
                continue
            if not bp.bounds:
                continue
            b = gbox.setdefault(gid, list(bp.bounds))
            b[0] = min(b[0], bp.bounds[0]); b[1] = min(b[1], bp.bounds[1])
            b[2] = max(b[2], bp.bounds[2]); b[3] = max(b[3], bp.bounds[3])

        for srccp in range(s, e + 1):
            # SymStart..SymEnd is the range in the SYMBOL font and
            # SrcStart is where it lands in the font being patched --
            # font-patcher calls the font it is patching "sourceFont",
            # so the field names read backwards.  Getting this the
            # wrong way round silently yields the wrong icon rather
            # than an error, which is how it was caught: rendering a
            # grid beside a reference and seeing different pictures.
            dest = srccp if exact or src_start is None else srccp - s + src_start
            if ours(dest):
                skipped_ours += 1
                continue
            g = scmap.get(srccp)
            if g is None:
                missing += 1
                continue
            pen = RecordingPen()
            try:
                sgs[g].draw(pen)
            except Exception:
                continue
            cs = contours_of(pen.value)
            if not cs:
                continue
            bp = BoundsPen(sgs)
            sgs[g].draw(bp)
            if not bp.bounds:
                continue
            gid = groups.get(srccp)
            x0, y0, x1, y1 = gbox[gid] if gid in gbox else bp.bounds
            w, h = max(1.0, x1 - x0), max(1.0, y1 - y0)
            k = box / max(w, h)                 # preserve aspect, fit the square
            # centre the GROUP's box, so grouped icons keep their
            # relative sizes and sit on a common baseline
            cx, cy = (x0 + x1) / 2, (y0 + y1) / 2
            shifted = [[(kind, (x - cx) * k + adv / 2, (y - cy) * k + midy)
                        for kind, x, y in c] for c in cs]
            name = f"uni{dest:04X}" if dest <= 0xFFFF else f"u{dest:X}"
            (gdir / f"{name}.glif").write_text(glif(name, dest, adv, shifted))
            newnames[name] = f"{name}.glif"
            added += 1
        sf.close()

    # contents.plist: append, don't rewrite -- the generator owns the rest
    cp = gdir / "contents.plist"
    text = cp.read_text()
    ins = "".join(f"  <key>{n}</key>\n  <string>{f}</string>\n"
                  for n, f in sorted(newnames.items()))
    cp.write_text(text.replace("</dict>", ins + "</dict>", 1))
    if verbose:
        print(f"  {ufo.name:<34} +{added} icons  "
              f"({skipped_ours} ours, {missing} absent upstream)")
    return added


if __name__ == "__main__":
    ufos = sorted((ROOT / "build").glob("*.ufo"))
    if not ufos:
        sys.exit("no UFOs; run the generator first")
    total = 0
    for u in ufos:
        total += merge(u, verbose=True)
        rename(u)
    print(f"merged {total} icon glyphs across {len(ufos)} fonts")
