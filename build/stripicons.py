"""Drop the hinting instructions from icon glyphs.

Hinting is load-bearing for the text of this font -- at 13px every
horizontal stroke is sub-pixel, which is the whole argument in
hint.py.  It does nothing for a Nerd Font icon: those are full-cell
pictograms with no stems to snap and no stroke that has to land on a
pixel boundary.

Measured on the merged font before this step:

    text    722 glyphs ->     45,690 instruction bytes
    icons 10,554 glyphs ->  1,659,216 instruction bytes

So 97% of the bytecode was instrumenting artwork, costing ~1.7MB per
font and ~13MB across the eight.

This is the ordering the ecosystem already uses, reached a different
way: font-patcher merges icons INTO an already-hinted font, so the
icons never get instructed at all.  Our pipeline merges into the UFO
and hints afterwards, which is simpler and keeps one toolchain -- but
it instruments everything, so the icons have to be stripped back out.
Maple's own NF base font carries no instructions whatsoever.
"""

# SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: OFL-1.1
import argparse, pathlib
from fontTools.ttLib import TTFont

ICON = lambda cp: 0xE000 <= cp <= 0xF8FF or cp >= 0xF0000

ap = argparse.ArgumentParser()
ap.add_argument("--dir", default="fonts-nf")
args = ap.parse_args()

for path in sorted(pathlib.Path(args.dir).glob("*.ttf")):
    f = TTFont(path)
    glyf = f["glyf"]
    rev = {v: k for k, v in f.getBestCmap().items()}
    before = path.stat().st_size
    stripped = 0
    for name in f.getGlyphOrder():
        cp = rev.get(name)
        if cp is None or not ICON(cp):
            continue
        g = glyf[name]
        g.expand(glyf)
        prog = getattr(g, "program", None)
        if prog is not None and prog.getBytecode():
            prog.fromBytecode(b"")
            stripped += 1
    f.save(path)
    after = path.stat().st_size
    print(f"  {path.name:<30} {stripped:6d} icons unhinted   "
          f"{before/1024/1024:5.2f} -> {after/1024/1024:5.2f} MB")
