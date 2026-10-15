"""Hint the compiled statics with ttfautohint.

Hinting is not a finishing step for this font, it is load-bearing.  At
2048 upm a whole pixel is 158 units at 13px; the thin is 127 and the
dark grade's is 115, so every horizontal stroke is sub-pixel at the sizes
this font exists for.  Unhinted, they wash out to grey.  Instructions
snap them to whole pixels, which is what makes the humanist modulation
affordable at all.

ttfautohint does not support variable fonts and looks unmaintained,
which is one reason the statics are the real deliverable here.
"""

# SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: OFL-1.1
import argparse, pathlib
from ttfautohint import ttfautohint

# Defaults are the base font's paths; the Nerd Font family passes its own
# so it gets the identical hinting rather than a second copy of these
# settings that could drift.
ap = argparse.ArgumentParser()
ap.add_argument("--in", dest="src", default="build/ttf")
ap.add_argument("--out", dest="out", default="build/ttf-hinted")
args = ap.parse_args()

SRC = pathlib.Path(args.src)
OUT = pathlib.Path(args.out)
OUT.mkdir(parents=True, exist_ok=True)

for src in sorted(SRC.glob("*.ttf")):
    dst = OUT / src.name
    data = ttfautohint(
        in_file=str(src),
        hinting_range_min=8,
        hinting_range_max=50,
        hinting_limit=200,
        # 1 = strong: stems snapped hard to the pixel grid.  Terminals
        # render with grayscale antialiasing, and ttfautohint's default
        # leaves grayscale merely quantised (0); crispness at 11-14px is
        # the whole point here, so grayscale gets strong too.
        gray_stem_width_mode=1,
        gdi_cleartype_stem_width_mode=1,
        dw_cleartype_stem_width_mode=1,
        increase_x_height=14,
        default_script="latn",
        # Box drawing and braille are unclassified; without a fallback
        # they would be left unhinted, and a rule that does not land on
        # the pixel grid is exactly the blur we are removing.
        fallback_script="latn",
        no_info=False,
    )
    dst.write_bytes(data)
    print(f"hinted {src.name} -> {dst}  ({len(data):,} bytes)")
