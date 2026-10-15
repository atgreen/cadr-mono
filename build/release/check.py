# SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: OFL-1.1
"""Invariants that must hold in every shipped font. Exits non-zero.

These are facts with right answers, not judgements: the README states
them and a build that breaks one is broken. Deliberately NOT here are
the shape instruments (build/confusion.py, build/archetype.py), which
ask questions with no right answer -- wiring those to a threshold
would drive the font toward the average of its competitors.

    python3 build/release/check.py fonts/
"""
import argparse, glob, pathlib, sys
from fontTools.ttLib import TTFont
from fontTools.pens.boundsPen import BoundsPen

ADVANCE = 1229
# Box drawing, blocks and the powerline separators overflow the cell on
# purpose: that is what makes them meet across the boundary.
BLEED = [(0x2500, 0x259F), (0xE0A0, 0xE0D7)]
BASELINE = pathlib.Path(__file__).with_name("overflow-baseline.json")


def bleeds(cp):
    return any(a <= cp <= b for a, b in BLEED)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("dir", nargs="?", default="fonts")
    ap.add_argument("--advance", type=int, default=ADVANCE)
    ap.add_argument("--update-baseline", action="store_true",
                    help="record the current overflow counts as the new ceiling")
    a = ap.parse_args()
    paths = sorted(glob.glob(f"{a.dir}/*.ttf"))
    if not paths:
        sys.exit(f"no fonts in {a.dir}")

    fails, sets, metrics = [], {}, {}
    overflow, examples = {}, {}
    for p in paths:
        name = pathlib.Path(p).name
        f = TTFont(p)
        cmap, hmtx, hhea = f.getBestCmap(), f["hmtx"].metrics, f["hhea"]
        gs = f.getGlyphSet()

        widths = {hmtx[g][0] for g in f.getGlyphOrder() if g in hmtx}
        if widths != {a.advance}:
            fails.append(f"{name}: advances {sorted(widths)}, expected {{{a.advance}}}")

        out = []
        for cp, gname in cmap.items():
            if bleeds(cp):
                continue
            bp = BoundsPen(gs)
            try:
                gs[gname].draw(bp)
            except Exception:
                continue
            if not bp.bounds:
                continue
            x0, _, x1, _ = bp.bounds
            if x0 < 0 or x1 > a.advance:
                out.append(f"U+{cp:04X}")
        overflow[name] = len(out)
        examples[name] = out[:8]

        sets[name] = frozenset(cmap)
        metrics[name] = (hhea.ascender, hhea.descender, f["head"].unitsPerEm)

    # The two grades promise identical metrics -- that promise is the
    # whole reason you can swap them without reflowing a column.
    if len(set(sets.values())) != 1:
        ref = sorted(sets)[0]
        for n, s in sets.items():
            d = s ^ sets[ref]
            if d:
                fails.append(f"{n}: glyph set differs from {ref} by {len(d)} codepoints")
    if len(set(metrics.values())) != 1:
        fails.append(f"vertical metrics differ across fonts: {metrics}")

    # Ink outside the cell is a RATCHET, not a pass/fail line.  Some
    # of it is unavoidable: every monospace measured -- JetBrains,
    # Liberation, Noto -- puts d-caron past its own advance, because
    # the letter does not fit a fixed cell and the field accepts that.
    # The rest is the italic shear, tracked separately.  So the rule is
    # that it must not get WORSE, and tightening the baseline is how
    # progress is recorded.
    import json
    base = json.loads(BASELINE.read_text()) if BASELINE.exists() else {}
    if a.update_baseline:
        BASELINE.write_text(json.dumps(overflow, indent=2, sort_keys=True) + "\n")
        print(f"  baseline written: {BASELINE}")
    else:
        for n, got in sorted(overflow.items()):
            allowed = base.get(n)
            if allowed is None:
                fails.append(f"{n}: not in the overflow baseline; "
                             f"run check.py --update-baseline and review the diff")
            elif got > allowed:
                fails.append(f"{n}: {got} glyphs with ink outside the cell, "
                             f"baseline allows {allowed} "
                             f"({', '.join(examples[n][:6])})")
            elif got < allowed:
                print(f"  note  {n}: overflow improved, {allowed} -> {got}; "
                      f"tighten the baseline")

    count = len(next(iter(sets.values())))
    for f_ in fails:
        print(f"  FAIL  {f_}")
    if fails:
        sys.exit(f"\n{len(fails)} invariant(s) violated")
    print(f"  OK  {len(paths)} fonts, {count} glyphs each, all at advance {a.advance},")
    print(f"      identical glyph sets and vertical metrics,")
    print(f"      ink outside the cell within baseline "
          f"({min(overflow.values())}-{max(overflow.values())} glyphs per font)")


if __name__ == "__main__":
    main()
