# Changelog

All notable changes to this project are documented here. Release notes on the
GitHub releases page are extracted from this file, so each `## <version>`
section is what users read.

## Unreleased

## 1.0.1 - 2026-10-15

Five punctuation characters redrawn: `(`, `)`, `` ` ``, `@` and `#`. No metric
changes — same glyph set, same advance, same vertical metrics — so nothing
reflows.

Each was measured against seven installed monospaces before and after. Where a
range is given below, it is that field's.

### The characters Lisp leans on

- **Parentheses** are deeper. At 0.33 of the advance wide they were narrower
  than any of the seven (0.37–0.55), and a paren that shallow is close enough
  to a vertical stroke that a run of four reads as a picket fence — which
  matters when counting `))))` by eye is something you do all day. Now 0.47,
  mid-field, and a run reads as scallops you can count.
- **The backtick** is its own shape rather than the grave accent reused. The
  slant was never the problem: flattening it from 43° to 17° moved the
  measurement by nothing. The problem was vertical position — it sat inside the
  apostrophe's band, so blurred to the acuity limit the two were the same mark
  in the same place. It is now longer and set higher, clear of that band, and
  the distance between `'` and `` ` `` goes from 0.123 to 0.381 against a field
  of 0.117–0.369. Letters like `à` keep the old accent, which wants to sit
  close over its letter rather than stand alone.

### `@`

Rebuilt as a real single-storey `a` — a bowl and a right stem — inside a ring
whose right wall *is* that stem. It had been a ring around a floating ellipse:
three contours, nothing tying the inner shape to the outer, and at terminal
sizes a dark bullseye. The ring's left is now one curve from the tail to the
crown with no straight flank, its right is straight below 0.60 of cap, and the
stem stops short above the bowl's foot to leave the channel between them open —
which is what holds the glyph at two contours instead of three.

It had been below the field on both of the measures that matter for an `@`:

| | 1.0.0 | 1.0.1 | field |
|---|---|---|---|
| height / cap | 0.97 | 1.22 | 1.13–1.31 |
| width / advance | 0.78 | 0.86 | 0.81–0.96 |
| ink vs `o` | 1.44 | 1.63 | 1.36–1.97 |
| contours | 3 | 2 | 2 |

`#` against `@` improves from 0.058 — below every reference — to 0.131.

### `#`

Measured as ink per unit of its own bounding box — the number that says
*crowded*, where ink per cell only says *heavy* — it sat at 0.505 against a
field topping out at 0.395. The cause was width, not weight: at 0.61 of the
advance it was the narrowest `#` of the seven, and narrower than our own `o`.

The bars now run past the round letters instead of inside them, the diagonals
sit at a third of the half-width so the three vertical lanes come out equal,
and bars at 0.32 and 0.68 of cap do the same for the three horizontal ones. The
pen comes down to 0.82 to pay for the extra stroke length, so total ink drops
rather than rising.

| | 1.0.0 | 1.0.1 | field |
|---|---|---|---|
| box fill | 0.505 | 0.375 | 0.294–0.395 |
| width / advance | 0.61 | 0.85 | 0.72–0.99 |
| ink vs `o` | 1.41 | 1.37 | 1.04–1.48 |

`X` against `#` was 0.039, below the entire field, and is now 0.050, at its
floor.

## 1.0.0 - 2026-10-15

First release.

### The typeface

A monospace for terminals and code, generated from Common Lisp. Every glyph is
a pure function of the parameters in `src/params.lisp` — there are no drawn
outlines and no font editor in the pipeline.

- **Two grades.** `Cadr Mono` for dark-on-light and `Cadr Mono Dark` for
  light-on-dark, the second with ink removed for that polarity — stem 164
  against 180 — and with identical advances and vertical metrics, so swapping
  between them never reflows a column.
- **Four styles each**, regular, italic, bold and bold italic, for eight static
  fonts. The italic is drawn rather than sloped: six Latin letters are reshaped,
  and in Cyrillic `и`, `п`, `т` and `ц` take their cursive forms while the
  capitals slope.
- **935 glyphs**, every one at advance 1229 on a 2048 em.

### Coverage

39 European languages, tested per language against the characters each needs
rather than by counting Unicode blocks.

- Latin-1 complete, plus Latin Extended for Polish, Czech, Slovak, Hungarian,
  Romanian, Croatian, Slovenian, Serbian, Latvian, Lithuanian, Estonian,
  Turkish, Maltese and Welsh — with the marks each language actually uses:
  Romanian's comma-below rather than a cedilla, Czech's `ď ť ľ` with the caron
  turned on its side, Latvian's `ģ` with the comma above.
- Greek with the monotonic accents, so Greek text can be set and not merely the
  alphabet displayed.
- Cyrillic for Russian, Ukrainian, Belarusian, Bulgarian, Serbian and
  Macedonian.

### Terminal glyphs

- Box drawing, blocks and all 256 braille patterns are computed from the cell
  rather than drawn, which is what makes them meet: rules reach the cell edges,
  junction arms overrun by half the crossing rule's weight, braille dots sit at
  advance/4 so the gap inside a cell equals the gap across cells.
- Box drawing and braille opt out of the italic shear — a sloped rule cannot
  meet its neighbour across a cell boundary.
- Powerline separators `U+E0B0`–`U+E0BF` are generated from the same cell
  metrics, so a shell prompt renders with no third-party artwork and no seam
  where the background colour changes.

### Nerd Font

`make -f build/Makefile nf` builds `Cadr Mono NF`, 11,130 codepoints, merging
the vendored icon sets into the UFO before compilation. No FontForge: the merge
reads the sources with fontTools. Hinting is stripped from the icons
afterwards, which is 97% of the bytecode and buys nothing on a full-cell
pictogram.

Font Logos and Pomicons are deliberately omitted — the first is marked
`unlicensed` upstream, the second ships a licence that contradicts its own
metadata — so a build from here is 145 glyphs short of an official Nerd Font
and is redistributable without asking anyone.

### Hinting

Hinted with `ttfautohint`, which is load-bearing rather than decorative: at 13px
a whole pixel is 158 units and the horizontal stroke is 147. Above 13px the
instructions measurably hurt, and `hintslight` — what most Linux desktops run —
ignores the font's bytecode entirely.
