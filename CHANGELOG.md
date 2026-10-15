# Changelog

All notable changes to this project are documented here. Release notes on the
GitHub releases page are extracted from this file, so each `## <version>`
section is what users read.

## Unreleased

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
