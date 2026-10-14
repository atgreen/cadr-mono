# Muskako

A parametric monospace typeface for terminals, generated from Common Lisp.

Every glyph is a pure function of the parameters in `src/params.lisp`.
There are no drawn outlines and no font editor in the loop: change a
number, regenerate, look at the PNG proofs.

## Design commitments

- **Dark-graded.** Two cuts with identical advance widths and vertical
  metrics: a reference cut for dark-on-light, and a graded cut with ink
  removed for the light-on-dark terminals where this font will live.
  Swapping them never reflows a column.
- **Humanist construction.** Open apertures survive halation; the flat
  sides of geometric bowls pool it.
- **Restrained x-height.** x/cap at 0.714, so ascenders keep enough room
  for word silhouettes to read in prose and diffs.
- **Terminal glyphs generated from cell metrics**, not drawn — braille,
  box drawing and blocks tile exactly because they are computed.

## Build

    sbcl --script build/build.lisp

Writes UFO sources to `build/` and PNG proofs to `proof/`.

## Licence

SIL Open Font License 1.1.
