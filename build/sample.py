"""A sample sheet: what this font looks like doing its job.

Distinct from every other sheet in proof/.  Those are instruments --
size ladders, grids, measured comparisons -- and they answer "is this
correct?".  This one answers "do I want to read in it?", which is a
different question and the first one anybody actually asks.

So: real terminal output, in colour, at one size, with nothing
repeated.  The content is chosen to be things a terminal genuinely
puts on screen AND things this font computes from the cell rather than
draws -- git graphs, box-drawing trees, braille sparklines, block
meters -- because those are the glyphs no sample of prose would show.

Rendered at 30px, which is a 12pt terminal on a 2.5x display: the
README is read on laptops, and a 13px render there looks like a
softness most readers never see.  See the SIZE CHOICE note in
proofsheet.py.

Colour is applied per glyph as solid ink through the glyph's own
coverage mask -- never by pasting a grey bitmap through itself, which
squares the coverage and renders everything thin.  Same rule as
proofsheet.py, for the same reason.
"""

# SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: OFL-1.1
import freetype, pathlib
from PIL import Image, ImageChops, ImageDraw, ImageFont

# build/ttf-hinted, NOT fonts/.  Both hold the same bytes once a build
# finishes, but fonts/ is a COPY made by a later Makefile target than
# the one that renders proofs -- so reading it here made this sheet
# depend on the previous build's output.  It went unnoticed while a
# stale fonts/ happened to be lying around, and surfaced the moment
# the font was renamed and that directory was empty.  Every other
# proof reads the hinted build directly; so does this one now.
FONTS = pathlib.Path("build/ttf-hinted")
OUT = pathlib.Path("proof")
OUT.mkdir(exist_ok=True)

PX = 40
PAD = 34
GAP = 26          # between panels

# NO invented leading.  The row pitch is the font's own line box and
# the baseline is its own ascender, because that is what a terminal
# does -- and it is the whole reason box drawing works there.  A sheet
# set on a 1.42 line multiple, as this one was, spaces 30px rows 42px
# apart over a 36.3px cell and opens a 5.7px gap under every row; the
# vertical rules then cannot reach the horizontals, and the panel
# captioned "box drawing meets across the cell boundary" showed box
# drawing failing to meet.  The glyphs were never at fault: U+2502
# spans the full line box, -594 to 1884, and U+2500 the full advance.

# A conventional dark-terminal palette.  The font shipped here is
# CADR Mono DARK -- the grade with ink removed for this polarity -- which
# is the whole argument of the project, so the hero image had better
# be set in it.
C = {
    "bg":   (18, 21, 28),
    "fg":   (200, 204, 212),
    "dim":  (108, 117, 131),
    "red":  (224, 108, 117),
    "grn":  (152, 195, 121),
    "yel":  (229, 192, 123),
    "blu":  (97, 175, 239),
    "mag":  (198, 120, 221),
    "cyn":  (86, 182, 194),
    "wht":  (230, 233, 239),
    # grounds for the powerline prompt: a separator is only
    # meaningful against the colour change it hides.
    "g1":   (40, 52, 87),
    "g2":   (54, 45, 66),
    "g3":   (40, 42, 62),
}

FACES = {}
def face(style):
    if style not in FACES:
        f = freetype.Face(str(FONTS / f"CadrMonoDark-{style}.ttf"))
        f.set_pixel_sizes(0, PX)
        FACES[style] = f
    return FACES[style]


def advance():
    f = face("Regular")
    f.load_char("M", freetype.FT_LOAD_DEFAULT)
    return f.glyph.advance.x >> 6


ADV = advance()


def line_box():
    """Row pitch and baseline, in pixels, from the font's own metrics."""
    f = face("Regular")
    upm = f.units_per_EM
    asc, desc = f.ascender, f.descender          # font units
    return (asc - desc) * PX / upm, asc * PX / upm


LINE_F, BASE_F = line_box()
# ROUND, not truncate -- and 40px is chosen, not arbitrary.  Tiling
# has to work on BOTH axes at once and the two want different things:
#
#   advance 0.600 em -> integral only at multiples of 5px
#   line box 1.210 em -> integral essentially never
#
# A fractional advance leaves every block's last column part-covered
# and solid fill shows vertical seams; a cell whose height the rule
# cannot fill leaves a pale scanline at every row join.  At 30px the
# advance is exactly 18.00px but the rule's top row rasterises at 50%
# (measured 110 -> 65 at each seam).  At 33px the rows are perfect and
# the advance is 19.80px in a 20px cell, which is the seam visible in
# the block meters.  40px gets both: advance exactly 24.00px, and the
# rule hints to 49 rows over a 48px pitch, so cells overlap by one row
# instead of leaving one short.
#
# The good sizes are not evenly spaced.  Re-measure if PX moves.
LINE = round(LINE_F)
BASE = round(BASE_F)

_LABEL_PATHS = ["/usr/share/fonts/dejavu-sans-fonts/DejaVuSans-Bold.ttf",
                "/usr/share/fonts/google-noto/NotoSans-Bold.ttf"]
def label_font(size=15):
    for p in _LABEL_PATHS:
        if pathlib.Path(p).exists():
            return ImageFont.truetype(p, size)
    return ImageFont.load_default()
LBL = label_font()


def draw_spans(cov, grounds, spans, x, baseline):
    """Accumulate SPANS into per-colour COVERAGE maps.

    Coverage is accumulated and the colour applied once at the end,
    never pasted glyph by glyph: pasting through each glyph's own mask
    REPLACES what is under it, which broke every row seam of the box
    drawing.

    Accumulated by ADDITION, not by max.  Max is the right rule for
    strokes that overlap -- it stops a crossing going double-dark --
    but these do not overlap, they ABUT: a vertical rule is 36.30px of
    ink on a 36px pitch, so the seam row takes part of its coverage
    from the cell above and part from the cell below.  Two halves of
    one pixel are a whole pixel; max of them is a half, and the rule
    came out 40% pale at every join (110 -> 65, measured) in a panel
    captioned "box drawing meets across the cell boundary".

    Adjacent glyphs in a monospace do not otherwise touch, so there is
    nothing else here for addition to over-darken.
    """
    pen = x
    for span in spans:
        text, ck, style = span[0], span[1], span[2]
        bg = span[3] if len(span) > 3 else None
        f = face(style)
        layer = cov[ck]
        if bg is not None:
            # full CELL height, not the glyph box: a prompt
            # segment is a painted row of cells, and a gap at the
            # top or bottom would read as a seam.
            top = baseline - BASE
            grounds.append((pen, top, pen + ADV * len(text), top + LINE, C[bg]))
        for ch in text:
            f.load_char(ch, freetype.FT_LOAD_RENDER)
            g = f.glyph
            bm = g.bitmap
            if bm.width and bm.rows:
                mask = Image.frombytes("L", (bm.width, bm.rows), bytes(bm.buffer))
                bx, by = pen + g.bitmap_left, baseline - g.bitmap_top
                box = (bx, by, bx + bm.width, by + bm.rows)
                layer.paste(ImageChops.add(layer.crop(box), mask), (bx, by))
            pen += ADV
    return pen


def t(text, ck="fg", style="Regular", bg=None):
    return (text, ck, style, bg)


# --- the content -------------------------------------------------------
#
# Real output, not a pangram and not a glyph inventory.  Each panel is
# here because a terminal actually produces it and because it leans on
# something this font does deliberately.
#
# A line is a list of (column, spans) groups placed at ABSOLUTE cell
# columns.  Padding with spaces instead would mean counting characters
# by hand across styles, and the first time a label changed length the
# box drawing would stop meeting -- which is precisely the failure this
# font exists to avoid, so the sample had better not commit it.

def col(n, *spans):
    return (n, list(spans))


PANELS = [
    ("A session", [
        [col(0, t("~/git/cadr-mono", "blu"), t(" $ ", "dim"),
                t("git log --graph --oneline -5"))],
        [col(0, t("* ", "yel"), t("746661e ", "yel"), t("(", "dim"),
                t("HEAD -> main", "cyn", "Bold"), t(") ", "dim"),
                t("Proof at HiDPI sizes, and qualify the hinting claim"))],
        [col(0, t("* ", "yel"), t("3ed0412 ", "yel"),
                t("Cut the README roughly in half"))],
        [col(0, t("* ", "yel"), t("ccfd9e0 ", "yel"),
                t("Raise the x-height to 530 per mille"))],
        [col(0, t("* ", "yel"), t("e79eada ", "yel"),
                t("Calm the letterforms: less stress tilt, less modulation"))],
    ]),

    ("A diff", [
        [col(0, t("~/git/cadr-mono", "blu"), t(" $ ", "dim"),
                t("git diff src/glyphs.lisp"))],
        [col(0, t("@@ -635,7 +635,9 @@ ", "cyn"), t("(defglyph \"J\" 74", "dim"))],
        [col(0, t("-         (r (* (p :tail) 1.5))", "red"))],
        [col(0, t("+         (rx (+ (p :serif) (* (bowl-rx) 0.15)))", "grn"))],
        [col(0, t("+         (ry (* (p :cap) 0.32))", "grn"))],
        [col(0, t(" 1 file changed, ", "dim"), t("16 insertions(+)", "grn"),
                t(", ", "dim"), t("3 deletions(-)", "red"))],
    ]),

    ("Box drawing meets across the cell boundary", [
        [col(0, t("cadr-mono", "blu", "Bold")),
         col(30, t("┌──────────────┬───────┬──────┐", "dim"))],
        [col(0, t("├── ", "dim"), t("src", "blu")),
         col(30, t("│", "dim"), t(" glyph set    "), t("│", "dim"),
                 t("   729 "), t("│", "dim"), t(" ", "fg"), t("PASS", "grn"),
                 t(" ", "fg"), t("│", "dim"))],
        [col(0, t("│   ├── ", "dim"), t("glyphs.lisp")),
         col(30, t("│", "dim"), t(" advance      "), t("│", "dim"),
                 t("  1229 "), t("│", "dim"), t(" ", "fg"), t("PASS", "grn"),
                 t(" ", "fg"), t("│", "dim"))],
        [col(0, t("│   └── ", "dim"), t("params.lisp")),
         col(30, t("│", "dim"), t(" fonts        "), t("│", "dim"),
                 t("     8 "), t("│", "dim"), t(" ", "fg"), t("PASS", "grn"),
                 t(" ", "fg"), t("│", "dim"))],
        [col(0, t("└── ", "dim"), t("proof", "blu")),
         col(30, t("│", "dim"), t(" ink in cell  "), t("│", "dim"),
                 t("    41 "), t("│", "dim"), t(" ", "fg"), t("WARN", "yel"),
                 t(" ", "fg"), t("│", "dim"))],
        [col(0, t("    └── ", "dim"), t("sample.png")),
         col(30, t("└──────────────┴───────┴──────┘", "dim"))],
    ]),

    ("Braille and blocks, computed from the cell", [
        [col(0, t("cpu ", "dim")), col(4, t("⣀⣠⣤⣴⣶⣾⣿⣷⣶⣤⣄⣀⣠⣴⣾⣿", "wht")),
         col(22, t(" 47%")),
         col(30, t("net ", "dim")), col(34, t("⢀⣀⣠⣴⣶⣿⣷⣦⣄⡀⢀⣠⣴⣶⣿⣷", "cyn")),
         col(52, t(" 2.1M/s"))],
        [col(0, t("mem ", "dim")), col(4, t("████████████", "yel"), t("▓▒░░", "dim")),
         col(22, t(" 61%")),
         col(30, t("io  ", "dim")), col(34, t("▁▂▄█▆▃▁▂▅▇█▄▂▁▃▆", "mag")),
         col(52, t(" 118 iops")),
         ],
        [col(0, t("dsk ", "dim")), col(4, t("█████", "red"), t("▓▒░░░░░░░░░", "dim")),
         col(22, t(" 31%")),
         col(30, t("tmp ", "dim")), col(34, t("░░░░░░░░░░░░░░░░", "dim")),
         col(52, t("   0 B")),
         ],
    ]),

    ("Powerline separators, computed from the cell like the box drawing", [
        [col(0, t("\ue0b6", "g1"), t("~/git/cadr-mono ", "blu", "Regular", "g1"),
                t("\ue0b0", "g1", "Regular", "g2"),
                t(" main ", "grn", "Regular", "g2"),
                t("\ue0b0", "g2", "Regular", "g3"),
                t(" 0.4s ", "mag", "Regular", "g3"),
                t("\ue0b0", "g3"))],
        [col(0, t("\ue0b6", "g1"), t("~/git/cadr-mono ", "blu", "Regular", "g1"),
                t("\ue0b4", "g1", "Regular", "g2"),
                t(" main ", "grn", "Regular", "g2"),
                t("\ue0b4", "g2", "Regular", "g3"),
                t(" 0.4s ", "mag", "Regular", "g3"),
                t("\ue0b4", "g3"))],
        [col(0, t("\ue0b0\ue0b1\ue0b2\ue0b3", "dim"), t(" "),
                t("\ue0b4\ue0b5\ue0b6\ue0b7", "dim"), t(" "),
                t("\ue0b8\ue0b9\ue0ba\ue0bb", "dim"), t(" "),
                t("\ue0bc\ue0bd\ue0be\ue0bf", "dim"),
                t("   all 16, straight in the italic", "dim"))],
    ]),

    ("The shapes that have to stay apart", [
        [col(0, t("0O", "wht")), col(4, t("zero and cap O", "dim")),
         col(22, t("1lI", "wht")), col(27, t("one, ell, cap I", "dim")),
         col(46, t("rn m", "wht")), col(52, t("rn against m", "dim"))],
        [col(0, t(";:,.", "wht")), col(6, t("stops", "dim")),
         col(22, t("{}[]()<>", "wht")), col(32, t("brackets", "dim")),
         col(46, t("~-_=+", "wht")), col(52, t("operators", "dim"))],
        [col(0, t("Regular", "fg")), col(9, t("Bold", "fg", "Bold")),
         col(15, t("Italic", "fg", "Italic")),
         col(23, t("Bold Italic", "fg", "BoldItalic")),
         col(46, t("value", "wht", "Italic")),
         col(52, t("the italic redraws v, w, y", "dim"))],
    ]),
]


def main():
    cols = 0
    for _, body in PANELS:
        for line in body:
            for c, spans in line:
                cols = max(cols, c + sum(len(s[0]) for s in spans))
    W = PAD * 2 + cols * ADV
    H = PAD * 2 + sum(24 + len(body) * LINE + GAP for _, body in PANELS) - GAP

    img = Image.new("RGB", (W, H), C["bg"])
    cov = {k: Image.new("L", (W, H), 0) for k in C if k != "bg"}
    grounds = []
    d = ImageDraw.Draw(img)
    y = PAD
    for title, body in PANELS:
        d.text((PAD, y), title.upper(), font=LBL, fill=C["dim"])
        y += 24
        top = y
        for i, line in enumerate(body):
            # One INTEGER cell height for every row, which is what a
            # terminal does.  Accumulating on the unrounded 36.30px
            # pitch instead makes rows alternate 36 and 37 px, and the
            # rules then meet at a different sub-pixel phase on each
            # row -- continuous, but visibly uneven.  Rounding down
            # also guarantees the meeting: the glyph's rule is 36.30px
            # tall in a 36px cell, so consecutive cells overlap rather
            # than risk a hairline.
            base = top + i * LINE + BASE
            for c, spans in line:
                draw_spans(cov, grounds, spans, PAD + c * ADV, base)
            y = top + (i + 1) * LINE
        y += GAP

    # backgrounds first, then ink over them
    for x0, y0, x1, y1, colour in grounds:
        d.rectangle([x0, y0, x1 - 1, y1 - 1], fill=colour)

    for key, layer in cov.items():
        img.paste(Image.new("RGB", (W, H), C[key]), (0, 0), layer)

    out = OUT / "sample.png"
    img.save(out)
    print(f"wrote {out}  {img.size[0]}x{img.size[1]}  "
          f"({PX}px, advance {ADV}, CADR Mono Dark, {cols} columns)")


if __name__ == "__main__":
    main()
