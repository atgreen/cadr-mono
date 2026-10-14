"""Proof sheets rendered from the COMPILED font.

The previous renderer drew from the generator's own contour lists, so
it shared every assumption the generator made.  It drew two serious
bugs as if they were fine -- a reversed contour whose point types were
wrong, and rings whose winding cancelled against their stems -- because
it built paths by counting off-curve points rather than reading the
declared types, and because nothing it did exercised the fill rule the
way a real rasteriser does.

These sheets go through the path the font actually takes: UFO ->
fontmake -> TTF -> FreeType.  Anything wrong with contour types,
overlap removal, winding or hinting shows up here, because this is the
same machinery a terminal uses.
"""
import freetype, pathlib, sys
from PIL import Image, ImageDraw

HINTED = pathlib.Path("build/ttf-hinted")
PLAIN = pathlib.Path("build/ttf")
OUT = pathlib.Path("proof")
OUT.mkdir(exist_ok=True)

REF = HINTED / "Muskako-Regular.ttf"
DARK = HINTED / "MuskakoDark-Regular.ttf"


def face(path, px):
    f = freetype.Face(str(path))
    f.set_pixel_sizes(0, px)
    return f


def draw_run(img, f, text, x, baseline, hinted=True, invert=False):
    """Draw TEXT at its true advance.  Returns the pen x after the run."""
    flags = freetype.FT_LOAD_RENDER
    if not hinted:
        flags |= freetype.FT_LOAD_NO_HINTING
    pen = x
    for ch in text:
        f.load_char(ch, flags)
        g = f.glyph
        bm = g.bitmap
        if bm.width and bm.rows:
            patch = Image.frombytes("L", (bm.width, bm.rows), bytes(bm.buffer))
            if invert:
                patch = Image.eval(patch, lambda v: v)
            img.paste(patch, (pen + g.bitmap_left, baseline - g.bitmap_top), patch)
        pen += g.advance.x >> 6
    return pen


def advance_of(f):
    f.load_char("M", freetype.FT_LOAD_DEFAULT)
    return f.glyph.advance.x >> 6


# ---------------------------------------------------------------- sheets

def sheet_letters(px=120, cols=10):
    """Every text glyph at display size, against the metric guides."""
    f = face(REF, px)
    upm = f.units_per_EM
    k = px / upm
    chars = [chr(c) for c in range(33, 127)]
    adv = advance_of(f)
    rowh = int((f.bbox.yMax - f.bbox.yMin) * k) + 14
    rows = (len(chars) + cols - 1) // cols
    img = Image.new("L", (adv * cols + 20, rowh * rows + 20), 0)
    asc = f.size.ascender / 64
    bases = []
    for r in range(rows):
        base = int(10 + r * rowh + asc)
        bases.append(base)
        draw_run(img, f, "".join(chars[r * cols:(r + 1) * cols]), 10, base, hinted=False)
    # Invert first, then lay the guides on, or they inverted too and
    # came out as heavy black rules across every row.
    img = Image.eval(img, lambda v: 255 - v)
    d = ImageDraw.Draw(img)
    for base in bases:
        for y, grey in ((f.size.ascender / 64, 212), (760 * k, 196),
                        (700 * k, 196), (500 * k, 150), (0, 150),
                        (-230 * k, 196), (f.size.descender / 64, 212)):
            d.line([(0, base - y), (img.width, base - y)], fill=grey)
    img.save(OUT / "letters.png")


def sheet_sizes(dark_bg, sizes=(11, 12, 13, 14, 16, 20, 28)):
    lines = [
        "(defun grade (glyph &key (units 4))",
        "  (dolist (c (contours glyph))     ; 4u/side",
        "    (offset c (- units)))          ; 180 -> 164",
        "  (assert (= (width glyph) 1229)))",
        "",
        "Illegible? 0O1lI {}[]()<> =>|& #!$%^~",
        "The quick brown fox jumps over 42 dogs.",
    ]
    faces = [(s, face(REF, s)) for s in sizes]
    h = sum(int((fc.size.height / 64) * len(lines)) + 12 for _, fc in faces) + 20
    w = max(advance_of(fc) * max(len(l) for l in lines) for _, fc in faces) + 30
    img = Image.new("L", (w, h), 0)
    y = 10
    for s, fc in faces:
        lh = int(fc.size.height / 64)
        for ln in lines:
            y += lh
            draw_run(img, fc, ln, 12, y - int(abs(fc.size.descender / 64)))
        y += 12
    if not dark_bg:
        img = Image.eval(img, lambda v: 255 - v)
    img.save(OUT / ("sizes-dark.png" if dark_bg else "sizes-light.png"))


def sheet_grade(sizes=(13, 16, 24, 48)):
    """Reference cut beside the graded cut, both on black."""
    text = "once coil 01-."
    h = sum(int(face(REF, s).size.height / 64) + 10 for s in sizes) + 20
    w = 900
    img = Image.new("L", (w, h), 0)
    d = ImageDraw.Draw(img)
    half = w // 2
    d.line([(half, 0), (half, h)], fill=60)
    y = 10
    for s in sizes:
        fr, fd = face(REF, s), face(DARK, s)
        lh = int(fr.size.height / 64)
        y += lh
        base = y - int(abs(fr.size.descender / 64))
        fits = max(1, (half - 24) // advance_of(fr))
        draw_run(img, fr, text[:fits], 12, base)
        draw_run(img, fd, text[:fits], half + 12, base)
        y += 10
    img.save(OUT / "grade.png")


def sheet_boxes(sizes=(13, 17, 26)):
    art = [
        "┌─────┬─────┐  ╔═════╦═════╗  ╭─────╮",
        "│ foo │ bar │  ║ foo ║ bar ║  │ box │",
        "├─────┼─────┤  ╠═════╬═════╣  ╰─────╯",
        "│ baz │ qux │  ║ baz ║ qux ║   ╱╲╳  ",
        "└─────┴─────┘  ╚═════╩═════╝  ┏━┳━┓",
        "┠─┰─┨ ┞─┟ ╞═╡ ╟─╢ ┌┬┐ ╒╤╕ ╓╥╖ ┗━┻━┛",
        "▁▂▃▄▅▆▇█ ░▒▓ ▌▐▀▄ ▖▗▘▙▚▛▜▝▞▟ ┄┈╌ ┆┊╎",
    ]
    h = sum(int(face(REF, s).size.height / 64) * len(art) + 14 for s in sizes) + 20
    w = max(advance_of(face(REF, s)) * max(len(l) for l in art) for s in sizes) + 24
    img = Image.new("L", (w, h), 0)
    y = 10
    for s in sizes:
        fc = face(REF, s)
        lh = int(fc.size.height / 64)
        for ln in art:
            y += lh
            # Rules must meet across the cell boundary, which only shows
            # when cells sit at their true advance with no gap.
            draw_run(img, fc, ln, 12, y - int(abs(fc.size.descender / 64)))
        y += 14
    img.save(OUT / "boxes.png")


def sheet_range(name, first, last, px=44, cols=16):
    fc = face(REF, px)
    adv = advance_of(fc)
    lh = int(fc.size.height / 64)
    n = last - first + 1
    rows = (n + cols - 1) // cols
    img = Image.new("L", (adv * cols + 20, lh * rows + 20), 0)
    for i in range(n):
        r, c = divmod(i, cols)
        base = 10 + r * lh + int(fc.size.ascender / 64)
        draw_run(img, fc, chr(first + i), 10 + c * adv, base)
    img.save(OUT / name)


if __name__ == "__main__":
    sheet_letters()
    sheet_sizes(True)
    sheet_sizes(False)
    sheet_grade()
    sheet_boxes()
    sheet_range("box-grid.png", 0x2500, 0x257F)
    sheet_range("braille.png", 0x2800, 0x28FF, px=36, cols=32)
    print("proof sheets rendered from the compiled font")
