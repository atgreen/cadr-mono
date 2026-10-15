"""Proof sheets rendered from the COMPILED font.

Proofs go the way the font goes: UFO -> fontmake -> TTF -> FreeType.
Anything wrong with contour types, overlap removal, winding or hinting
shows up here, because this is the machinery a terminal uses.

Compositing note.  Glyphs are accumulated into a COVERAGE map and the
polarity is applied once at the end.  Pasting the grey bitmap through
itself as a mask squares the coverage -- a half-covered edge pixel
lands at 64 instead of 128 -- which made every sheet render thinner and
harsher than the font really is, and made weight comparisons worthless.
"""

# SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: OFL-1.1
#
# SIZE CHOICE.  A "13px" proof is a 96dpi assumption.  On a 2x or 2.5x
# display -- Xft.dpi 192 or 240 -- a 12pt terminal font rasterises at 32
# or 40 real pixels, where this font measures 44-50% of its ink at full
# coverage against 13% at 16px.  Proofing only the small sizes shows a
# softness most readers never see, so the size sheets run up to 40.
import freetype, pathlib
from PIL import Image, ImageDraw, ImageFont

HINTED = pathlib.Path("build/ttf-hinted")
OUT = pathlib.Path("proof")
OUT.mkdir(exist_ok=True)
REF = HINTED / "CadrMono-Regular.ttf"
DARK = HINTED / "CadrMonoDark-Regular.ttf"
ITAL = HINTED / "CadrMono-Italic.ttf"
BOLD = HINTED / "CadrMono-Bold.ttf"
BOLDITAL = HINTED / "CadrMono-BoldItalic.ttf"
DARKITAL = HINTED / "CadrMonoDark-Italic.ttf"

_LABEL_PATHS = ["/usr/share/fonts/dejavu-sans-fonts/DejaVuSans.ttf",
                "/usr/share/fonts/google-noto/NotoSans-Regular.ttf"]


def label_font(size=11):
    for p in _LABEL_PATHS:
        if pathlib.Path(p).exists():
            return ImageFont.truetype(p, size)
    return ImageFont.load_default()


LBL = label_font()
GUT = 64          # left gutter for labels


def face(path, px):
    f = freetype.Face(str(path))
    f.set_pixel_sizes(0, px)
    return f


def advance_of(f):
    f.load_char("M", freetype.FT_LOAD_DEFAULT)
    return f.glyph.advance.x >> 6


def draw_run(cov, f, text, x, baseline, hinted=True):
    """Accumulate TEXT into the coverage map at its true advance."""
    flags = freetype.FT_LOAD_RENDER
    if not hinted:
        flags |= freetype.FT_LOAD_NO_HINTING
    pen = x
    for ch in text:
        f.load_char(ch, flags)
        g = f.glyph
        bm = g.bitmap
        if bm.width and bm.rows:
            mask = Image.frombytes("L", (bm.width, bm.rows), bytes(bm.buffer))
            # solid ink THROUGH the coverage mask, never the bitmap as
            # its own mask
            cov.paste(255, (pen + g.bitmap_left, baseline - g.bitmap_top), mask)
        pen += g.advance.x >> 6
    return pen


def finish(cov, dark):
    """Coverage map to picture.  Dark sheets are ink-on-black, light
    sheets ink-on-white."""
    return cov if dark else Image.eval(cov, lambda v: 255 - v)


def tag(img, xy, text, dark, bright=None):
    d = ImageDraw.Draw(img)
    d.text(xy, text, font=LBL, fill=(bright if bright is not None
                                     else (150 if dark else 110)))


# ---------------------------------------------------------------- sheets

def sheet_letters(px=120, cols=10):
    f = face(REF, px)
    k = px / f.units_per_EM
    chars = [chr(c) for c in range(33, 127)]
    adv = advance_of(f)
    rowh = int((f.bbox.yMax - f.bbox.yMin) * k) + 14
    rows = (len(chars) + cols - 1) // cols
    cov = Image.new("L", (adv * cols + 20, rowh * rows + 20), 0)
    bases = []
    for r in range(rows):
        base = int(10 + r * rowh + f.size.ascender / 64)
        bases.append(base)
        draw_run(cov, f, "".join(chars[r * cols:(r + 1) * cols]), 10, base,
                 hinted=False)
    # Guides go UNDER the ink: draw them on the ground first, then
    # composite the glyphs on top through the coverage mask.  Drawn
    # over, they cut through every letter and fight the thing the sheet
    # exists to show.
    img = Image.new("L", cov.size, 255)
    d = ImageDraw.Draw(img)
    for base in bases:
        for y, grey in ((f.size.ascender / 64, 226), (760 * k, 206),
                        (700 * k, 206), (500 * k, 176), (0, 176),
                        (-230 * k, 206), (f.size.descender / 64, 226)):
            d.line([(0, base - y), (img.width, base - y)], fill=grey)
    img.paste(0, (0, 0), cov)
    tag(img, (6, 2), f"Cadr Mono Regular - outlines at {px}px, unhinted - "
                     "guides: line-asc / asc / cap / x / base / desc / line-desc",
        dark=False)
    img.save(OUT / "letters.png")


TEXT_LINES = [
    "(defun grade (glyph &key (units 4))",
    "  (dolist (c (contours glyph))     ; 4u/side",
    "    (offset c (- units)))          ; 180 -> 164",
    "  (assert (= (width glyph) 1229)))",
    "",
    "Illegible? 0O1lI {}[]()<> =>|& #!$%^~",
    "The quick brown fox jumps over 42 dogs.",
]


def sheet_sizes(dark, sizes=(11, 13, 16, 20, 26, 32, 40)):
    """Light sheet shows the Regular grade; the dark sheet shows the DARK
    CUT, which is the one you would actually set on a dark ground."""
    path = DARK if dark else REF
    name = "Cadr Mono Dark" if dark else "Cadr Mono Regular"
    faces = [(s, face(path, s)) for s in sizes]
    h = sum(int(fc.size.height / 64) * len(TEXT_LINES) + 16 for _, fc in faces) + 20
    w = max(advance_of(fc) * max(len(l) for l in TEXT_LINES) for _, fc in faces) + GUT + 20
    cov = Image.new("L", (w, h), 0)
    marks, y = [], 10
    for s, fc in faces:
        lh = int(fc.size.height / 64)
        marks.append((y + 2, f"{s}px"))
        for ln in TEXT_LINES:
            y += lh
            draw_run(cov, fc, ln, GUT, y - int(abs(fc.size.descender / 64)))
        y += 16
    img = finish(cov, dark)
    for yy, txt in marks:
        tag(img, (8, yy), txt, dark)
    tag(img, (8, h - 16), name, dark)
    img.save(OUT / ("sizes-dark.png" if dark else "sizes-light.png"))


def sheet_grade(sizes=(11, 13, 16, 24, 48)):
    """The two grades at EQUAL sizes, both on black, which is the only
    comparison that says anything about the grade."""
    text = "once coil 0O1lI mnu"
    h = sum(int(face(REF, s).size.height / 64) + 14 for s in sizes) + 34
    w = 940
    cov = Image.new("L", (w, h), 0)
    half = w // 2
    marks, y = [], 22
    for s in sizes:
        fr, fd = face(REF, s), face(DARK, s)
        lh = int(fr.size.height / 64)
        y += lh
        base = y - int(abs(fr.size.descender / 64))
        fits = max(1, (half - GUT - 16) // advance_of(fr))
        draw_run(cov, fr, text[:fits], GUT, base)
        draw_run(cov, fd, text[:fits], half + 12, base)
        marks.append((y - lh + 2, f"{s}px"))
        y += 14
    img = finish(cov, dark=True)
    d = ImageDraw.Draw(img)
    d.line([(half, 0), (half, h)], fill=70)
    for yy, txt in marks:
        tag(img, (8, yy), txt, True)
    tag(img, (GUT, 4), "Regular (stem 180)", True, bright=200)
    tag(img, (half + 12, 4), "Dark grade (stem 164)", True, bright=200)
    img.save(OUT / "grade.png")


BOX_ART = [
    "┌─────┬─────┐  ╔═════╦═════╗  ╭─────╮",
    "│ foo │ bar │  ║ foo ║ bar ║  │ box │",
    "├─────┼─────┤  ╠═════╬═════╣  ╰─────╯",
    "│ baz │ qux │  ║ baz ║ qux ║   ╱╲╳  ",
    "└─────┴─────┘  ╚═════╩═════╝  ┏━┳━┓",
    "┠─┰─┨ ┞─┟ ╞═╡ ╟─╢ ┌┬┐ ╒╤╕ ╓╥╖ ┗━┻━┛",
    "▁▂▃▄▅▆▇█ ░▒▓ ▌▐▀▄ ▖▗▘▙▚▛▜▝▞▟ ┄┈╌ ┆┊╎",
    "⣀⣤⣶⣿⠿⠛⠉⠙⠹⠸⠼⠴⠦⠧⠇⠏ ⡀⡄⡆⡇⣇⣧⣷⣿ sparkline",
]


def sheet_boxes(sizes=(11, 13, 17, 26)):
    """Seams only show when cells sit at their true advance with no gap,
    and only at the sizes people actually use."""
    h = sum(int(face(REF, s).size.height / 64) * len(BOX_ART) + 18 for s in sizes) + 20
    w = max(advance_of(face(REF, s)) * max(len(l) for l in BOX_ART)
            for s in sizes) + GUT + 20
    cov = Image.new("L", (w, h), 0)
    marks, y = [], 10
    for s in sizes:
        fc = face(REF, s)
        lh = int(fc.size.height / 64)
        marks.append((y + 2, f"{s}px"))
        for ln in BOX_ART:
            y += lh
            draw_run(cov, fc, ln, GUT, y - int(abs(fc.size.descender / 64)))
        y += 18
    img = finish(cov, dark=True)
    for yy, txt in marks:
        tag(img, (8, yy), txt, True)
    img.save(OUT / "boxes.png")


TERMINAL = [
    "$ rg --json 'defun (grade|stroke)' src/ | jq -r '.data.path.text'",
    "src/pen.lisp:  2026-10-14T13:07:41.882Z  INFO   127.0.0.1:8443 -> 10.0.2.15",
    "  [warn] ~/.local/share/fonts/CadrMono-Regular.ttf: no 'DSIG' table",
    "  commit 8ad8b47  (HEAD -> main)  fix: bowls cancelling against stems",
    "  ├── build/   47.3 KiB   ├── src/     12 files   └── proof/  8 sheets",
    "  cpu ⣀⣄⣆⣇⣧⣷⣿⣿⣷⣧⣇⣆⣄⣀  mem ▁▂▃▅▆▇█▇▆▅▃▂▁  io ░▒▓█▓▒░",
    "  PASS  512/512 glyphs   advances {1229}   top gap 11.8%   0 FAIL",
    "  a1b2c3d4-5e6f-7890-abcd-ef0123456789  SHA256:Ql+9x/Z0==  $?=0  ~/§",
]


def sheet_terminal(sizes=(13, 16, 20, 26, 32, 40)):
    """What the font is actually for: dense output, paths, addresses,
    hashes, tree glyphs and sparklines, at working sizes."""
    faces = [(s, face(DARK, s)) for s in sizes]
    h = sum(int(fc.size.height / 64) * len(TERMINAL) + 16 for _, fc in faces) + 20
    w = max(advance_of(fc) * max(len(l) for l in TERMINAL) for _, fc in faces) + GUT + 20
    cov = Image.new("L", (w, h), 0)
    marks, y = [], 10
    for s, fc in faces:
        lh = int(fc.size.height / 64)
        marks.append((y + 2, f"{s}px"))
        for ln in TERMINAL:
            y += lh
            draw_run(cov, fc, ln, GUT, y - int(abs(fc.size.descender / 64)))
        y += 16
    img = finish(cov, dark=True)
    for yy, txt in marks:
        tag(img, (8, yy), txt, True)
    tag(img, (8, h - 16), "Cadr Mono Dark", True)
    img.save(OUT / "terminal.png")


EXTENDED = [
    ("Latin-1 lower", "àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"),
    ("Latin-1 upper", "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞß"),
    ("ligatures",     "œŒ æÆ ıîïÎÏ  coeur -> cœur, oeuvre -> œuvre"),
    ("French",        "Où êtes-vous allé ? Ça coûte 9 € à Noël."),
    ("Greek lower",   "αβγδεζηθικλμνξοπρςστυφχψω"),
    ("Greek upper",   "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"),
    ("currency",      "¢ £ ¤ ¥ € ƒ $ ‰ ª º § ¶ © ®"),
    ("operators",     "+ − ± × ÷ = ≠ ≡ ≤ ≥ ≈ ¬ · ° µ"),
    ("maths",         "∞ √ ∑ ∏ ∫ ∂ ∆ ∅ ∀ ∃ ∈ ∧ ∨ ∪ ∩"),
    ("arrows",        "→ ← ↑ ↓ ↔ ⇒"),
    ("punctuation",   "« » ‹ › ¡ ¿ – — ' ' \u201c \u201d … • † ‡"),
]


def sheet_extended(sizes=(14, 20, 38)):
    """The non-ASCII repertoire, at the sizes it gets judged at.

    Permanent, because these blocks were reviewed once from a throwaway
    render and the defects it found -- disconnected Greek, a dark oe, an
    undersized infinity -- are exactly what a standing sheet catches."""
    rows = []
    for px in sizes:
        for name, txt in EXTENDED:
            f = face(REF, px)
            adv = advance_of(f)
            cov = Image.new("L", (adv * len(txt) + 16, int(f.size.height / 64) + 8), 0)
            draw_run(cov, f, txt, 8, int(4 + f.size.ascender / 64))
            rows.append((f"{name} {px}px", finish(cov, dark=True)))
    gut = 160
    W = max(r.width for _, r in rows) + gut + 16
    H = sum(r.height + 7 for _, r in rows) + 10
    sheet = Image.new("L", (W, H), 0)
    marks, y = [], 6
    for lab, r in rows:
        sheet.paste(r, (gut, y))
        marks.append((6, y + r.height // 2 - 6, lab))
        y += r.height + 7
    d = ImageDraw.Draw(sheet)
    for x, yy, lab in marks:
        d.text((x, yy), lab, font=LBL, fill=165)
    sheet.save(OUT / "extended.png")


def sheet_italic(sizes=(13, 16, 22, 44)):
    """Roman against italic, alternating, so the reshaped letters and
    the unchanged box drawing are both obvious."""
    lines = [
        "afykvw agile defy walk — the quick brown fox",
        "(defmacro with-design ((d) &body body) `(let ...))",
        "┌──┬──┐ │ab│cd│ ├──┼──┤ └──┴──┘  ░▒▓█",
    ]
    rows = []
    for px in sizes:
        for lab, path in (("roman", REF), ("italic", ITAL)):
            f = face(path, px)
            adv = advance_of(f)
            lh = int(f.size.height / 64)
            cov = Image.new("L", (adv * max(len(l) for l in lines) + 16,
                                  lh * len(lines) + 8), 0)
            y = 4
            for ln in lines:
                y += lh
                draw_run(cov, f, ln, 8, y - int(abs(f.size.descender / 64)))
            rows.append((f"{lab} {px}px", finish(cov, dark=True)))
    gut = 110
    W = max(r.width for _, r in rows) + gut + 16
    H = sum(r.height + 6 for _, r in rows) + 10
    sheet = Image.new("L", (W, H), 0)
    marks, y = [], 6
    for lab, r in rows:
        sheet.paste(r, (gut, y))
        marks.append((6, y + r.height // 2 - 6, lab))
        y += r.height + 6
    d = ImageDraw.Draw(sheet)
    for x, yy, lab in marks:
        d.text((x, yy), lab, font=LBL, fill=165)
    sheet.save(OUT / "italic.png")


def sheet_weights(sizes=(13, 16, 22, 40)):
    """All four styles of the light grade, at the sizes they get set at.

    The question a bold has to answer in a terminal is whether it reads
    as bold at 13px without closing its counters."""
    text = "aeomnsg 0O8 ┌─┬─┐ ░▒▓ the quick brown fox 42"
    styles = [("Regular", REF), ("Bold", BOLD),
            ("Italic", ITAL), ("Bold Italic", BOLDITAL)]
    rows = []
    for px in sizes:
        for lab, path in styles:
            f = face(path, px)
            adv = advance_of(f)
            cov = Image.new("L", (adv * len(text) + 16,
                                  int(f.size.height / 64) + 8), 0)
            draw_run(cov, f, text, 8, int(4 + f.size.ascender / 64))
            rows.append((f"{lab} {px}px", finish(cov, dark=True)))
    gut = 130
    W = max(r.width for _, r in rows) + gut + 16
    H = sum(r.height + 5 for _, r in rows) + 10
    sheet = Image.new("L", (W, H), 0)
    marks, y = [], 6
    for lab, r in rows:
        sheet.paste(r, (gut, y))
        marks.append((6, y + r.height // 2 - 6, lab))
        y += r.height + 5
    d = ImageDraw.Draw(sheet)
    for x, yy, lab in marks:
        d.text((x, yy), lab, font=LBL, fill=165)
    sheet.save(OUT / "weights.png")


def sheet_range(name, first, last, px, cols, title):
    fc = face(REF, px)
    adv = advance_of(fc)
    lh = int(fc.size.height / 64)
    n = last - first + 1
    rows = (n + cols - 1) // cols
    cov = Image.new("L", (adv * cols + 20, lh * rows + 34), 0)
    for i in range(n):
        r, c = divmod(i, cols)
        base = 24 + r * lh + int(fc.size.ascender / 64)
        draw_run(cov, fc, chr(first + i), 10 + c * adv, base)
    img = finish(cov, dark=True)
    tag(img, (8, 4), f"{title} at {px}px", True)
    img.save(OUT / name)


if __name__ == "__main__":
    sheet_letters()
    sheet_sizes(False)
    sheet_sizes(True)
    sheet_grade()
    sheet_boxes()
    sheet_terminal()
    sheet_extended()
    sheet_italic()
    sheet_weights()
    sheet_range("box-grid.png", 0x2500, 0x257F, 52, 16, "U+2500 box drawing")
    sheet_range("braille.png", 0x2800, 0x28FF, 56, 32, "U+2800 braille")
    print("proof sheets rendered from the compiled font")
