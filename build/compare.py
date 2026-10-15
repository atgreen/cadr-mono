"""Side-by-side comparison against the monospace fonts on this machine.

Two sheets, because they answer different questions.

compare-size.png sets every font at the SAME PIXEL SIZE.  That is the
practical comparison: it is what happens when you type 13 into a
terminal's font setting, and differences in apparent size are real
consequences of the choice.

compare-xheight.png matches X-HEIGHTS instead, scaling each font until
its lowercase is the same height.  That is the typographic comparison:
it separates "this font is drawn better" from "this font is simply set
larger", which the first sheet cannot tell you.
"""

# SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: OFL-1.1
import freetype, pathlib, sys
sys.path.insert(0, "build")
from proofsheet import finish, label_font, REF, DARK
from PIL import Image, ImageDraw

OUT = pathlib.Path("proof")
SPEC = "0O1lI rn/m {}[]() => 42 quick brown fox"
SIZES = [11, 12, 13, 14, 16, 18]
GUT = 170          # font-name gutter
MET = 92           # metrics column
NAME = label_font(11)
HEAD = label_font(13)

CANDIDATES = [
    ("Cadr Mono",            str(REF),  True),
    ("Cadr Mono Dark",       str(DARK), True),
    ("JetBrains Mono",     "/usr/share/fonts/jetbrains-mono-fonts/JetBrainsMono-Regular.otf", False),
    ("Hack",               "/usr/share/fonts/hack-fonts/Hack-Regular.ttf", False),
    ("Adwaita Mono",       "/usr/share/fonts/adwaita-mono-fonts/AdwaitaMono-Regular.ttf", False),
    ("Noto Sans Mono",     "/usr/share/fonts/google-noto/NotoSansMono[wght].ttf", False),
    ("Liberation Mono",    "/usr/share/fonts/liberation-mono/LiberationMono-Regular.ttf", False),
]


def resolve():
    """Keep the fonts that are actually present, so the sheet degrades
    to what this machine has rather than failing."""
    import glob, os
    found = []
    for name, path, ours in CANDIDATES:
        if pathlib.Path(path).exists():
            found.append((name, path, ours)); continue
        # glob.escape matters here: Noto's variable font is literally
        # named NotoSansMono[wght].ttf, and [wght] is a character class.
        hits = glob.glob(f"/usr/share/fonts/**/{glob.escape(os.path.basename(path))}",
                         recursive=True)
        if hits:
            found.append((name, hits[0], ours))
    return found


FONTS = resolve()


def x_em(path):
    """x-height as a fraction of the em, measured from the outline."""
    f = freetype.Face(path)
    f.set_pixel_sizes(0, 1000)
    f.load_char("x", freetype.FT_LOAD_NO_HINTING)
    return (f.glyph.metrics.horiBearingY / 64) / 1000.0


def run(cov, path, px, text, x, baseline):
    f = freetype.Face(path)
    f.set_pixel_sizes(0, px)
    pen = x
    for ch in text:
        f.load_char(ch, freetype.FT_LOAD_RENDER)
        g = f.glyph
        bm = g.bitmap
        if bm.width and bm.rows:
            mask = Image.frombytes("L", (bm.width, bm.rows), bytes(bm.buffer))
            cov.paste(255, (pen + g.bitmap_left, baseline - g.bitmap_top), mask)
        pen += g.advance.x >> 6
    return pen


def metrics(path, px):
    f = freetype.Face(path)
    f.set_pixel_sizes(0, px)
    f.load_char("M", freetype.FT_LOAD_DEFAULT)
    adv = f.glyph.advance.x >> 6
    line = f.size.height / 64
    return adv, (x_em(path) * px), line


def sheet(path_out, match_xheight):
    """One block per size, one row per font."""
    ref_x = x_em(str(REF))
    blocks = []
    for px in SIZES:
        rows = []
        target_x = ref_x * px
        for name, path, ours in FONTS:
            use = max(6, round(target_x / x_em(path))) if match_xheight else px
            rows.append((name, path, use, ours))
        blocks.append((px, rows))

    rowh = max(28, int(max(SIZES) * 1.9))
    H = sum(26 + len(rows) * (int(px * 1.9) + 6) + 14 for px, rows in blocks) + 20
    W = GUT + max(metrics(p, max(SIZES))[0] for _, p, _ in [(n, p, o) for n, p, o in FONTS]) * len(SPEC) + MET + 30
    cov = Image.new("L", (W, H), 0)
    notes, y = [], 10

    for px, rows in blocks:
        notes.append(("head", 8, y, f"{px}px" + ("  (x-heights matched)" if match_xheight else "")))
        y += 22
        for name, path, use, ours in rows:
            lh = int(px * 1.9) + 6
            adv, xpx, line = metrics(path, use)
            base = y + int(lh * 0.72)
            run(cov, path, use, SPEC, GUT, base)
            tagtxt = name + (f"  {use}px" if use != px else "")
            notes.append(("name", 8, y + int(lh * 0.30), tagtxt, ours))
            notes.append(("met", W - MET - 8, y + int(lh * 0.30),
                          f"x{xpx:.1f} a{adv} l{line:.0f}"))
            y += lh
        y += 14

    img = finish(cov, dark=True)
    d = ImageDraw.Draw(img)
    for n in notes:
        if n[0] == "head":
            d.text((n[1], n[2]), n[3], font=HEAD, fill=235)
        elif n[0] == "name":
            d.text((n[1], n[2]), n[3], font=NAME, fill=(225 if n[4] else 140))
        else:
            d.text((n[1], n[2]), n[3], font=NAME, fill=105)
    d.text((8, H - 16),
           "x = x-height px   a = advance px   l = line height px   "
           "ours in white, others in grey", font=NAME, fill=105)
    img.save(OUT / path_out)


if __name__ == "__main__":
    print("comparing against:", ", ".join(n for n, _, _ in FONTS))
    sheet("compare-size.png", match_xheight=False)
    sheet("compare-xheight.png", match_xheight=True)
    print("wrote proof/compare-size.png and proof/compare-xheight.png")
