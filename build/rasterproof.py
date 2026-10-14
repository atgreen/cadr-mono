"""Rasterise the hinted font with and without hinting, and measure.

Both halves come from the SAME file, toggling FreeType's hinting, so
the only variable is the instructions themselves.

The metric is the share of inked pixels that are FULLY on.  A stem
landing on the pixel grid renders as solid pixels; a sub-pixel stem
smears across two columns as greys.  So "fraction of ink at full
coverage" is a direct measure of the crispness this font was supposed
to deliver at terminal sizes.
"""
import freetype, pathlib
from PIL import Image

FONT = "build/ttf-hinted/Muskako-Regular.ttf"
TEXT = "illi nnnn 0O1lI"
SIZES = [11, 13, 16]
PAD, GAP = 10, 6

def render(size, hinted):
    face = freetype.Face(FONT)
    face.set_pixel_sizes(0, size)
    flags = freetype.FT_LOAD_RENDER
    if not hinted:
        flags |= freetype.FT_LOAD_NO_HINTING
    adv = round(1229 * size / 2048)
    asc = round(1556 * size / 2048)
    h = round((1556 + 471) * size / 2048)
    img = Image.new("L", (adv * len(TEXT) + 4, h + 4), 0)
    full = total = 0
    for i, ch in enumerate(TEXT):
        face.load_char(ch, flags)
        bm = face.glyph.bitmap
        if bm.width == 0:
            continue
        mask = Image.frombytes("L", (bm.width, bm.rows), bytes(bm.buffer))
        x = 2 + i * adv + face.glyph.bitmap_left
        y = 2 + asc - face.glyph.bitmap_top
        # Solid ink through the coverage mask.  A plain paste writes the
        # glyph's whole rectangle, blacking out a neighbour's ink where
        # the boxes overlap.
        img.paste(255, (x, y), mask)
        for v in bm.buffer:
            if v > 8:
                total += 1
                if v >= 250:
                    full += 1
    return img, (full / total if total else 0)

rows, stats = [], []
for size in SIZES:
    for hinted in (False, True):
        img, frac = render(size, hinted)
        rows.append(img)
        stats.append((size, hinted, frac))

# Magnified with nearest-neighbour: the comparison is about where ink
# lands on the pixel grid, which is invisible at actual size.
ZOOM = 6
rows = [i.resize((i.width * ZOOM, i.height * ZOOM), Image.NEAREST) for i in rows]
W = max(i.width for i in rows) + PAD * 2
H = sum(i.height for i in rows) + GAP * (len(rows) + 1)
sheet = Image.new("L", (W, H), 40)
y = GAP
for n, img in enumerate(rows):
    sheet.paste(img, (PAD, y))
    y += img.height + GAP
sheet.save("proof/hinting.png")

print(f"{'size':>5} {'unhinted':>9} {'hinted':>8}  {'change':>8}")
for i in range(0, len(stats), 2):
    s, _, u = stats[i]
    _, _, hn = stats[i + 1]
    print(f"{s:>5} {u*100:>8.1f}% {hn*100:>7.1f}%  {(hn-u)*100:>+7.1f} pts")
