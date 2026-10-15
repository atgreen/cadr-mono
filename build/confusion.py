"""Which pairs of glyphs are hardest to tell apart?

This is the one objective question about letterSHAPE that has an
answer.  Not "is it beautiful" -- nothing measures that -- but "under
the conditions a terminal actually imposes, do these two characters
resolve into different images?"  A reader at 13px in the corner of
their eye is not reading outlines, they are discriminating small grey
rasters, and two characters that produce the same raster are a bug
whatever the curves look like at 400px.

METHOD.  Render every character into its own cell at the terminal
sizes that matter.  Blur to the acuity limit -- the formalised version
of the squint test practitioners have used forever, and the regime
where confusion actually happens.  Then measure the cosine distance
between the two coverage maps.

Cosine, not RMS, because it compares the DISTRIBUTION of ink rather
than its quantity: we are asking whether the shapes differ, and two
glyphs of different weight but identical shape should score as
confusable (they are).

No re-alignment before comparing.  In a monospace the glyph's position
in its cell is not free, so a pair that only separates when you slide
one of them is not separated for a reader.

WHAT THIS DOES NOT DO.  It cannot rank two good fonts, it does not
measure readability, and it says nothing about whether the face is
pleasant to read in.  Legibility research is clear that those are
different questions and that preference does not predict performance.
This catches a letterform that fails to be ITSELF -- which is exactly
the failure mode a parametric font is most prone to, because a glyph
here is a formula that nobody ever looked at in isolation.
"""
import freetype, itertools, pathlib, sys
from PIL import Image, ImageFilter

SIZES = [11, 13, 16]
BLUR = 0.085        # sigma as a fraction of pixel size: the acuity limit

# Everything a terminal routinely puts on screen and a reader has to
# tell apart.  Box drawing and braille are excluded: they are SUPPOSED
# to form continuous shapes with their neighbours, so a distance metric
# built on isolated cells says nothing true about them.
CHARS = ("abcdefghijklmnopqrstuvwxyz"
         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
         "0123456789"
         "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")

# Sequence-versus-single confusions: the classic monospace failures
# that no single-glyph comparison can see.
DIGRAPHS = [("rn", "m"), ("vv", "w"), ("cl", "d"), ("ii", "u"),
            ("nn", "m"), ("I1", "11"), ("l1", "11"), ("O0", "00")]


def raster(face, text, px, cell_w):
    """TEXT laid out at its true advances on a fixed canvas."""
    h = px * 2
    base = int(h * 0.75)
    img = Image.new("L", (cell_w * max(1, len(text)) + px, h), 0)
    pen = px // 2
    for ch in text:
        face.load_char(ch, freetype.FT_LOAD_RENDER)
        g = face.glyph
        bm = g.bitmap
        if bm.width and bm.rows:
            m = Image.frombytes("L", (bm.width, bm.rows), bytes(bm.buffer))
            img.paste(m, (pen + g.bitmap_left, base - g.bitmap_top), m)
        pen += cell_w
    return img.filter(ImageFilter.GaussianBlur(px * BLUR))


def cosine_distance(a, b):
    pa, pb = a.load(), b.load()
    dot = na = nb = 0.0
    for y in range(a.size[1]):
        for x in range(a.size[0]):
            va, vb = pa[x, y], pb[x, y]
            dot += va * vb
            na += va * va
            nb += vb * vb
    if na == 0 or nb == 0:
        return 1.0
    return 1.0 - dot / ((na ** 0.5) * (nb ** 0.5))


def analyse(path, top=20):
    scores = {}
    for px in SIZES:
        f = freetype.Face(str(path))
        f.set_pixel_sizes(0, px)
        f.load_char("M", freetype.FT_LOAD_DEFAULT)
        cell = f.glyph.advance.x >> 6
        imgs = {c: raster(f, c, px, cell) for c in CHARS}
        for a, b in itertools.combinations(CHARS, 2):
            d = cosine_distance(imgs[a], imgs[b])
            scores.setdefault((a, b), []).append(d)
        for a, b in DIGRAPHS:
            wide = max(len(a), len(b))
            ia = raster(f, a.ljust(wide), px, cell)
            ib = raster(f, b.ljust(wide), px, cell)
            scores.setdefault((a, b), []).append(cosine_distance(ia, ib))

    mean = {k: sum(v) / len(v) for k, v in scores.items()}
    return sorted(mean.items(), key=lambda kv: kv[1])


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "build/ttf/CadrMono-Regular.ttf"
    ranked = analyse(path)
    print(f"\n=== confusability, {pathlib.Path(path).name} ===")
    print(f"    cosine distance between blurred rasters, mean over "
          f"{SIZES} px.  Lower = harder to tell apart.\n")
    for (a, b), d in ranked[:25]:
        bar = "#" * int(d * 400)
        print(f"  {a!r:>6} {b!r:<6} {d:7.4f}  {bar}")
    print()
    # A named pair can be queried directly, which is what makes this
    # usable as a regression check rather than only a ranking.
    order = [k for k, _ in ranked]
    table = dict(ranked)
    for pair in [("J", "T"), ("J", "I"), ("l", "I"), ("O", "0"), ("rn", "m")]:
        key = pair if pair in table else pair[::-1]
        if key in table:
            print(f"  {pair[0]!r} vs {pair[1]!r}: {table[key]:.4f}  (rank "
                  f"{order.index(key) + 1} of {len(order)})")


if __name__ == "__main__":
    main()


# --- a second metric, because the first one has a theory in it --------
#
# Global cosine distance models the reader as a template matcher, and
# that model is wrong in a way that matters: it rates a pair by how
# much ink they share, so two letters that agree on their largest
# features score as confusable no matter how clearly they differ
# somewhere small.  J and T share a crossbar and a stem; the hook that
# tells them apart is a tenth of the ink, and the global number barely
# notices it.
#
# Human letter identification is better described as detecting local
# features.  So: divide the cell into regions, normalise each glyph by
# its own total ink, and score the pair by the LARGEST local
# disagreement.  A diagnostic feature is precisely a local difference,
# and this asks whether the pair has one anywhere.
#
# This is still a model, and still wrong in its own ways.  That is the
# point: every objective measure of a letterform embeds a theory of
# the reader, and when the theory is wrong the number is confidently
# wrong.  Two metrics that disagree are more honest than one that does
# not.

GRID = (4, 5)       # columns, rows


def local_max_distance(a, b):
    pa, pb = a.load(), b.load()
    W, H = a.size
    gw, gh = GRID
    ca = [0.0] * (gw * gh)
    cb = [0.0] * (gw * gh)
    for y in range(H):
        ry = min(gh - 1, y * gh // H)
        for x in range(W):
            rx = min(gw - 1, x * gw // W)
            ca[ry * gw + rx] += pa[x, y]
            cb[ry * gw + rx] += pb[x, y]
    ta, tb = sum(ca), sum(cb)
    if ta == 0 or tb == 0:
        return 1.0
    return max(abs(va / ta - vb / tb) for va, vb in zip(ca, cb))
