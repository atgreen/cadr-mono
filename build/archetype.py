"""How far is each letterform from what everyone else draws?

The confusion metric in confusion.py asks a perceptual question: can a
reader tell these two apart?  This asks a different one, and it is the
question that actually catches a letter that has gone wrong:

    does this glyph look like the thing it is supposed to be?

That is not a fact about the eye, it is a fact about convention -- an
archetype readers learned by reading, and a designer departs from it
only deliberately.  Which makes it measurable in the one way
conventions ever are: against the corpus.  Render the same character
in every monospace on this machine, normalise away size and position,
and ask how far ours sits from theirs.

The number that matters is not the raw distance.  It is the distance
RELATIVE TO HOW MUCH THE REFERENCES DISAGREE WITH EACH OTHER.  Six
fonts drawing 'g' six different ways means being far from their mean
says nothing; six fonts drawing 'J' the same way and ours elsewhere is
a finding.  So the score is ours/spread, and a glyph only gets flagged
when the field is united and we are not.

This is conformance, not quality.  A high score is a question, never a
verdict -- Cadr Mono Dark is SUPPOSED to be an outlier, and so is any
letter the project departed from the field on purpose.  Used the other
way round it would just sand the font down into the average of its
competitors, which is the one result nobody wants.
"""
import freetype, pathlib, sys
from PIL import Image, ImageChops, ImageStat, ImageFilter

CAP = 96                 # normalise every font to this cap height, px
CANVAS = (220, 260)
BLUR = 2.0

REFERENCES = [
    ("JetBrains Mono", "/usr/share/fonts/jetbrains-mono-fonts/JetBrainsMono-Regular.otf"),
    ("Fira Code",      "/usr/share/fonts/fira-code/FiraCode-Regular.ttf"),
    ("Hack",           "/usr/share/fonts/source-foundry-hack-fonts/Hack-Regular.ttf"),
    ("Adwaita Mono",   "/usr/share/fonts/adwaita-mono-fonts/AdwaitaMono-Regular.ttf"),
    ("Noto Sans Mono", "/usr/share/fonts/google-noto/NotoSansMono-Regular.ttf"),
    ("Liberation Mono","/usr/share/fonts/liberation-mono-fonts/LiberationMono-Regular.ttf"),
    ("Source Code Pro","/usr/share/fonts/adobe-source-code-pro-fonts/SourceCodePro-Regular.otf"),
]

CHARS = ("abcdefghijklmnopqrstuvwxyz"
         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
         "0123456789")


def scaled_size(path, target=CAP):
    """Pixel size at which this font's cap height is TARGET."""
    probe = 200
    f = freetype.Face(path)
    f.set_pixel_sizes(0, probe)
    f.load_char("H", freetype.FT_LOAD_RENDER)
    h = f.glyph.bitmap.rows or 1
    return max(8, round(probe * target / h))


def normalised(path, ch, size):
    """CH rendered at SIZE with its ink box centred on a fixed canvas.

    Centring on the INK rather than the advance is the point: we are
    comparing shapes, and two fonts' sidebearings are a fitting
    decision, not a letterform one.
    """
    f = freetype.Face(path)
    f.set_pixel_sizes(0, size)
    f.load_char(ch, freetype.FT_LOAD_RENDER)
    bm = f.glyph.bitmap
    img = Image.new("L", CANVAS, 0)
    if bm.width and bm.rows:
        m = Image.frombytes("L", (bm.width, bm.rows), bytes(bm.buffer))
        img.paste(m, ((CANVAS[0] - bm.width) // 2, (CANVAS[1] - bm.rows) // 2), m)
    return img.filter(ImageFilter.GaussianBlur(BLUR))


def _sum(img):
    return ImageStat.Stat(img).sum[0]


def cosine(a, b):
    dot = _sum(ImageChops.multiply(a, b))
    na = _sum(ImageChops.multiply(a, a))
    nb = _sum(ImageChops.multiply(b, b))
    if na == 0 or nb == 0:
        return 1.0
    return 1.0 - dot / ((na ** 0.5) * (nb ** 0.5))


def main():
    ours_path = sys.argv[1] if len(sys.argv) > 1 else "build/ttf/CadrMono-Regular.ttf"
    refs = [(n, p) for n, p in REFERENCES if pathlib.Path(p).exists()]
    sizes = {p: scaled_size(p) for _, p in refs}
    sizes[ours_path] = scaled_size(ours_path)

    rows = []
    for ch in CHARS:
        ours = normalised(ours_path, ch, sizes[ours_path])
        theirs = [normalised(p, ch, sizes[p]) for _, p in refs]

        d_ours = sum(cosine(ours, t) for t in theirs) / len(theirs)
        pairs = [cosine(theirs[i], theirs[j])
                 for i in range(len(theirs)) for j in range(i + 1, len(theirs))]
        spread = sum(pairs) / len(pairs)
        rows.append((ch, d_ours, spread, d_ours / spread if spread else 0.0))

    rows.sort(key=lambda r: -r[3])
    print(f"\n=== archetype conformance, {pathlib.Path(ours_path).name} ===")
    print(f"    against {len(refs)} references, cap height normalised to {CAP}px\n")
    print(f"    {'ch':<4}{'ours':>9}{'field':>9}{'ratio':>8}")
    for ch, d, s, z in rows[:14]:
        flag = "  <-- the field agrees and we do not" if z > 1.6 else ""
        print(f"    {ch!r:<4}{d:9.4f}{s:9.4f}{z:8.2f}{flag}")
    print(f"\n    median ratio {sorted(r[3] for r in rows)[len(rows)//2]:.2f}")
    return dict((r[0], r[3]) for r in rows)


if __name__ == "__main__":
    main()
