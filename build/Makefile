# Muskako build.  Run from the repo root: make -f build/Makefile
VENV := build/venv/bin
ROOT := $(shell pwd)

all: hint proof

# The generator itself has NO Lisp dependencies: SBCL alone writes the
# UFO sources.  Everything in the venv is for turning them into fonts.
$(VENV)/fontmake:
	python3 -m venv build/venv
	$(VENV)/pip install --quiet fontmake skia-pathops ttfautohint-py \
	  freetype-py pillow

venv: $(VENV)/fontmake

sources:
	sbcl --script build/build.lisp

# Proofs come from the compiled, hinted font -- not from the
# generator's own contour lists, which share its assumptions.
proof: hint
	$(VENV)/python build/proofsheet.py
	$(VENV)/python build/compare.py

ttf: sources venv
	for u in build/*.ufo; do \
	  $(VENV)/fontmake -u $$u -o ttf --output-dir build/ttf \
	    --overlaps-backend pathops || exit 1; \
	done

hint: ttf
	$(VENV)/python build/hint.py
	$(VENV)/python build/rasterproof.py

# The HINTED cuts are what gets installed.  Unhinted, every horizontal
# in this font is sub-pixel at 13px.
#
# install depends on PROOF, not just hint.  They used to be siblings off
# hint, so installing never regenerated the sheets and the committed
# proofs could silently lag the font they claimed to show.
install: hint proof
	mkdir -p $(HOME)/.local/share/fonts
	cp build/ttf-hinted/*.ttf $(HOME)/.local/share/fonts/
	fc-cache -f $(HOME)/.local/share/fonts

clean:
	rm -rf build/ttf build/ttf-hinted build/*.ufo proof/*.png

.PHONY: all venv sources proof ttf hint install clean
