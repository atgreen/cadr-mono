# Muskako build.  Run from the repo root: make -f build/Makefile
VENV := build/venv/bin
ROOT := $(shell pwd)

all: hint proof fonts

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

# fonts/ is what a user downloads: the hinted cuts, at a path that does
# not look like a scratch directory.  Kept in the repo because there is
# no release pipeline to publish them from.
fonts: hint
	mkdir -p fonts
	cp build/ttf-hinted/*.ttf fonts/

# The HINTED cuts are what gets installed.  Unhinted, every horizontal
# in this font is sub-pixel at 13px.
#
# install depends on PROOF, not just hint.  They used to be siblings off
# hint, so installing never regenerated the sheets and the committed
# proofs could silently lag the font they claimed to show.
install: hint proof fonts
	mkdir -p $(HOME)/.local/share/fonts
	cp fonts/*.ttf $(HOME)/.local/share/fonts/
	fc-cache -f $(HOME)/.local/share/fonts

# Nudge already-running applications toward the rebuilt font.
#
# Replacing a font file does not dislodge it from a running process:
# VTE, Pango and the toolkits hold an open FreeType face and have no
# reason to re-resolve a path whose family name has not changed, and
# one process serves every tab, so a new tab shows the old font too.
# Rewriting the caches and toggling the GNOME monospace key gives Pango
# cause to drop what it has cached.  It does not always take, and a
# terminal that does not pick it up has to be restarted.
#
# NOT "fc-cache -rf".  The -r erases every cache file before rebuilding,
# and a running application has those caches MMAPPED -- pulling them out
# from under it leaves it with an invalid view of the font set, which
# renders as most glyphs silently vanishing.  It takes a restart of that
# application to recover, which is the opposite of the point here.
reload: install
	fc-cache -f
	@if command -v gsettings >/dev/null 2>&1; then \
	  old=$$(gsettings get org.gnome.desktop.interface monospace-font-name); \
	  if [ "$$old" = "''" ]; then \
	    echo "no GNOME monospace font set; caches refreshed"; \
	  else \
	    gsettings set org.gnome.desktop.interface monospace-font-name 'Monospace 12'; \
	    sleep 1; \
	    gsettings set org.gnome.desktop.interface monospace-font-name "$$old"; \
	    echo "nudged $$old -- if the terminal still shows the old font, restart it"; \
	  fi; \
	else \
	  echo "no gsettings; caches refreshed, restart the terminal"; \
	fi
	@$(VENV)/python -c "from fontTools.ttLib import TTFont; \
	  print('installed:', TTFont('fonts/Muskako-Regular.ttf')['name'].getDebugName(5))"

clean:
	rm -rf build/ttf build/ttf-hinted build/*.ufo proof/*.png

.PHONY: all venv sources proof ttf hint fonts install reload clean
