# SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: OFL-1.1

# CADR Mono build.  Run from the repo root: make -f build/Makefile
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
	$(VENV)/python build/sample.py

ttf: sources venv
	for u in build/*.ufo; do \
	  $(VENV)/fontmake -u $$u -o ttf --output-dir build/ttf \
	    --overlaps-backend pathops || exit 1; \
	done

hint: ttf
	$(VENV)/python build/hint.py
	$(VENV)/python build/rasterproof.py

# fonts/ is what a user downloads: the hinted fonts, at a path that does
# not look like a scratch directory.  Kept in the repo because there is
# no release pipeline to publish them from.
fonts: hint
	mkdir -p fonts
	cp build/ttf-hinted/*.ttf fonts/

# The HINTED fonts are what gets installed.  Unhinted, every horizontal
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
	  print('installed:', TTFont('fonts/CadrMono-Regular.ttf')['name'].getDebugName(5))"

# The Nerd Font family.  A SEPARATE family (CADR Mono NF), built from the
# same generator output with src/nerdfont/ merged in before fontmake,
# so the icons are hinted along with everything else.  Not part of
# "all": it takes minutes, it needs no network but does need the
# vendored 3.8MB of upstream artwork, and its output is ~15MB that
# must never be committed -- see .gitignore.
#
# The base font stays pure: generated glyphs only, OFL-1.1, no
# Reserved Font Name.  Only this variant carries the mixed upstream
# licences in src/nerdfont/LICENSES/.
nf: sources venv
	$(VENV)/python build/nerdfont.py
	mkdir -p fonts-nf
	for u in build/*.ufo; do \
	  $(VENV)/fontmake -u $$u -o ttf --output-dir build/ttf-nf \
	    --overlaps-backend pathops || exit 1; \
	done
	$(VENV)/python build/hint.py --in build/ttf-nf --out fonts-nf
	@# fontmake names output after the UFO, not the family, so these
	@# land as CadrMono-*.ttf -- the same filenames the base font uses.
	@# Installing both would have one silently overwrite the other.
	cd fonts-nf && for f in CadrMono*.ttf; do \
	  mv "$$f" "`echo $$f | sed 's/^CadrMono/CadrMonoNF/'`"; \
	done
	$(VENV)/python build/stripicons.py --dir fonts-nf
	@echo "fonts-nf/ built.  The UFOs now hold merged icons; 'make sources'"
	@echo "(or any base target) regenerates them clean."

clean:
	rm -rf build/ttf build/ttf-hinted build/*.ufo proof/*.png

.PHONY: all venv sources proof ttf hint fonts nf install reload clean
