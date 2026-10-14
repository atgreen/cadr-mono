# Muskako build.  Run from the repo root: make -f build/Makefile
VENV := build/venv/bin
ROOT := $(shell pwd)

all: proof ttf

sources:
	sbcl --script build/build.lisp

proof: sources

ttf: sources
	$(VENV)/fontmake -u build/Muskako-Regular.ufo -o ttf \
	  --output-dir build/ttf --overlaps-backend pathops
	$(VENV)/fontmake -u build/MuskakoDark-Regular.ufo -o ttf \
	  --output-dir build/ttf --overlaps-backend pathops

install: ttf
	mkdir -p $(HOME)/.local/share/fonts
	cp build/ttf/*.ttf $(HOME)/.local/share/fonts/
	fc-cache -f $(HOME)/.local/share/fonts

clean:
	rm -rf build/ttf build/*.ufo proof/*.png

.PHONY: all sources proof ttf install clean
