;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: OFL-1.1

;;;; accents.lisp -- marks, and the letters built by composing them.
;;;;
;;;; An accented letter is its base plus a mark, placed rather than
;;;; redrawn.  That is how fonts do it and it is the only way the set
;;;; stays consistent: move the acute once and every letter carrying it
;;;; moves with it.
;;;;
;;;; Marks are drawn about x = 0 with their own foot at y = 0, so
;;;; placing one is a translation.

(in-package #:cadr-mono)

(defun mark-acute ()
  (list (diag (u -58) 0 (u 62) (u 148) :trim nil)))

(defun mark-grave ()
  ;; Wider than the acute, because as U+0060 this doubles as Lisp's
  ;; backtick and has to read against the apostrophe at 12px.  Measured
  ;; against the field, ours was 175 wide where theirs run 207-253.
  (list (diag (u -86) (u 150) (u 76) 0 :trim nil)))

(defun mark-circumflex ()
  (list (diag (u -96) 0 0 (u 150) :trim nil)
        (diag (u 96) 0 0 (u 150) :trim nil)))

(defun mark-caron ()
  (list (diag (u -96) (u 150) 0 0 :trim nil)
        (diag (u 96) (u 150) 0 0 :trim nil)))

(defun mark-dieresis ()
  (let ((r (* (p :dot) 0.82)) (d (u 104)))
    (list (dot-at (- d) (+ r) r) (dot-at d (+ r) r))))

(defun mark-tilde ()
  (let* ((r (u 112)) (h (u 52)))
    (list (stroke-open (append (arc-path (- (/ r 2)) (u 70) (/ r 2) h 180 0)
                               (arc-path (+ (/ r 2)) (u 70) (/ r 2) h 180 360))))))

(defun mark-macron ()
  (list (stroke-open (list (line-seg (u -120) (u 60) (u 120) (u 60))))))

(defun mark-breve ()
  (list (stroke-open (arc-path 0 (u 150) (u 112) (u 112) 200 340))))

(defun mark-ring ()
  (let ((r (u 96)))
    (stroke-closed (arc-path 0 (+ r (u 10)) (- r (/ (p :stem) 2))
                             (- r (/ (p :stem) 2)) 0 360))))

(defun mark-dotaccent ()
  (list (dot-at 0 (* (p :dot) 0.9) (* (p :dot) 0.86))))

(defun mark-cedilla ()
  "Hangs BELOW the letter, so its own origin is its top."
  (let ((r (u 86)))
    (list (stroke-open (append (list (line-seg 0 0 0 (u -46)))
                               (arc-path (- r) (u -46) r (u 62) 0 -150))))))

(defun mark-ogonek ()
  (let ((r (u 80)))
    (list (stroke-open (arc-path (+ (u 40)) (u -60) r (u 66) 170 10)))))

;;; --- placing ---------------------------------------------------------------

(defun with-mark (base mark &key (gap nil) below (dx nil))
  "BASE's contours with MARK placed over them -- or under, for a
cedilla.  The mark centres on the base's own ink, not on the cell, so
it sits over the letter rather than over the middle of the box."
  (let* ((b (glyph-contours (find-glyph base)))
         (x (or dx (ink-mid-x b)))
         ;; Capitals get a tighter gap than lowercase: an accent over a
         ;; cap has the whole ascender to clear, and the line box has to
         ;; contain it.  Lowercase has room to spare.
         (gap (or gap (if (>= (ink-top b) (* 0.95 (p :cap))) (u 10) (u 34))))
         ;; Clamp to the line box.  An acute over an ASCENDER -- Slovak
         ;; l-acute is the one that catches it -- stacks two tall things
         ;; and lands past the ascent, where a terminal clips it or it
         ;; collides with the row above.  Rather than special-case the
         ;; letter, every mark is held under the ceiling: the accent
         ;; tucks closer instead of the glyph growing.
         (ceil (- (p :line-asc) (u 12)))
         (top (reduce #'max (contour-points mark) :key #'cdr :initial-value 0))
         (y (if below
                (- (ink-bottom b) gap)
                (min (+ (ink-top b) gap) (- ceil top)))))
    (append b (move-contours mark (round x) (round y)))))

(defmacro defaccented (name code base mark &rest opts)
  `(defglyph ,name ,code (with-mark ,base (,mark) ,@opts)))

;;; --- the marks as glyphs in their own right ---------------------------------

;;; U+0060 is the BACKTICK, not a floating accent, and in Lisp it is
;;; quasiquote -- `(a ,b) and '(a ,b) mean different things, so telling
;;; it from the apostrophe is a semantic matter, not a nicety.
;;;
;;; It used to be MARK-GRAVE parked at 0.62 cap, which put it at
;;; 0.579-0.875 of cap while the apostrophe runs 0.570-1.000: the
;;; backtick sat entirely INSIDE the apostrophe's band.  Blurred to the
;;; acuity limit the two were the same blob in the same place, and
;;; ours was the least distinct of the four fonts measured.
;;;
;;; The first guess was that the 43-degree slant was too steep to read
;;; at 13px.  Flattening it to 17 degrees changed the measurement by
;;; nothing at all -- 0.116 to 0.124.  What every other font actually
;;; does is put the backtick in a different vertical BAND: higher, and
;;; clear of the apostrophe.  Doing that scores 0.424, against a field
;;; of 0.313-0.369.
;;;
;;; So this is its own shape and no longer shares the accent's.
;;; MARK-GRAVE stays where it is for a-grave and its family, which want
;;; an accent sitting close over a letter, not a backtick.
(defglyph "grave" 96
  (list (diag (+ (centre-x) (u -120)) (u 794)
              (+ (centre-x) (u 126)) (u 646) :trim nil)))
(defglyph "acute" 180 (move-contours (mark-acute) (centre-x) (* (p :cap) 0.62)))
(defglyph "circumflex" 710 (move-contours (mark-circumflex) (centre-x) (* (p :cap) 0.62)))
(defglyph "caron" 711 (move-contours (mark-caron) (centre-x) (* (p :cap) 0.62)))
(defglyph "dieresis" 168 (move-contours (mark-dieresis) (centre-x) (* (p :cap) 0.62)))
(defglyph "tilde" 732 (move-contours (mark-tilde) (centre-x) (* (p :cap) 0.62)))
(defglyph "macron" 175 (move-contours (mark-macron) (centre-x) (* (p :cap) 0.62)))
(defglyph "breve" 728 (move-contours (mark-breve) (centre-x) (* (p :cap) 0.62)))
(defglyph "ring" 730 (move-contours (mark-ring) (centre-x) (* (p :cap) 0.62)))
(defglyph "dotaccent" 729 (move-contours (mark-dotaccent) (centre-x) (* (p :cap) 0.62)))
(defglyph "cedilla" 184 (move-contours (mark-cedilla) (centre-x) 0))
(defglyph "ogonek" 731 (move-contours (mark-ogonek) (centre-x) 0))

;;; --- Latin-1 and the French set --------------------------------------------
;;;
;;; French needs acute, grave, circumflex, dieresis and cedilla, plus
;;; the oe ligature and guillemets.  The rest of Latin-1 comes free from
;;; the same marks.

(defaccented "agrave"       224 "a" mark-grave)
(defaccented "aacute"       225 "a" mark-acute)
(defaccented "acircumflex"  226 "a" mark-circumflex)
(defaccented "atilde"       227 "a" mark-tilde)
(defaccented "adieresis"    228 "a" mark-dieresis)
(defaccented "aring"        229 "a" mark-ring)
(defaccented "ccedilla"     231 "c" mark-cedilla :below t :gap 0)
(defaccented "egrave"       232 "e" mark-grave)
(defaccented "eacute"       233 "e" mark-acute)
(defaccented "ecircumflex"  234 "e" mark-circumflex)
(defaccented "edieresis"    235 "e" mark-dieresis)
;; An accented i drops its own dot -- the accent replaces it.  Stacking
;; both is a classic composition bug and it is plainly visible in "île".
(defglyph "dotlessi" 305
  (list (stroke-open (list (line-seg (centre-x) 0 (centre-x) (p :x))))
        (entry-serif (centre-x) (p :x))
        (foot-serif (centre-x))))

(defaccented "igrave"       236 "dotlessi" mark-grave)
(defaccented "iacute"       237 "dotlessi" mark-acute)
(defaccented "icircumflex"  238 "dotlessi" mark-circumflex)
(defaccented "idieresis"    239 "dotlessi" mark-dieresis)
(defaccented "ntilde"       241 "n" mark-tilde)
(defaccented "ograve"       242 "o" mark-grave)
(defaccented "oacute"       243 "o" mark-acute)
(defaccented "ocircumflex"  244 "o" mark-circumflex)
(defaccented "otilde"       245 "o" mark-tilde)
(defaccented "odieresis"    246 "o" mark-dieresis)
(defaccented "ugrave"       249 "u" mark-grave)
(defaccented "uacute"       250 "u" mark-acute)
(defaccented "ucircumflex"  251 "u" mark-circumflex)
(defaccented "udieresis"    252 "u" mark-dieresis)
(defaccented "yacute"       253 "y" mark-acute)
(defaccented "ydieresis"    255 "y" mark-dieresis)

(defaccented "Agrave"       192 "A" mark-grave)
(defaccented "Aacute"       193 "A" mark-acute)
(defaccented "Acircumflex"  194 "A" mark-circumflex)
(defaccented "Atilde"       195 "A" mark-tilde)
(defaccented "Adieresis"    196 "A" mark-dieresis)
(defaccented "Aring"        197 "A" mark-ring)
(defaccented "Ccedilla"     199 "C" mark-cedilla :below t :gap 0)
(defaccented "Egrave"       200 "E" mark-grave)
(defaccented "Eacute"       201 "E" mark-acute)
(defaccented "Ecircumflex"  202 "E" mark-circumflex)
(defaccented "Edieresis"    203 "E" mark-dieresis)
(defaccented "Igrave"       204 "I" mark-grave)
(defaccented "Iacute"       205 "I" mark-acute)
(defaccented "Icircumflex"  206 "I" mark-circumflex)
(defaccented "Idieresis"    207 "I" mark-dieresis)
(defaccented "Ntilde"       209 "N" mark-tilde)
(defaccented "Ograve"       210 "O" mark-grave)
(defaccented "Oacute"       211 "O" mark-acute)
(defaccented "Ocircumflex"  212 "O" mark-circumflex)
(defaccented "Otilde"       213 "O" mark-tilde)
(defaccented "Odieresis"    214 "O" mark-dieresis)
(defaccented "Ugrave"       217 "U" mark-grave)
(defaccented "Uacute"       218 "U" mark-acute)
(defaccented "Ucircumflex"  219 "U" mark-circumflex)
(defaccented "Udieresis"    220 "U" mark-dieresis)
(defaccented "Yacute"       221 "Y" mark-acute)
(defaccented "Ydieresis"    376 "Y" mark-dieresis)

;;; ==========================================================================
;;; CENTRAL AND EASTERN EUROPE
;;; ==========================================================================
;;;
;;; Three marks the Latin-1 set never needed, six letters that are
;;; struck through rather than marked, and the declarations that put
;;; them together.  The marks above already covered most of this --
;;; caron, ogonek, breve, macron and the dot were all sitting here
;;; unused by anything west of Germany.

(defun mark-hungarumlaut ()
  "Two acutes, splayed.  Hungarian's long o and u, and nothing else in
Europe.  Not a dieresis that slipped: the strokes are the acute's, so
they move with it."
  (list (diag (u -124) 0 (u -16) (u 148) :trim nil)
        (diag (u 28) 0 (u 136) (u 148) :trim nil)))

(defun mark-commaaccent ()
  "A comma below.  Romanian's s and t take THIS, not the cedilla, and
the distinction is one Romanian typography is emphatic about -- the
cedilla is attached to the letter, the comma hangs free below it.
Latvian's g k l n take it too."
  (list (stroke-open (list (line-seg (u 26) (u -10) (u -34) (u -142))))))

(defun mark-tonos ()
  "Greek tonos.  Set steeper and closer than the Latin acute: in
monotonic Greek it is the only accent there is, so it carries the
whole job of marking stress and must not read as a stray mark."
  (list (diag (u -30) 0 (u 30) (u 150) :trim nil)))

(defglyph "hungarumlaut" 733
  (move-contours (mark-hungarumlaut) (centre-x) (* (p :cap) 0.62)))
(defglyph "commaaccent" 807 (move-contours (mark-commaaccent) (centre-x) 0))
(defglyph "tonos" 900 (move-contours (mark-tonos) (centre-x) (* (p :cap) 0.62)))

;;; --- struck-through letters ------------------------------------------------
;;;
;;; These are not marked letters, they are different letterforms: the
;;; bar crosses the stem rather than sitting over it, so WITH-MARK is
;;; the wrong tool and each one places its own.

(defun with-bar (base y x0 x1 &optional (w (p :rule)))
  "BASE plus a horizontal bar struck through it at Y."
  (append (glyph-contours (find-glyph base))
          (list (rect x0 (- y (/ w 2)) x1 (+ y (/ w 2))))))

(defun with-slash (base x0 y0 x1 y1)
  "BASE plus a diagonal struck through it.  Polish l only."
  (append (glyph-contours (find-glyph base))
          (list (diag x0 y0 x1 y1 :trim nil))))

;; Dcroat / dcroat -- Croatian, Serbian Latin, Slovenian, Sami.
;; The bar crosses the LEFT stem and reaches out past it; on the
;; lowercase it crosses the ascender instead, which is above the bowl.
(defglyph "Dcroat" 272
  (with-bar "D" (* (p :cap) 0.52) (- (left-stem-x) (u 96)) (+ (left-stem-x) (u 108))))
(defglyph "dcroat" 273
  (with-bar "d" (* (p :asc) 0.84) (- (+ (centre-x) (bowl-rx)) (u 104))
                (+ (+ (centre-x) (bowl-rx)) (u 92))))

;; Hbar / hbar -- Maltese.  Also the physics constant, which is why it
;; turns up in source far more often than Maltese alone would explain.
(defglyph "Hbar" 294
  (with-bar "H" (* (p :cap) 0.80) (- (left-stem-x) (u 86))
                (+ (centre-x) (bowl-rx) (u 86))))
(defglyph "hbar" 295
  (with-bar "h" (* (p :asc) 0.84) (- (left-stem-x) (u 86)) (+ (centre-x) (u 24))))

;; Lslash / lslash -- Polish.  A diagonal, not a bar: the stroke cuts
;; the stem at an angle and that slope is the letter's identity.
(defglyph "Lslash" 321
  (with-slash "L" (- (left-stem-x) (u 104)) (* (p :cap) 0.34)
                  (+ (left-stem-x) (u 118)) (* (p :cap) 0.60)))
(defglyph "lslash" 322
  (with-slash "l" (- (centre-x) (u 122)) (* (p :asc) 0.44)
                  (+ (centre-x) (u 108)) (* (p :asc) 0.68)))

;;; --- the composed set ------------------------------------------------------
;;;
;;; One line each.  Grouped by the language that needs them rather than
;;; by codepoint, because that is how the gaps were found and how
;;; anyone will check the work: set a sentence in the language and look.

;; Polish
(defaccented "Aogonek"      260 "A" mark-ogonek :below t :gap 0)
(defaccented "aogonek"      261 "a" mark-ogonek :below t :gap 0)
(defaccented "Cacute"       262 "C" mark-acute)
(defaccented "cacute"       263 "c" mark-acute)
(defaccented "Eogonek"      280 "E" mark-ogonek :below t :gap 0)
(defaccented "eogonek"      281 "e" mark-ogonek :below t :gap 0)
(defaccented "Nacute"       323 "N" mark-acute)
(defaccented "nacute"       324 "n" mark-acute)
(defaccented "Sacute"       346 "S" mark-acute)
(defaccented "sacute"       347 "s" mark-acute)
(defaccented "Zacute"       377 "Z" mark-acute)
(defaccented "zacute"       378 "z" mark-acute)
(defaccented "Zdotaccent"   379 "Z" mark-dotaccent)
(defaccented "zdotaccent"   380 "z" mark-dotaccent)

;; Czech and Slovak
(defaccented "Ccaron"       268 "C" mark-caron)
(defaccented "ccaron"       269 "c" mark-caron)
(defaccented "Dcaron"       270 "D" mark-caron)
(defaccented "Ecaron"       282 "E" mark-caron)
(defaccented "ecaron"       283 "e" mark-caron)
(defaccented "Ncaron"       327 "N" mark-caron)
(defaccented "ncaron"       328 "n" mark-caron)
(defaccented "Rcaron"       344 "R" mark-caron)
(defaccented "rcaron"       345 "r" mark-caron)
(defaccented "Scaron"       352 "S" mark-caron)
(defaccented "scaron"       353 "s" mark-caron)
(defaccented "Tcaron"       356 "T" mark-caron)
(defaccented "Zcaron"       381 "Z" mark-caron)
(defaccented "zcaron"       382 "z" mark-caron)
(defaccented "Uring"        366 "U" mark-ring)
(defaccented "uring"        367 "u" mark-ring)
(defaccented "Lacute"       313 "L" mark-acute)
(defaccented "lacute"       314 "l" mark-acute)
(defaccented "Lcaron"       317 "L" mark-caron)
(defaccented "Racute"       340 "R" mark-acute)
(defaccented "racute"       341 "r" mark-acute)

;; Hungarian
(defaccented "Ohungarumlaut" 336 "O" mark-hungarumlaut)
(defaccented "ohungarumlaut" 337 "o" mark-hungarumlaut)
(defaccented "Uhungarumlaut" 368 "U" mark-hungarumlaut)
(defaccented "uhungarumlaut" 369 "u" mark-hungarumlaut)

;; Romanian.  Comma below, not cedilla.
(defaccented "Abreve"       258 "A" mark-breve)
(defaccented "abreve"       259 "a" mark-breve)
(defaccented "Scommaaccent" 536 "S" mark-commaaccent :below t :gap 0)
(defaccented "scommaaccent" 537 "s" mark-commaaccent :below t :gap 0)
(defaccented "Tcommaaccent" 538 "T" mark-commaaccent :below t :gap 0)
(defaccented "tcommaaccent" 539 "t" mark-commaaccent :below t :gap 0)

;; Turkish.  The dotted capital I is the counterpart of the dotless
;; lowercase the font already had for French -- in Turkish the pair is
;; a real distinction, not a composition detail.
(defaccented "Gbreve"       286 "G" mark-breve)
(defaccented "gbreve"       287 "g" mark-breve)
(defaccented "Idotaccent"   304 "I" mark-dotaccent)
(defaccented "Scedilla"     350 "S" mark-cedilla :below t :gap 0)
(defaccented "scedilla"     351 "s" mark-cedilla :below t :gap 0)

;; Welsh
(defaccented "Wcircumflex"  372 "W" mark-circumflex)
(defaccented "wcircumflex"  373 "w" mark-circumflex)
(defaccented "Ycircumflex"  374 "Y" mark-circumflex)
(defaccented "ycircumflex"  375 "y" mark-circumflex)

;; Maltese
(defaccented "Cdotaccent"   266 "C" mark-dotaccent)
(defaccented "cdotaccent"   267 "c" mark-dotaccent)
(defaccented "Gdotaccent"   288 "G" mark-dotaccent)
(defaccented "gdotaccent"   289 "g" mark-dotaccent)

;; Latvian
(defaccented "Amacron"      256 "A" mark-macron)
(defaccented "amacron"      257 "a" mark-macron)
(defaccented "Emacron"      274 "E" mark-macron)
(defaccented "emacron"      275 "e" mark-macron)
(defaccented "Imacron"      298 "I" mark-macron)
(defaccented "imacron"      299 "dotlessi" mark-macron)
(defaccented "Umacron"      362 "U" mark-macron)
(defaccented "umacron"      363 "u" mark-macron)
(defaccented "Gcommaaccent" 290 "G" mark-commaaccent :below t :gap 0)
;; The lowercase is the exception in the whole set: g already owns the
;; space below, so Latvian turns the comma over and puts it ABOVE.
;; Hanging it under the descender instead pushes past the line box and
;; is not what the letter looks like anyway.
(defun mark-commaaccent-turned ()
  (list (stroke-open (list (line-seg (u -26) (u 142) (u 24) (u 12))))))
(defaccented "gcommaaccent" 291 "g" mark-commaaccent-turned)
(defaccented "Kcommaaccent" 310 "K" mark-commaaccent :below t :gap 0)
(defaccented "kcommaaccent" 311 "k" mark-commaaccent :below t :gap 0)
(defaccented "Lcommaaccent" 315 "L" mark-commaaccent :below t :gap 0)
(defaccented "lcommaaccent" 316 "l" mark-commaaccent :below t :gap 0)
(defaccented "Ncommaaccent" 325 "N" mark-commaaccent :below t :gap 0)
(defaccented "ncommaaccent" 326 "n" mark-commaaccent :below t :gap 0)

;; Lithuanian.  The ogonek'd i keeps its dot -- unlike the acute, the
;; ogonek is below and has no quarrel with it.
(defaccented "Iogonek"      302 "I" mark-ogonek :below t :gap 0)
(defaccented "iogonek"      303 "i" mark-ogonek :below t :gap 0)
(defaccented "Uogonek"      370 "U" mark-ogonek :below t :gap 0)
(defaccented "uogonek"      371 "u" mark-ogonek :below t :gap 0)
(defaccented "Edotaccent"   278 "E" mark-dotaccent)
(defaccented "edotaccent"   279 "e" mark-dotaccent)

;; Czech and Slovak d t l, which do NOT take a caron over the top.
;; The letter already has an ascender there, so the caron turns on its
;; side and becomes a raised stroke immediately to the right -- a
;; different mark in the same slot.  Stacking a caron over the
;; ascender instead is the classic way to make a font look like it was
;; built by someone who does not read Czech.
(defun with-caron-right (base)
  (let* ((b (glyph-contours (find-glyph base)))
         (pts (contour-points b))
         (l (reduce #'min pts :key #'car))
         (r (reduce #'max pts :key #'car))
         ;; The gap is whatever the cell has left after the letter, not a
         ;; fixed figure.  A fixed one suited t and l and pushed d out of
         ;; the cell by eight units -- d is the widest of the three and
         ;; the bold is wider still.
         (room (- (p :advance) (- r l) (p :stem)))
         (gap (max (u 14) (min (u 54) (- room (u 30)))))
         (x (+ r gap))
         (y0 (* (p :asc) 0.52))
         (y1 (* (p :asc) 0.95)))
    (append b (list (diag x y0 (+ x (u 16)) y1 :trim nil)))))

(defglyph "dcaron" 271 (with-caron-right "d"))
(defglyph "tcaron" 357 (with-caron-right "t"))
(defglyph "lcaron" 318 (with-caron-right "l"))

;;; --- Greek, monotonic ------------------------------------------------------
;;;
;;; The alphabet alone does not let you write Greek.  Every stressed
;;; syllable carries a tonos, so a font with 49 Greek letters and no
;;; accents can set the word for "alphabet" and almost nothing else.

(defaccented "Alphatonos"   902 "uni0391" mark-tonos)
(defaccented "Epsilontonos" 904 "uni0395" mark-tonos)
(defaccented "Etatonos"     905 "uni0397" mark-tonos)
(defaccented "Iotatonos"    906 "uni0399" mark-tonos)
(defaccented "Omicrontonos" 908 "uni039F" mark-tonos)
(defaccented "Upsilontonos" 910 "uni03A5" mark-tonos)
(defaccented "Omegatonos"   911 "Omega"   mark-tonos)
(defaccented "Iotadieresis"    938 "uni0399" mark-dieresis)
(defaccented "Upsilondieresis" 939 "uni03A5" mark-dieresis)

(defaccented "alphatonos"   940 "alpha"   mark-tonos)
(defaccented "epsilontonos" 941 "epsilon" mark-tonos)
(defaccented "etatonos"     942 "eta"     mark-tonos)
(defaccented "iotatonos"    943 "iota"    mark-tonos)
(defaccented "omicrontonos" 972 "omicron" mark-tonos)
(defaccented "upsilontonos" 973 "upsilon" mark-tonos)
(defaccented "omegatonos"   974 "omega"   mark-tonos)
(defaccented "iotadieresis"    970 "iota"    mark-dieresis)
(defaccented "upsilondieresis" 971 "upsilon" mark-dieresis)

;; Both marks at once.  The dialytika says the vowel is its own
;; syllable, the tonos says it is stressed, and Greek needs to say both
;; of those about the same letter -- so the tonos goes BETWEEN the two
;; dots rather than above them.
(defun with-dialytika-tonos (base)
  (let* ((b (glyph-contours (find-glyph base)))
         (x (ink-mid-x b))
         (y (+ (ink-top b) (u 24))))
    (append b
            (move-contours (mark-dieresis) (round x) (round y))
            ;; ABOVE the dots, not through them.  Placing both at the
            ;; same height let the tonos cut straight across the
            ;; dialytika and the pair read as one smudged mark.
            (move-contours (mark-tonos) (round x) (round (+ y (u 150)))))))

(defglyph "iotadieresistonos"    912 (with-dialytika-tonos "iota"))
(defglyph "upsilondieresistonos" 944 (with-dialytika-tonos "upsilon"))
