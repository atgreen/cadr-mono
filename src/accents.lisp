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
         (y (if below (- (ink-bottom b) gap) (+ (ink-top b) gap))))
    (append b (move-contours mark (round x) (round y)))))

(defmacro defaccented (name code base mark &rest opts)
  `(defglyph ,name ,code (with-mark ,base (,mark) ,@opts)))

;;; --- the marks as glyphs in their own right ---------------------------------

(defglyph "grave" 96 (move-contours (mark-grave) (centre-x) (* (p :cap) 0.62)))
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
