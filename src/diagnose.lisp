;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: OFL-1.1

;;;; diagnose.lisp -- numbers, not squinting.
;;;;
;;;; The PNG proofs show whether a letter reads.  They do not show
;;;; whether it sits in its cell, whether its strokes are the weight the
;;;; parameters claim, or whether it aligns to the grid.  This does.

(in-package #:cadr-mono)

(defun contour-area-exact (c)
  "Signed area by the shoelace formula on a FLATTENED contour.

Distinct from GEOM:CONTOUR-AREA, which measures control points and is
what winding decisions use.  This one samples the curve, so it is the
accurate one for measuring ink -- but it must not share the name, or
whichever file loads last silently decides how the font is wound."
  (let ((pts (flatten-contour c 24)) (sum 0d0))
    (loop for (a b) on (append pts (list (first pts)))
          while b
          do (incf sum (- (* (car a) (cdr b)) (* (car b) (cdr a)))))
    (/ sum 2)))

(defun ink-area (glyph)
  "Ink in square units.  Counters wind against their outer contour, so
summing SIGNED areas subtracts them, which is what the eye sees."
  (abs (reduce #'+ (mapcar #'contour-area-exact (glyph-contours glyph))
               :initial-value 0d0)))

(defun string-ink (str)
  (loop for ch across str
        for g = (let ((n (gethash (char-code ch) (code-index)))) (and n (find-glyph n)))
        sum (if g (ink-area g) 0) into ink
        maximize (if g (multiple-value-bind (x0 y0 x1 y1) (ink-bounds g)
                         (declare (ignore x0 x1))
                         (if y0 (- y1 y0) 0))
                     0) into tallest
        finally (return (values (/ ink (max 1 (length str))) tallest))))

(defun colour-test (&rest strings)
  "The practitioner's evenness test: a line of illi should look about as
dark as a line of nnnn.

Measured against nnnn rather than against the mean, and extender
strings are exempt from the heavy flag: a line of h carries an ascender
stem that a line of n does not, so it IS darker, in this and in every
other typeface.  Lightness is the failure we are hunting -- a narrow
glyph floating in its cell."
  (format t "~&~%=== even colour (ink per cell, nnnn = 1.00) ===~%")
  (let ((base (string-ink "nnnn"))
        (cell (* (p :advance) (p :x))))
    (dolist (str strings)
      (multiple-value-bind (ink tallest) (string-ink str)
        (let* ((ratio (/ ink base))
               (extender (> tallest (* 1.12 (p :x)))))
          (format t "  ~10a ~5,2f  ~5,1f%  ~a~a~%"
                  str ratio (* 100 (/ ink cell))
                  (make-string (round (* 42 (min 1.4 ratio))) :initial-element #\#)
                  ;; Crowded glyphs are SUPPOSED to run heavy.  Measured
                  ;; across JetBrains, Hack, Adwaita, Noto, Liberation and
                  ;; Nimbus, m sits at 1.28-1.35 of n and w at 1.15-1.26;
                  ;; not one of them equalises them.  Judging those against
                  ;; 1.00 is the instrument asking for a thinner stem than
                  ;; renders at 13px.
                  (let ((want (cond ((find #\m str) 1.30)
                                    ((find #\w str) 1.20)
                                    (t 1.00))))
                    (cond ((< ratio (* want 0.85)) "  <-- LIGHT")
                          ((and (> ratio (* want 1.15)) (not extender)) "  <-- HEAVY")
                          (extender "  (extender)")
                          ((> want 1.0) (format nil "  (crowded, field ~,2f)" want))
                          (t "")))))))))

(defun ink-bounds (glyph)
  (let ((pts (loop for c in (glyph-contours glyph) append (flatten-contour c))))
    (if (null pts)
        (values nil nil nil nil)
        (values (reduce #'min pts :key #'car) (reduce #'min pts :key #'cdr)
                (reduce #'max pts :key #'car) (reduce #'max pts :key #'cdr)))))

(defun vertical-stem-width ()
  "What a vertical stroke ACTUALLY measures under the current pen."
  (pen-width 0 1))

(defun horizontal-bar-width ()
  (pen-width 1 0))

(defun report ()
  (format t "~&~%=== pen ===~%")
  (format t "  stem parameter      ~,1f~%" (float (p :stem)))
  (format t "  vertical stem ACTUAL ~,1f   (~,1f% of parameter)~%"
          (vertical-stem-width) (* 100 (/ (vertical-stem-width) (p :stem))))
  (format t "  thin parameter      ~,1f~%" (float (p :thin)))
  (format t "  horizontal bar ACTUAL ~,1f~%" (horizontal-bar-width))
  (format t "  thickest at tangent  ~,1f deg (vertical is 90)~%"
          (+ 90 (p :stress)))
  (format t "~%=== cell fit (advance ~a, cap ~a, x ~a) ===~%"
          (p :advance) (p :cap) (p :x))
  (format t "~&~12a ~6a ~6a ~6a ~6a ~6a ~6a~%"
          "glyph" "L" "R" "bot" "top" "width" "skew")
  (dolist (g (all-glyphs))
    ;; Box drawing and blocks are placed absolutely against the cell and
    ;; are asymmetric by design, so centring and fill say nothing useful
    ;; about them.
    (when (and (glyph-contours g) (< (or (glyph-code g) 0) #x2500))
      (multiple-value-bind (x0 y0 x1 y1) (ink-bounds g)
        (when x0
          (let* ((lsb (round x0))
                 (rsb (round (- (p :advance) x1)))
                 ;; A glyph carrying a declared optical offset is
                 ;; SUPPOSED to sit off centre; judge it against its
                 ;; own intent, not against zero.
                 (want (* 2 (u (or (fourth (gethash (glyph-name g)
                                                    *glyph-builders*))
                                   0))))
                 (skew (- (- lsb rsb) want)))
            (format t "~12a ~6d ~6d ~6d ~6d ~6d ~6d ~a~%"
                    (glyph-name g) lsb rsb (round y0) (round y1)
                    (round (- x1 x0)) (- lsb rsb)
                    ;; Only letters and figures are judged on fill.
                    ;; Punctuation is SUPPOSED to be narrow -- flagging a
                    ;; colon for floating in its cell is the instrument
                    ;; misunderstanding the glyph.
                    (let* ((code (or (glyph-code g) 0))
                           (letter? (or (<= 48 code 57) (<= 65 code 90)
                                        (<= 97 code 122)))
                           (fill (if letter?
                                     (/ (- x1 x0) (float (p :advance)))
                                     1.0)))
                      (cond ((> (abs skew) 40) "<-- OFF CENTRE")
                            ((< fill 0.55) "<-- LONELY (floats in its cell)")
                            ((> (abs skew) 12) "<-- drifting")
                            ((< fill 0.62) "<-- narrow")
                            (t ""))))))))))


(defun segment-violations (c)
  "Count cubic segments in C whose extremes carry no on-curve point."
  (let ((c (normalize-start c)) (cur nil) (pending '()) (bad 0))
    (setf cur (first c))
    (dolist (pnt (append (rest c) (list (first c))))
      (if (eq (kind pnt) :offcurve)
          (push pnt pending)
          (let ((ctrl (nreverse pending)))
            (setf pending '())
            (when (= (length ctrl) 2)
              (when (or (extrema-ts (px cur) (px (first ctrl)) (px (second ctrl)) (px pnt))
                        (extrema-ts (py cur) (py (first ctrl)) (py (second ctrl)) (py pnt)))
                (incf bad)))
            (setf cur pnt))))
    bad))

(defun extrema-audit ()
  "How many outline segments hide an extreme?  Every one of them is a
curve the hinting cannot grab."
  (let ((bad 0) (total 0) (glyphs 0))
    (dolist (g (all-glyphs))
      (let ((n (reduce #'+ (mapcar #'segment-violations (glyph-contours g))
                       :initial-value 0)))
        (incf bad n)
        (when (plusp n) (incf glyphs))
        (incf total (reduce #'+ (mapcar (lambda (c) (count :curve c :key #'kind))
                                        (glyph-contours g))
                            :initial-value 0))))
    (format t "~&  segments with a hidden extreme: ~a of ~a curve points, across ~a glyphs~%"
            bad total glyphs)
    bad))
