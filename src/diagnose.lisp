;;;; diagnose.lisp -- numbers, not squinting.
;;;;
;;;; The PNG proofs show whether a letter reads.  They do not show
;;;; whether it sits in its cell, whether its strokes are the weight the
;;;; parameters claim, or whether it aligns to the grid.  This does.

(in-package #:muskako)

(defun contour-area (c)
  "Signed area by the shoelace formula on a flattened contour."
  (let ((pts (flatten-contour c 24)) (sum 0d0))
    (loop for (a b) on (append pts (list (first pts)))
          while b
          do (incf sum (- (* (car a) (cdr b)) (* (car b) (cdr a)))))
    (/ sum 2)))

(defun ink-area (glyph)
  "Ink in square units.  Counters wind against their outer contour, so
summing SIGNED areas subtracts them, which is what the eye sees."
  (abs (reduce #'+ (mapcar #'contour-area (glyph-contours glyph))
               :initial-value 0d0)))

(defun colour-test (&rest strings)
  "The practitioner's evenness test: a line of illi should look about
as dark as a line of nnnn.  Making it a number means it can fail a
build instead of waiting for someone to notice."
  (format t "~&~%=== even colour (ink per cell, % of advance x x-height) ===~%")
  (let ((results
          (loop for str in strings
                collect (cons str
                              (/ (loop for ch across str
                                       for g = (let ((n (gethash (char-code ch) (code-index))))
                                                 (and n (find-glyph n)))
                                       sum (if g (ink-area g) 0))
                                 (* (length str) (p :advance) (p :x)))))))
    (let ((best (reduce #'max results :key #'cdr)))
      (dolist (r results)
        (format t "  ~10a ~5,1f%   ~a~a~%" (car r) (* 100 (cdr r))
                (make-string (round (* 60 (/ (cdr r) best))) :initial-element #\#)
                (let ((dev (abs (- 1 (/ (cdr r) (/ (reduce #'+ results :key #'cdr)
                                                   (length results)))))))
                  (if (> dev 0.18) "  <-- UNEVEN" "")))))))

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
    (when (and (glyph-contours g) (< (or (glyph-code g) 0) #x2800))
      (multiple-value-bind (x0 y0 x1 y1) (ink-bounds g)
        (when x0
          (let* ((lsb (round x0))
                 (rsb (round (- (p :advance) x1)))
                 (skew (- lsb rsb)))
            (format t "~12a ~6d ~6d ~6d ~6d ~6d ~6d ~a~%"
                    (glyph-name g) lsb rsb (round y0) (round y1)
                    (round (- x1 x0)) skew
                    ;; punctuation is meant to be small; only judge
                    ;; glyphs tall enough to be letters or figures
                    (let ((fill (if (> (- y1 y0) (* 0.4 (p :x)))
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
