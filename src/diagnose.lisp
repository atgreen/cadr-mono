;;;; diagnose.lisp -- numbers, not squinting.
;;;;
;;;; The PNG proofs show whether a letter reads.  They do not show
;;;; whether it sits in its cell, whether its strokes are the weight the
;;;; parameters claim, or whether it aligns to the grid.  This does.

(in-package #:muskako)

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
                    (cond ((> (abs skew) 40) "<-- OFF CENTRE")
                          ((> (abs skew) 12) "<-- drifting")
                          (t "")))))))))
