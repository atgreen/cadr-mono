;;;; primitives.lisp -- the parts letters are assembled from.
;;;;
;;;; Primitives may overlap freely: the compiler unions them when it
;;;; builds a static TTF, so a stem and a shoulder simply sit on top of
;;;; one another instead of needing a hand-fitted join.

(in-package #:muskako)

(defun stem-at (x y0 y1 &optional (w (p :stem)))
  "A vertical stroke centred on X, running from Y0 to Y1."
  (rect (- x (/ w 2)) y0 (+ x (/ w 2)) y1))

(defun bar-at (y x0 x1 &optional (w (p :thin)))
  "A horizontal stroke centred on Y."
  (rect x0 (- y (/ w 2)) x1 (+ y (/ w 2))))

(defun dot-at (cx cy &optional (r (p :dot)))
  (ellipse cx cy r r))

(defun ring (cx cy rx ry)
  "A closed round letter: o, and the bowls of b d p q.  The counter is
rotated by the stress angle, which puts the thin points off the vertical
and is what separates a humanist o from a geometric one."
  (list (ellipse cx cy rx ry :ccw t)
        (rotate-about (ellipse cx cy (- rx (p :stem)) (- ry (p :thin)) :ccw nil)
                      cx cy (p :stress))))

(defun shoulder (cx baseline rx ry)
  "The branch of n m h u r, as a half annulus."
  (arch cx baseline rx ry (p :stem) (p :thin)))

(defun centre-x ()
  "Horizontal centre of the cell."
  (/ (p :advance) 2))

(defun round-width ()
  "Outer half-width of the round letters, inset from the cell edge."
  (- (centre-x) 58))
