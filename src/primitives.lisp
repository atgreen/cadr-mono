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
  (- (centre-x) (u 58)))

;;; --- ink measurement and optical centring -----------------------------

(defun flatten-contour (c &optional (steps 16))
  "Sample a contour into points.  A curve lies inside its control
points, so anything measured from control points overstates it."
  (let ((c (normalize-start c)) (pts '()) (pending '()) cur)
    (setf cur (first c))
    (push (cons (px cur) (py cur)) pts)
    (dolist (pnt (append (rest c) (list (first c))))
      (if (eq (kind pnt) :offcurve)
          (push pnt pending)
          (let ((ctrl (nreverse pending)))
            (setf pending '())
            (if (= (length ctrl) 2)
                (let ((s (seg (px cur) (py cur)
                              (px (first ctrl)) (py (first ctrl))
                              (px (second ctrl)) (py (second ctrl))
                              (px pnt) (py pnt))))
                  (loop for i from 1 to steps
                        do (multiple-value-bind (x y) (seg-point s (/ i (float steps)))
                             (push (cons x y) pts))))
                (push (cons (px pnt) (py pnt)) pts))
            (setf cur pnt))))
    pts))

(defun contour-points (contours)
  (loop for c in contours append (flatten-contour c)))

(defun shift-contours (contours dx)
  (if (zerop dx)
      contours
      (mapcar (lambda (c)
                (mapcar (lambda (pnt) (pt (kind pnt) (+ (px pnt) dx) (py pnt))) c))
              contours)))

(defun centre-ink (contours)
  "Shift CONTOURS so their ink sits centred in the cell.

A glyph drawn symmetrically about the cell centre is not necessarily
centred: c loses material on the right where its aperture opens, l
gains it on the right from its tail.  Centring the skeleton is not
centring the letter.

Measured against the field, box centring is what fonts actually do for
n o l f -- all sit within a few units of centre.  Only r j t carry a
deliberate optical offset, and those are applied per glyph rather than
derived, because no formula reproduces them: a centre-of-area blend
that got r roughly right threw n out by 78 units, and every font in the
comparison keeps n centred."
  (let ((pts (contour-points contours)))
    (if (null pts)
        contours
        (let* ((x0 (reduce #'min pts :key #'car))
               (x1 (reduce #'max pts :key #'car))
               (box (/ (+ x0 x1) 2)))
          (shift-contours contours (round (- (/ (p :advance) 2) box)))))))
