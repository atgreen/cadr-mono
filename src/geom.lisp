;;;; geom.lisp -- contours, and the bezier arithmetic under them.
;;;;
;;;; A contour is a list of points, each (KIND X Y) where KIND is
;;;; :line, :curve or :offcurve -- the same vocabulary GLIF uses, so
;;;; emitting a UFO is a transcription rather than a translation.
;;;; Contours are always closed.

(in-package #:cadr-mono)

(defconstant +kappa+ 0.5522847498307936d0
  "Control-point ratio that makes a cubic bezier approximate a quarter arc.")

(defun pt (kind x y) (list kind (round x) (round y)))
(defun px (p) (second p))
(defun py (p) (third p))
(defun kind (p) (first p))

(defun deg (degrees) (* pi (/ degrees 180d0)))

(defun rotate-about (contour cx cy degrees)
  "Rotate CONTOUR about (CX, CY).  Used to tilt a counter off the
vertical axis, which is what gives the face its humanist stress."
  (if (zerop degrees)
      contour
      (let ((c (cos (deg degrees))) (s (sin (deg degrees))))
        (mapcar (lambda (p)
                  (let ((dx (- (px p) cx)) (dy (- (py p) cy)))
                    (pt (kind p)
                        (+ cx (- (* dx c) (* dy s)))
                        (+ cy (+ (* dx s) (* dy c))))))
                contour))))

(defun normalize-start (contour)
  "Rotate CONTOUR so it begins on an on-curve point."
  (let ((n (position-if (lambda (p) (not (eq (kind p) :offcurve))) contour)))
    (if (or (null n) (zerop n))
        contour
        (append (nthcdr n contour) (subseq contour 0 n)))))

(defun reverse-contour (contour)
  "Reverse winding direction.  Counters must wind against their outer
contour or they fill in solid instead of cutting a hole."
  (let* ((rev (reverse contour))
         ;; After reversing, on-curve kinds must shift by one position
         ;; because a :curve point describes the segment arriving at it.
         (kinds (mapcar #'kind rev)))
    ;; A point's type describes the segment ENDING at it, so after
    ;; reversal the deciding neighbour is the one BEFORE it, not after.
    ;; This went unnoticed while every reversed contour was all curve
    ;; points -- offcurve on both sides, so either neighbour gave the
    ;; same answer.  The terminal caps of an open stroke are the first
    ;; line points to pass through here, and they came out declared as
    ;; curves with two control points attached.
    (loop for p in rev
          for i from 0
          for prev = (nth (mod (1- i) (length rev)) kinds)
          collect (if (eq (kind p) :offcurve)
                      p
                      (pt (if (eq prev :offcurve) :curve :line) (px p) (py p)))
          into out
          finally (return (normalize-start out)))))

(defun ellipse (cx cy rx ry &key (ccw t) (tension nil))
  "A closed round contour as four cubic arcs.  TENSION above +kappa+
flattens the extremes and squares the shoulders -- a pure ellipse is
too pointed at the top and bottom to read as a typographic bowl."
  (let* ((k (or tension (ignore-errors (p :tension)) +kappa+))
         (kx (* k rx)) (ky (* k ry))
         (c (list (pt :curve (+ cx rx) cy)
                  (pt :offcurve (+ cx rx) (+ cy ky))
                  (pt :offcurve (+ cx kx) (+ cy ry))
                  (pt :curve cx (+ cy ry))
                  (pt :offcurve (- cx kx) (+ cy ry))
                  (pt :offcurve (- cx rx) (+ cy ky))
                  (pt :curve (- cx rx) cy)
                  (pt :offcurve (- cx rx) (- cy ky))
                  (pt :offcurve (- cx kx) (- cy ry))
                  (pt :curve cx (- cy ry))
                  (pt :offcurve (+ cx kx) (- cy ry))
                  (pt :offcurve (+ cx rx) (- cy ky)))))
    (if ccw c (reverse-contour c))))

(defun rect (x0 y0 x1 y1)
  "An axis-aligned rectangle, wound counterclockwise."
  (list (pt :line x0 y0) (pt :line x1 y0) (pt :line x1 y1) (pt :line x0 y1)))

(defun stroke-between (x0 y0 x1 y1 w)
  "A straight stroke of width W between two points, ends cut square to
the stroke.  The diagonals of k v w x y z."
  (let* ((dx (- x1 x0)) (dy (- y1 y0))
         (len (max 1d-6 (sqrt (+ (* dx dx) (* dy dy)))))
         (nx (* (/ dy len) (/ w 2)))
         (ny (* (/ (- dx) len) (/ w 2))))
    (list (pt :line (+ x0 nx) (+ y0 ny))
          (pt :line (+ x1 nx) (+ y1 ny))
          (pt :line (- x1 nx) (- y1 ny))
          (pt :line (- x0 nx) (- y0 ny)))))

(defun arch (cx baseline rx ry thick thin-v &key (dir 1))
  "A half-annulus opening downward: the shoulder of n m h u r.
RX and RY are CENTRELINE radii, so the legs land exactly on stems drawn
at cx+/-rx rather than straddling them and bulging the join.  DIR -1
flips it into a trough, which is the bowl of u and the underside of j.

Wound counterclockwise to match RECT: under the nonzero fill rule two
overlapping contours of opposite winding cancel, which punches a hole
where a stem and its shoulder are supposed to merge."
  (let* ((orx (+ rx (/ thick 2)))  (ory (+ ry (/ thin-v 2)))
         (irx (- rx (/ thick 2)))  (iry (- ry (/ thin-v 2)))
         (kx (* +kappa+ orx))  (ky (* +kappa+ ory))
         (ikx (* +kappa+ irx)) (iky (* +kappa+ iry)))
    (funcall (if (plusp dir) #'reverse-contour #'identity)
     (list (pt :line (- cx orx) baseline)
          (pt :offcurve (- cx orx) (+ baseline (* dir ky)))
          (pt :offcurve (- cx kx) (+ baseline (* dir ory)))
          (pt :curve cx (+ baseline (* dir ory)))
          (pt :offcurve (+ cx kx) (+ baseline (* dir ory)))
          (pt :offcurve (+ cx orx) (+ baseline (* dir ky)))
          (pt :curve (+ cx orx) baseline)
          (pt :line (+ cx irx) baseline)
          (pt :offcurve (+ cx irx) (+ baseline (* dir iky)))
          (pt :offcurve (+ cx ikx) (+ baseline (* dir iry)))
          (pt :curve cx (+ baseline (* dir iry)))
          (pt :offcurve (- cx ikx) (+ baseline (* dir iry)))
          (pt :offcurve (- cx irx) (+ baseline (* dir iky)))
          (pt :curve (- cx irx) baseline)))))

(defun contour-area (c)
  "Signed area by the shoelace formula.  Positive is counterclockwise.
Control points are included, which is close enough for judging winding
and relative size."
  (let ((sum 0))
    (loop for (a b) on (append c (list (first c)))
          while b
          do (incf sum (- (* (px a) (py b)) (* (px b) (py a)))))
    (/ sum 2)))

(defun contour-sign (c)
  (if (minusp (contour-area c)) -1 1))

(defun ensure-ccw (c)
  "Force counterclockwise winding.

A stroked outline's winding follows its direction of travel, so a
stroke drawn downward where its neighbours run upward comes out
reversed -- and under the nonzero fill rule it then SUBTRACTS from
whatever it overlaps instead of merging with it.  That is a hole where
a join should be, and it is invisible until two strokes happen to
cross."
  (if (minusp (contour-sign c)) (reverse-contour c) c))

(defun ensure-cw (c)
  "Force clockwise winding -- what a counter needs in order to cut a
hole in the shape around it."
  (if (plusp (contour-sign c)) (reverse-contour c) c))

(defun bounds (contours)
  "Ink bounding box of CONTOURS as (values x0 y0 x1 y1)."
  (let (xs ys)
    (dolist (c contours)
      (dolist (p c) (push (px p) xs) (push (py p) ys)))
    (if xs
        (values (reduce #'min xs) (reduce #'min ys)
                (reduce #'max xs) (reduce #'max ys))
        (values 0 0 0 0))))

;;; --- elliptical arcs -------------------------------------------------

(defun arc-points (cx cy rx ry a0 a1)
  "Cubic approximation of the elliptical arc from A0 to A1 (degrees),
split so no segment exceeds a quarter turn.  Returns points beginning
with an on-curve point at A0."
  (let* ((span (- a1 a0))
         (n (max 1 (ceiling (abs span) 90)))
         (step (/ span n))
         (out (list (pt :curve (+ cx (* rx (cos (deg a0))))
                              (+ cy (* ry (sin (deg a0))))))))
    (dotimes (i n)
      (let* ((s (deg (+ a0 (* i step))))
             (e (deg (+ a0 (* (1+ i) step))))
             (alpha (* 4/3 (tan (/ (- e s) 4)))))
        (setf out
              (append out
                      (list (pt :offcurve
                                (+ cx (* rx (cos s)) (* alpha (- (* rx (sin s)))))
                                (+ cy (* ry (sin s)) (* alpha (* ry (cos s)))))
                            (pt :offcurve
                                (- (+ cx (* rx (cos e))) (* alpha (- (* rx (sin e)))))
                                (- (+ cy (* ry (sin e))) (* alpha (* ry (cos e)))))
                            (pt :curve (+ cx (* rx (cos e))) (+ cy (* ry (sin e)))))))))
    out))

(defun arc-annulus (cx cy rx ry thick thin-v a0 a1 &key (stress nil))
  "An open ring segment -- the body of c, e, s and the bowl of a.

The inner arc is rotated about the centre by the stress angle.  That one
move does two humanist jobs at once: it tilts the thin points off the
vertical axis, and because the inner endpoints no longer sit radially
opposite the outer ones, it leaves the terminals cut on a slant instead
of chopped square."
  (let* ((tilt (or stress (ignore-errors (p :stress)) 0))
         (inner (rotate-about (arc-points cx cy (- rx thick) (- ry thin-v) a1 a0)
                              cx cy tilt)))
    (append (arc-points cx cy rx ry a0 a1)
            (list (pt :line (px (first inner)) (py (first inner))))
            (rest inner))))
