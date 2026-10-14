;;;; pen.lisp -- skeleton-and-pen stroking.
;;;;
;;;; A letter is a set of centrelines.  Each is stroked with an
;;;; elliptical pen whose width follows the local tangent:
;;;;
;;;;     w(phi) = hypot(stem * sin phi, thin * cos phi)
;;;;
;;;; where phi is the tangent angle measured against the stress axis.
;;;; A vertical tangent gives the full stem; a tangent lying along the
;;;; stress axis gives the thin.  Modulation is therefore continuous
;;;; around a curve, the thin spots land on the stress axis by
;;;; construction, and terminals are cut relative to the stroke rather
;;;; than to the coordinate system.
;;;;
;;;; Offsetting a cubic does not yield a cubic, so each segment is
;;;; subdivided and its control points pushed along the local normals --
;;;; accurate enough at type stroke widths, and it keeps the outline to
;;;; a handful of points per stroke so the result stays hintable.

(in-package #:muskako)

(defstruct (seg (:constructor seg (x0 y0 x1 y1 x2 y2 x3 y3)))
  x0 y0 x1 y1 x2 y2 x3 y3)

(defun line-seg (x0 y0 x1 y1)
  "A straight skeleton segment, expressed as a cubic."
  (seg x0 y0
       (+ x0 (/ (- x1 x0) 3)) (+ y0 (/ (- y1 y0) 3))
       (+ x0 (* 2 (/ (- x1 x0) 3))) (+ y0 (* 2 (/ (- y1 y0) 3)))
       x1 y1))

(defun arc-seg (cx cy rx ry a0 a1)
  "A skeleton arc from A0 to A1 degrees.  Spans of up to a quarter turn
stay accurate; callers split anything larger."
  (let* ((s (deg a0)) (e (deg a1))
         (alpha (* 4/3 (tan (/ (- e s) 4)))))
    (seg (+ cx (* rx (cos s))) (+ cy (* ry (sin s)))
         (+ cx (* rx (cos s)) (* alpha (- (* rx (sin s)))))
         (+ cy (* ry (sin s)) (* alpha (* ry (cos s))))
         (- (+ cx (* rx (cos e))) (* alpha (- (* rx (sin e)))))
         (- (+ cy (* ry (sin e))) (* alpha (* ry (cos e))))
         (+ cx (* rx (cos e))) (+ cy (* ry (sin e))))))

(defun arc-path (cx cy rx ry a0 a1)
  "A skeleton arc of any span, split into quarter turns or less."
  (let* ((n (max 1 (ceiling (abs (- a1 a0)) 90)))
         (step (/ (- a1 a0) n)))
    (loop for i from 0 below n
          collect (arc-seg cx cy rx ry (+ a0 (* i step)) (+ a0 (* (1+ i) step))))))

;;; --- evaluating a segment --------------------------------------------

(defun seg-point (s u)
  (let ((v (- 1 u)))
    (values (+ (* v v v (seg-x0 s)) (* 3 v v u (seg-x1 s))
               (* 3 v u u (seg-x2 s)) (* u u u (seg-x3 s)))
            (+ (* v v v (seg-y0 s)) (* 3 v v u (seg-y1 s))
               (* 3 v u u (seg-y2 s)) (* u u u (seg-y3 s))))))

(defun seg-tangent (s u)
  (let* ((v (- 1 u))
         (dx (+ (* 3 v v (- (seg-x1 s) (seg-x0 s)))
                (* 6 v u (- (seg-x2 s) (seg-x1 s)))
                (* 3 u u (- (seg-x3 s) (seg-x2 s)))))
         (dy (+ (* 3 v v (- (seg-y1 s) (seg-y0 s)))
                (* 6 v u (- (seg-y2 s) (seg-y1 s)))
                (* 3 u u (- (seg-y3 s) (seg-y2 s))))))
    ;; A degenerate tangent at an endpoint falls back to the chord.
    (if (and (< (abs dx) 1d-9) (< (abs dy) 1d-9))
        (values (- (seg-x3 s) (seg-x0 s)) (- (seg-y3 s) (seg-y0 s)))
        (values dx dy))))

(defun split-seg (s u)
  "de Casteljau subdivision at U."
  (flet ((mid (a b) (+ a (* u (- b a)))))
    (let* ((x01 (mid (seg-x0 s) (seg-x1 s))) (y01 (mid (seg-y0 s) (seg-y1 s)))
           (x12 (mid (seg-x1 s) (seg-x2 s))) (y12 (mid (seg-y1 s) (seg-y2 s)))
           (x23 (mid (seg-x2 s) (seg-x3 s))) (y23 (mid (seg-y2 s) (seg-y3 s)))
           (xa (mid x01 x12)) (ya (mid y01 y12))
           (xb (mid x12 x23)) (yb (mid y12 y23))
           (xc (mid xa xb))   (yc (mid ya yb)))
      (values (seg (seg-x0 s) (seg-y0 s) x01 y01 xa ya xc yc)
              (seg xc yc xb yb x23 y23 (seg-x3 s) (seg-y3 s))))))

(defun refine (segs n)
  "Subdivide each segment into N pieces, so offsetting by control-point
displacement stays accurate through the turn."
  (if (<= n 1)
      segs
      (loop for s in segs
            append (let ((acc '()) (rest s))
                     (loop for k from n downto 2
                           do (multiple-value-bind (a b) (split-seg rest (/ 1d0 k))
                                (push a acc) (setf rest b)))
                     (nreverse (cons rest acc))))))

;;; --- the pen ----------------------------------------------------------

(defvar *pen-scale* 1d0
  "Multiplier on the pen for the glyph being drawn.

Monospace practice thins the crowded letters -- m carries three stems
where n carries two, in the same cell -- and that is a deliberate
departure from one stem weight for the whole font.  It costs some stem
consistency for the hinting, which is the accepted trade.")

(defmacro with-pen-scale ((factor) &body body)
  `(let ((*pen-scale* ,factor)) ,@body))

(defun pen-axes ()
  "Solve for the pen's major and minor axes such that a VERTICAL stem
measures exactly (p :stem) and a HORIZONTAL bar exactly (p :thin).

Naming the pen's own axes stem and thin was wrong: because the pen is
rotated by the stress angle, its widest point falls at 90+stress, not
at the vertical, so a stem came out narrower than the parameter said
and a bar came out wider.  Given

    stem^2 = a^2 cos^2 A + b^2 sin^2 A
    thin^2 = a^2 sin^2 A + b^2 cos^2 A

adding and subtracting gives a^2 + b^2 = stem^2 + thin^2 and
a^2 - b^2 = (stem^2 - thin^2) / cos 2A.  So the parameters keep the
meaning a designer expects and the pen is derived from them."
  (let* ((alpha (deg (p :stress)))
         (c2 (cos (* 2 alpha)))
         (ss (* (p :stem) (p :stem)))
         (tt (* (p :thin) (p :thin)))
         (sum (+ ss tt))
         ;; Near 45 degrees of stress the system is ill-conditioned;
         ;; fall back to a circular pen rather than return nonsense.
         (dif (if (< (abs c2) 0.2d0) 0d0 (/ (- ss tt) c2)))
         (a2 (/ (+ sum dif) 2))
         (b2 (/ (- sum dif) 2)))
    (values (* *pen-scale* (sqrt (max 1d0 a2)))
            (* *pen-scale* (sqrt (max 1d0 b2))))))

(defun pen-width (dx dy)
  "Stroke width for a tangent (DX, DY) under the current design's pen."
  (multiple-value-bind (a b) (pen-axes)
    (let ((phi (- (atan dy dx) (deg (p :stress)))))
      (sqrt (+ (* (* a (sin phi)) (* a (sin phi)))
               (* (* b (cos phi)) (* b (cos phi))))))))

(defun offset-seg (s d0 d3)
  "Push S sideways by D0 at its start and D3 at its end, along the
local normals.  Positive is to the left of travel."
  (multiple-value-bind (tx0 ty0) (seg-tangent s 0d0)
    (multiple-value-bind (tx3 ty3) (seg-tangent s 1d0)
      (let* ((l0 (max 1d-9 (sqrt (+ (* tx0 tx0) (* ty0 ty0)))))
             (l3 (max 1d-9 (sqrt (+ (* tx3 tx3) (* ty3 ty3)))))
             (nx0 (* (/ (- ty0) l0) d0)) (ny0 (* (/ tx0 l0) d0))
             (nx3 (* (/ (- ty3) l3) d3)) (ny3 (* (/ tx3 l3) d3)))
        (seg (+ (seg-x0 s) nx0) (+ (seg-y0 s) ny0)
             (+ (seg-x1 s) nx0) (+ (seg-y1 s) ny0)
             (+ (seg-x2 s) nx3) (+ (seg-y2 s) ny3)
             (+ (seg-x3 s) nx3) (+ (seg-y3 s) ny3))))))

(defun side-widths (segs)
  "Half-width at each node of the refined skeleton."
  (append (loop for s in segs
                collect (multiple-value-bind (dx dy) (seg-tangent s 0d0)
                          (/ (pen-width dx dy) 2)))
          (list (multiple-value-bind (dx dy) (seg-tangent (car (last segs)) 1d0)
                  (/ (pen-width dx dy) 2)))))

(defun segs->points (segs &key (start t))
  "Convert offset segments to contour points."
  (append (when start (list (pt :curve (seg-x0 (first segs)) (seg-y0 (first segs)))))
          (loop for s in segs
                append (list (pt :offcurve (seg-x1 s) (seg-y1 s))
                             (pt :offcurve (seg-x2 s) (seg-y2 s))
                             (pt :curve (seg-x3 s) (seg-y3 s))))))

(defvar *insert-extrema* t
  "Whether to split outline segments at their extrema.  Togglable so the
audit can measure the difference rather than assume it.")

(defun extrema-ts (q0 q1 q2 q3)
  "Parameters strictly inside (0,1) where a cubic's derivative in one
axis vanishes -- the positions of its extremes.

The derivative of a cubic is a quadratic: with a = q1-q0, b = q2-q1 and
c = q3-q2, it is (a-2b+c)t^2 + 2(b-a)t + a."
  (let* ((a (- q1 q0)) (b (- q2 q1)) (c (- q3 q2))
         (aa (+ a (* -2 b) c)) (bb (* 2 (- b a))) (cc a)
         (eps 0.02d0)
         (out '()))
    (flet ((keep (tt) (when (and (realp tt) (> tt eps) (< tt (- 1 eps)))
                        (push (float tt 1d0) out))))
      (if (< (abs aa) 1d-9)
          (unless (< (abs bb) 1d-9) (keep (/ (- cc) bb)))
          (let ((disc (- (* bb bb) (* 4 aa cc))))
            (when (>= disc 0)
              (let ((r (sqrt (float disc 1d0))))
                (keep (/ (+ (- bb) r) (* 2 aa)))
                (keep (/ (- (- bb) r) (* 2 aa))))))))
    (sort out #'<)))

(defun split-at-extrema (s)
  "Split S so that every extreme of the curve falls on an on-curve
point.  A curve whose y-max or y-min has no point on it cannot be
hinted: the rasteriser has nothing to snap to the pixel grid."
  (let ((ts (remove-duplicates
             (sort (append (extrema-ts (seg-x0 s) (seg-x1 s) (seg-x2 s) (seg-x3 s))
                           (extrema-ts (seg-y0 s) (seg-y1 s) (seg-y2 s) (seg-y3 s)))
                   #'<)
             :test (lambda (a b) (< (abs (- a b)) 0.02d0)))))
    (if (null ts)
        (list s)
        (let ((pieces '()) (rest s) (prev 0d0))
          (dolist (tt ts)
            ;; remap into what is left of the curve after earlier splits
            (let ((local (/ (- tt prev) (- 1d0 prev))))
              (when (and (> local 0.01d0) (< local 0.99d0))
                (multiple-value-bind (head tail) (split-seg rest local)
                  (push head pieces)
                  (setf rest tail prev tt)))))
          (nreverse (cons rest pieces))))))

(defun with-extrema (segs)
  (if *insert-extrema*
      (loop for s in segs append (split-at-extrema s))
      segs))

(defun stroke-open (segs &key (refine 4))
  "Stroke an open skeleton, returning one closed contour."
  (let* ((segs (refine segs refine))
         (w (side-widths segs))
         (left  (loop for s in segs for i from 0
                      collect (offset-seg s (nth i w) (nth (1+ i) w))))
         (right (loop for s in segs for i from 0
                      collect (offset-seg s (- (nth i w)) (- (nth (1+ i) w))))))
    (ensure-ccw
     (normalize-start
      (append (segs->points (with-extrema left))
             (list (pt :line (seg-x3 (car (last right))) (seg-y3 (car (last right)))))
              (rest (segs->points (with-extrema (mapcar #'flip-seg (reverse right)))
                                  :start t)))))))

(defun flip-seg (s)
  (seg (seg-x3 s) (seg-y3 s) (seg-x2 s) (seg-y2 s)
       (seg-x1 s) (seg-y1 s) (seg-x0 s) (seg-y0 s)))

(defun stroke-closed (segs &key (refine 4))
  "Stroke a closed skeleton, returning outer and inner contours.

Which side of the skeleton is the OUTSIDE depends on the direction the
skeleton runs, so it is decided by area rather than assumed: the larger
contour is the outside and is wound counterclockwise, the smaller is
the counter and is wound clockwise against it.

Getting this backwards does not look obviously wrong on its own -- the
bowl still fills and its counter still cuts -- but it leaves the ring
with winding -1 while every stroke carries +1, so a stem crossing a
bowl SUBTRACTS.  That is the letter appearing xor'd with its own stem."
  (let* ((segs (refine segs refine))
         (w (side-widths segs))
         (a (rest (segs->points
                   (with-extrema (loop for s in segs for i from 0
                                       collect (offset-seg s (nth i w) (nth (1+ i) w)))))))
         (b (rest (segs->points
                   (with-extrema (loop for s in segs for i from 0
                                       collect (offset-seg s (- (nth i w))
                                                           (- (nth (1+ i) w)))))))))
    (if (> (abs (contour-area a)) (abs (contour-area b)))
        (list (ensure-ccw a) (ensure-cw b))
        (list (ensure-ccw b) (ensure-cw a)))))
