;;;; glyphs.lisp -- the letters, as stroked skeletons.
;;;;
;;;; Each letter is a centreline path run under the elliptical pen.
;;;; Where a stroke continues into another -- the stem of l into its
;;;; tail, the shoulder of n into its right leg -- they are ONE path, so
;;;; the join is continuous by construction rather than two parts
;;;; abutted and hoped over.

(in-package #:muskako)

(defstruct glyph name code advance contours)

(defvar *glyph-builders* (make-hash-table :test #'equal))

(defmacro defglyph (name code &body body)
  "Define a glyph.  :NO-CENTRE as the first body form opts out of
optical centring -- braille and box drawing must stay on their absolute
grid or they stop tiling."
  (let ((centre t))
    (when (eq (first body) :no-centre)
      (setf centre nil body (rest body)))
    `(setf (gethash ,name *glyph-builders*)
           (list ,code ,centre (lambda () (progn ,@body))))))

(defun build-contours (entry)
  (let ((contours (funcall (third entry))))
    (if (second entry) (centre-ink contours) contours)))

(defun all-glyphs ()
  (let (out)
    (maphash (lambda (name entry)
               (push (make-glyph :name name :code (first entry)
                                 :advance (p :advance)
                                 :contours (build-contours entry))
                     out))
             *glyph-builders*)
    (sort out #'< :key (lambda (g) (or (glyph-code g) 0)))))

(defun find-glyph (name)
  (let ((e (gethash name *glyph-builders*)))
    (when e
      (make-glyph :name name :code (first e) :advance (p :advance)
                  :contours (build-contours e)))))

;;; --- shared geometry ---------------------------------------------------

(defun bowl-rx () (- (round-width) (/ (p :stem) 2)))
(defun bowl-ry () (- (+ (/ (p :x) 2) (p :over)) (/ (p :thin) 2)))
(defun bowl-cy () (/ (p :x) 2))

(defglyph "space" 32 nil)

;;; --- round letters -----------------------------------------------------

(defglyph "o" 111
  (stroke-closed (arc-path (centre-x) (bowl-cy) (bowl-rx) (bowl-ry) 0 360)))

(defglyph "c" 99
  ;; A wider bowl than o: the opening removes ink that the extra width
  ;; puts back, so c does not read as the palest letter in the font.
  (let ((half (/ (p :aperture) 2))
        (rx (* (bowl-rx) 1.07)))
    (list (stroke-open (arc-path (centre-x) (bowl-cy) rx (bowl-ry)
                                 half (- 360 half))))))

(defglyph "e" 101
  (let* ((cy (bowl-cy)) (rx (bowl-rx)) (ry (bowl-ry))
         (half (/ (p :aperture) 2)))
    (list
     ;; The bowl starts where the bar meets it on the right, runs over
     ;; the top, down the left, round the bottom and out at the lower
     ;; right -- the whole letter in one sweep.
     (stroke-open (arc-path (centre-x) cy rx ry 0 (- 360 half)))
     (stroke-open (list (line-seg (- (centre-x) rx) cy (+ (centre-x) rx) cy))))))

;;; --- stems and shoulders -------------------------------------------------

(defglyph "n" 110
  (list (stroke-open (list (line-seg (- (centre-x) (bowl-rx)) 0
                                     (- (centre-x) (bowl-rx)) (p :x))))
        (stroke-open (shoulder-path (centre-x) (bowl-rx)))))

(defun foot-serif (cx &optional (span (p :serif)))
  "A flat slab sitting ON the baseline."
  (with-pen-scale ((p :serif-weight))
    (let ((y (/ (* (p :thin) (p :serif-weight)) 2)))
      (stroke-open (list (line-seg (- cx span) y (+ cx span) y))))))

(defun entry-serif (cx y &optional (span (* (p :serif) 0.74)))
  "The slab off the top left of a stem."
  (with-pen-scale ((p :serif-weight))
    (let ((yy (- y (/ (* (p :thin) (p :serif-weight)) 2))))
      (stroke-open (list (line-seg (- cx span) yy cx yy))))))

(defglyph "i" 105
  (list (stroke-open (list (line-seg (centre-x) 0 (centre-x) (p :x))))
        (entry-serif (centre-x) (p :x))
        (foot-serif (centre-x))
        (dot-at (centre-x) (- (p :cap) (u 14) (p :dot)))))

(defglyph "l" 108
  (let* ((r (* (p :tail) 1.30))
         (cy (+ r (/ (p :thin) 2))))
    (list (entry-serif (centre-x) (p :asc) (p :serif))
          (stroke-open
           (if (p :l-tail)
               ;; stem and tail: one path, so the turn is a bend in a
               ;; stroke rather than a quarter-circle glued to a stick
               (cons (line-seg (centre-x) (p :asc) (centre-x) cy)
                     (arc-path (+ (centre-x) r) cy r r 180 270))
               (list (line-seg (centre-x) (p :asc) (centre-x) 0)))))))

;;; --- figures --------------------------------------------------------------

(defglyph "zero" 48
  (let* ((cy (/ (p :cap) 2))
         (rx (- (round-width) (u 6) (/ (p :stem) 2)))
         (ry (- (+ cy (p :over)) (/ (p :thin) 2))))
    (append (stroke-closed (arc-path (centre-x) cy rx ry 0 360))
            (ecase (p :zero)
              (:none nil)
              (:dot (list (dot-at (centre-x) cy (* 0.62 (p :dot)))))
              (:slash (list (stroke-open
                             (list (line-seg (centre-x) (- cy (* ry 0.82))
                                             (centre-x) (+ cy (* ry 0.82)))))))))))

(defglyph "one" 49
  (let ((cx (centre-x)) (h (p :cap)))
    (append (list (stroke-open (list (line-seg cx 0 cx h))))
            ;; the flag is a stroke off the stem, cut by the pen
            ;; start the flag below the stem's flat top, or the pen's
            ;; cap on a diagonal pokes above the cap height
            (list (stroke-open (list (line-seg cx (- h (u 26)) (- cx (u 168)) (- h (u 196))))))
            (when (p :one-serif)
              ;; centred on thin/2 so the serif sits ON the baseline
              ;; rather than straddling it
              (let ((y (/ (p :thin) 2)))
                (list (stroke-open (list (line-seg (- cx (u 150)) y (+ cx (u 150)) y)))))))))

;;; --- punctuation ------------------------------------------------------------

(defglyph "period" 46 (list (dot-at (centre-x) (p :dot))))
(defglyph "hyphen" 45
  (list (stroke-open (list (line-seg (- (centre-x) (u 150)) (* 0.42 (p :x))
                                     (+ (centre-x) (u 150)) (* 0.42 (p :x)))))))

;;; --- bowl family: a stem plus a closed bowl ---------------------------
;;;
;;; The bowl's centreline left or right edge coincides with the stem, so
;;; they are the same stroke where they meet and the union is seamless.

(defun bowl-ring ()
  (arc-path (centre-x) (bowl-cy) (bowl-rx) (bowl-ry) 0 360))

(defun stem-side (side)
  "X of the stem that shares an edge with the bowl."
  (+ (centre-x) (* side (bowl-rx))))

(defglyph "b" 98
  (append (list (stroke-open (list (line-seg (stem-side -1) (p :asc)
                                             (stem-side -1) 0))))
          (stroke-closed (bowl-ring))))

(defglyph "d" 100
  (append (list (stroke-open (list (line-seg (stem-side 1) (p :asc)
                                             (stem-side 1) 0))))
          (stroke-closed (bowl-ring))))

(defglyph "p" 112
  (append (list (stroke-open (list (line-seg (stem-side -1) (p :x)
                                             (stem-side -1) (p :desc)))))
          (stroke-closed (bowl-ring))))

(defglyph "q" 113
  (append (list (stroke-open (list (line-seg (stem-side 1) (p :x)
                                             (stem-side 1) (p :desc)))))
          (stroke-closed (bowl-ring))))

;;; --- shoulder family ---------------------------------------------------
;;;
;;; Shoulder and leg are always one path, so the branch is a bend in a
;;; stroke rather than an arch parked on top of a post.

(defun shoulder-ry (&optional (rx (bowl-rx)))
  "Arch height for an arch of centreline half-width RX.  It has to scale
with the width: m's arches are half as wide as n's, and giving them n's
height makes them tall narrow tunnels instead of shoulders."
  (* (p :x) (p :ease) (/ rx (bowl-rx))))

(defun shoulder-cy (&optional (rx (bowl-rx)))
  (- (p :x) (shoulder-ry rx) (/ (p :thin) 2)))

(defun shoulder-path (cx rx &key (leg t) (to 0))
  "An arch over CX of centreline radius RX, optionally continuing into a
leg that drops to TO."
  (let ((cy (shoulder-cy rx)))
    (append (arc-path cx cy rx (shoulder-ry rx) 180 0)
            (when leg (list (line-seg (+ cx rx) cy (+ cx rx) to))))))

(defglyph "h" 104
  (list (stroke-open (list (line-seg (- (centre-x) (bowl-rx)) 0
                                     (- (centre-x) (bowl-rx)) (p :asc))))
        (stroke-open (shoulder-path (centre-x) (bowl-rx)))))

(defglyph "m" 109
  (let* ((rx (bowl-rx)) (half (/ rx 2)))
    (with-pen-scale (0.82d0)
      (list (stroke-open (list (line-seg (- (centre-x) rx) 0
                                         (- (centre-x) rx) (p :x))))
            (stroke-open (shoulder-path (- (centre-x) half) half))
            (stroke-open (shoulder-path (+ (centre-x) half) half))))))

(defglyph "r" 114
  (let ((rx (bowl-rx)))
    (list (stroke-open (list (line-seg (- (centre-x) rx) 0
                                       (- (centre-x) rx) (p :x))))
          ;; r's shoulder stops partway round and is cut by the pen
          (stroke-open (arc-path (centre-x) (shoulder-cy rx) rx (shoulder-ry rx)
                                 180 22)))))

(defglyph "u" 117
  (let* ((rx (bowl-rx))
         ;; shallower than n's arch: u needs straight sides to read as a
         ;; letter rather than a bucket
         (ry (* (shoulder-ry) 0.60))
         (cy (+ ry (/ (p :thin) 2))))
    (list (stroke-open
           ;; down the left, round the trough, up the right: one path
           (append (list (line-seg (- (centre-x) rx) (p :x) (- (centre-x) rx) cy))
                   (arc-path (centre-x) cy rx ry 180 360)
                   (list (line-seg (+ (centre-x) rx) cy (+ (centre-x) rx) (p :x))))))))

;;; --- diagonal family ----------------------------------------------------
;;;
;;; Each arm is its own stroke rather than one path through the vertex.
;;; Offsetting a path round a sharp corner needs a mitre my stroker does
;;; not build; overlapping strokes union into a blunt vertex instead,
;;; which is honest about what it is until mitring exists.

(defun diag (x0 y0 x1 y1 &key (trim :both))
  "A straight stroke, with its ends cut flat to the lines they sit on.

A butt cap is perpendicular to its stroke, so on a diagonal it
overshoots by half the stroke width times the cosine of the angle --
which is why every diagonal letter was breaking the baseline and the
x-height before this.  TRIM is :both, :start, :end or NIL; an end that
lands inside another stroke (k's knee, w's apex) must not be trimmed or
it opens a gap."
  (let* ((dx (- x1 x0)) (dy (- y1 y0))
         (len (max 1d-6 (sqrt (+ (* dx dx) (* dy dy)))))
         (over (* (/ (pen-width dx dy) 2) (/ (abs dx) len)))
         ;; On a horizontal stroke neither end is "the top", so the
         ;; rule below would tilt the bar.  Callers place those
         ;; explicitly instead.
         (over (if (< (abs dy) 1d-6) 0 over))
         (top-is-0 (> y0 y1))
         (d0 (if (member trim '(:both :start)) (if top-is-0 (- over) over) 0))
         (d1 (if (member trim '(:both :end))   (if top-is-0 over (- over)) 0)))
    (stroke-open (list (line-seg x0 (+ y0 d0) x1 (+ y1 d1))))))

(defglyph "v" 118
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (diag (- cx rx) (p :x) cx 0)
          (diag (+ cx rx) (p :x) cx 0))))

(defglyph "w" 119
  (with-pen-scale (0.84d0)
   (let* ((rx (bowl-rx)) (cx (centre-x))
         ;; raised crossing point shortens the inner arms, the standard
         ;; way of taking weight out of a crowded w
         (mid (* (p :x) 0.64)))
    (list (diag (- cx rx) (p :x) (- cx (* rx 0.46)) 0)
          ;; the inner arms are trimmed only where they touch the
          ;; baseline; the middle apex is interior and must stay long
          (diag (- cx (* rx 0.46)) 0 cx mid :trim :start)
          (diag cx mid (+ cx (* rx 0.46)) 0 :trim :end)
          (diag (+ cx (* rx 0.46)) 0 (+ cx rx) (p :x))))))

(defglyph "x" 120
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (diag (- cx rx) (p :x) (+ cx rx) 0)
          (diag (+ cx rx) (p :x) (- cx rx) 0))))

(defglyph "y" 121
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (diag (- cx rx) (p :x) (+ cx (* rx 0.18)) 0)
          (diag (+ cx rx) (p :x) (- cx (* rx 0.55)) (p :desc)))))

(defglyph "z" 122
  (let ((rx (bowl-rx)) (cx (centre-x)) (h (/ (p :thin) 2)))
    (list ;; bars placed so their ink lands on the lines, not centred on them
          (stroke-open (list (line-seg (- cx rx) (- (p :x) h) (+ cx rx) (- (p :x) h))))
          (diag (+ cx rx) (p :x) (- cx rx) 0)
          (stroke-open (list (line-seg (- cx rx) h (+ cx rx) h))))))

(defglyph "k" 107
  (let* ((rx (bowl-rx)) (cx (centre-x)) (knee (* (p :x) 0.42)))
    (list (stroke-open (list (line-seg (- cx rx) 0 (- cx rx) (p :asc))))
          (diag (+ cx rx) (p :x) (- cx rx) knee :trim :start)
          (diag (- cx rx) knee (+ cx rx) 0 :trim :end))))

;;; --- stem family: a stem that turns ---------------------------------------

(defglyph "t" 116
  (let* ((cx (- (centre-x) (u 54)))
         (r (* (p :tail) 0.78))
         (cy (+ r (/ (p :thin) 2))))
    (list (stroke-open
           (cons (line-seg cx (* (p :x) 1.42) cx cy)
                 (arc-path (+ cx r) cy r r 180 270)))
          (with-pen-scale ((p :serif-weight))
            (stroke-open (list (line-seg (- cx (u 196)) (p :x)
                                         (+ cx (u 224)) (p :x))))))))

(defglyph "f" 102
  (let* ((cx (- (centre-x) (u 36)))
         (r (* (p :tail) 0.86))
         (cy (- (p :asc) r (/ (p :thin) 2))))
    (list (stroke-open
           (append (list (line-seg cx 0 cx cy))
                   (arc-path (+ cx r) cy r r 180 90)))
          (stroke-open (list (line-seg (- cx (u 204)) (p :x) (+ cx (u 228)) (p :x)))))))

(defglyph "j" 106
  (let* ((cx (+ (centre-x) (u 38)))
         (r (* (p :tail) 1.72))
         (cy (+ (p :desc) r (/ (p :thin) 2))))
    (list (stroke-open
           (append (list (line-seg cx (p :x) cx cy))
                   (arc-path (- cx r) cy r r 0 -90)))
          (entry-serif cx (p :x) (p :serif))
          (dot-at cx (- (p :cap) (u 14) (p :dot))))))

;;; --- the three that fit no family -----------------------------------------

(defglyph "a" 97
  ;; Double-storey: a lower bowl sharing its right edge with the stem,
  ;; and a top arc springing from the stem's head and curving away left.
  (let* ((rx (bowl-rx))
         (ry-b (* (p :x) 0.30))
         (cy-b (+ ry-b (/ (p :thin) 2) (- (p :over))))
         (ry-t (* (p :x) 0.21))
         (cy-t (- (p :x) ry-t (/ (p :thin) 2))))
    (append (list (stroke-open (list (line-seg (+ (centre-x) rx) 0
                                               (+ (centre-x) rx) cy-t))))
            (stroke-closed (arc-path (centre-x) cy-b rx ry-b 0 360))
            (list (stroke-open (arc-path (centre-x) cy-t rx ry-t 0 168))))))

(defglyph "g" 103
  ;; Single-storey: o's bowl, then the stem carries on below the line
  ;; and hooks left, the same turn j makes.
  (let* ((rx (bowl-rx))
         (r (* (p :tail) 1.42))
         (cy (+ (p :desc) r (/ (p :thin) 2))))
    (append (stroke-closed (arc-path (centre-x) (bowl-cy) rx (bowl-ry) 0 360))
            (list (stroke-open
                   (append (list (line-seg (+ (centre-x) rx) (p :x)
                                           (+ (centre-x) rx) cy))
                           (arc-path (- (+ (centre-x) rx) r) cy r r 0 -90)))))))

(defglyph "s" 115
  ;; Two arcs that meet at mid-height by construction: the upper one
  ;; ends at the bottom of its circle and the lower one starts at the
  ;; top of its own, and the two centres are placed so those are the
  ;; same point.  The spine is then continuous without a join to fudge.
  ;; Three constraints fix the geometry exactly, so none of it is tuned:
  ;;   the arcs meet          cy_u - ry = cy_l + ry
  ;;   the top lands on       cy_u + ry + thin/2 =  x + overshoot
  ;;   the bottom lands on    cy_l - ry - thin/2 = -overshoot
  ;; which solve to ry = (x + 2*overshoot - thin)/4 and cy = (x +/- 2ry)/2.
  (let* ((rx (* (bowl-rx) 0.95))
         (ry (/ (+ (p :x) (* 2 (p :over)) (- (p :thin))) 4))
         (cy-u (/ (+ (p :x) (* 2 ry)) 2))
         (cy-l (/ (- (p :x) (* 2 ry)) 2)))
    (list (stroke-open
           (append (arc-path (centre-x) cy-u rx ry 10 270)
                   (arc-path (centre-x) cy-l rx ry 90 -170))))))
