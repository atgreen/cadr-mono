;;;; glyphs.lisp -- the letters, as stroked skeletons.
;;;;
;;;; Each letter is a centreline path run under the elliptical pen.
;;;; Where a stroke continues into another -- the stem of l into its
;;;; tail, the shoulder of n into its right leg -- they are ONE path, so
;;;; the join is continuous by construction rather than two parts
;;;; abutted and hoped over.

(in-package #:cadr-mono)

(defstruct glyph name code advance contours)

(defvar *glyph-builders* (make-hash-table :test #'equal))

(defmacro defglyph (name code &body body)
  "Define a glyph.

:NO-CENTRE opts out of centring -- braille and box drawing must stay on
their absolute grid or they stop tiling.  :SHIFT n moves the glyph n
per mille after centring, for the handful of letters that want an
optical offset the box cannot give them."
  (let ((centre t) (shift 0))
    (loop while (keywordp (first body))
          do (case (first body)
               (:no-centre (setf centre nil body (rest body)))
               (:shift (setf shift (second body) body (cddr body)))
               (t (return))))
    `(setf (gethash ,name *glyph-builders*)
           (list ,code ,centre (lambda () (progn ,@body)) ,shift))))

(defun build-contours (entry)
  (let ((contours (funcall (third entry))))
    ;; Glyphs that opt out of centring opt out of the slope too: box
    ;; drawing and braille are placed against the cell, and a sloped
    ;; rule cannot meet its neighbour across a cell boundary.  An
    ;; italic terminal still has to draw a straight box.
    (when (second entry)
      (setf contours (shear-contours contours (p :slant)))
      (setf contours (centre-ink contours)))
    (let ((shift (or (fourth entry) 0)))
      (if (zerop shift) contours (shift-contours contours (u shift))))))

(defun all-glyphs ()
  (let (out)
    (maphash (lambda (name entry)
               (push (make-glyph :name name :code (first entry)
                                 :advance (p :advance)
                                 :contours (build-contours entry))
                     out))
             *glyph-builders*)
    (sort out #'< :key (lambda (g) (or (glyph-code g) 0)))))

(defvar *code-index* nil)

(defun code-index ()
  "Codepoint to glyph name."
  (or *code-index*
      (setf *code-index*
            (let ((h (make-hash-table)))
              (maphash (lambda (name e)
                         (when (first e) (setf (gethash (first e) h) name)))
                       *glyph-builders*)
              h))))

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
              (:dot (list (dot-at (centre-x) cy (* 0.95 (p :dot)))))
              ;; A real diagonal.  This branch used to draw a line from
              ;; (centre-x ...) to (centre-x ...) -- a vertical BAR, not a
              ;; slash, whatever the keyword said.  Corner to corner of the
              ;; bowl exactly: shorter and the mark stops carrying at 13px,
              ;; longer and thin spurs poke out past the oval at display
              ;; sizes.
              (:slash (list (stroke-open
                             (list (line-seg (- (centre-x) rx) (- cy ry)
                                             (+ (centre-x) rx) (+ cy ry))))))))))

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
    ;; m is thinned only slightly.  Thinning it until its ink MATCHED n
    ;; was wrong: measured across the field, m runs 1.28-1.35 times n's
    ;; ink in every one of JetBrains, Hack, Adwaita, Noto, Liberation
    ;; and Nimbus.  Nobody equalises it, because at 13px a stem thinned
    ;; to 137 units is 0.87px and renders grey where a full one renders
    ;; solid.  The even-colour rule is about narrow glyphs FLOATING in
    ;; their cells, not about crowded ones.
    (with-pen-scale ((crowded 0.95d0))
      (list (stroke-open (list (line-seg (- (centre-x) rx) 0
                                         (- (centre-x) rx) (p :x))))
            (stroke-open (shoulder-path (- (centre-x) half) half))
            (stroke-open (shoulder-path (+ (centre-x) half) half))))))

(defglyph "r" 114
  :shift 22
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
    (if (p :italic)
        ;; Converging sides into a NARROW rounded foot.  Built as a wide
        ;; trough -- which is what this was -- the italic v has the same
        ;; silhouette as u, and "value" reads "ualue".  The rounded foot
        ;; is the italic character; the convergence is what makes it a v.
        ;; The foot is a rounded POINT, not a small bowl.  At a third of
        ;; the width it still read as u: convergence has to be steep
        ;; enough to register at 16px, not merely present.
        (let* ((fx (* rx 0.15)) (fy (* (p :x) 0.13))
               (cy (+ fy (/ (p :thin) 2))))
          (list (stroke-open
                 (append (list (line-seg (- cx rx) (p :x) (- cx fx) cy))
                         (arc-path cx cy fx fy 180 360)
                         (list (line-seg (+ cx fx) cy (+ cx rx) (p :x)))))))
        (list (diag (- cx rx) (p :x) cx 0)
              (diag (+ cx rx) (p :x) cx 0)))))

(defglyph "w" 119
  (if (p :italic)
      ;; Two narrow feet, same reasoning as v: wide troughs here would
      ;; make "uvw" read "uuw".
      (with-pen-scale ((crowded 0.95d0))
        (let* ((rx (bowl-rx)) (cx (centre-x))
               (fx (* rx 0.11)) (fy (* (p :x) 0.11))
               (cy (+ fy (/ (p :thin) 2)))
               (lf (- cx (* rx 0.50))) (rf (+ cx (* rx 0.50)))
               (mid (* (p :x) 0.72)))
          (list (stroke-open
                 (append (list (line-seg (- cx rx) (p :x) (- lf fx) cy))
                         (arc-path lf cy fx fy 180 360)
                         (list (line-seg (+ lf fx) cy cx mid))))
                (stroke-open
                 (append (list (line-seg cx mid (- rf fx) cy))
                         (arc-path rf cy fx fy 180 360)
                         (list (line-seg (+ rf fx) cy (+ cx rx) (p :x))))))))
      (with-pen-scale ((crowded 0.95d0))
        ;; All four strokes run at the SAME angle.  Placed by eye the
        ;; outer pair ran at 0.54*rx/x and the inner pair at 0.657,
        ;; so no two strokes in the letter were parallel and w read as
        ;; a cramped zigzag rather than two v's.  Matching the outer
        ;; run to the inner one fixes the vertex position exactly:
        ;;   (rx - v)/x = v/apex   =>   v = apex*rx / (x + apex)
        (let* ((rx (bowl-rx)) (cx (centre-x))
               (apex (* (p :x) 0.75))
               (v (/ (* apex rx) (+ (p :x) apex))))
          (list (diag (- cx rx) (p :x) (- cx v) 0)
                (diag (- cx v) 0 cx apex :trim :start)
                (diag cx apex (+ cx v) 0 :trim :end)
                (diag (+ cx v) 0 (+ cx rx) (p :x)))))))

(defglyph "x" 120
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (diag (- cx rx) (p :x) (+ cx rx) 0)
          (diag (+ cx rx) (p :x) (- cx rx) 0))))

(defglyph "y" 121
  (if (p :italic)
      ;; Two arms meeting at a vertex, with the right one carrying on
      ;; into a tail that sweeps left.  Built as a trough -- which is
      ;; what this was -- the right side never rises past mid height, so
      ;; there is no V at all and the letter reads as a u with a tail.
      ;;
      ;; The tail is a CUBIC rather than an arc so it can leave the
      ;; vertex along the arm's own direction and still arrive flat at
      ;; the descender: its first control point continues the arm by the
      ;; same slope, which is what makes the bend continuous instead of
      ;; a corner hidden inside the stroke width.
      (let* ((rx (bowl-rx)) (cx (centre-x))
             (bx (+ cx rx))
             (vx (+ cx (* rx 0.18)))            ; vertex, as in the upright
             (ey (+ (p :desc) (/ (p :thin) 2))) ; tail ink stops at the descender
             (ex (- cx (* rx 0.50)))
             (k 0.26))
        (list (diag (- cx rx) (p :x) vx 0)
              (stroke-open
               (list (line-seg bx (p :x) vx 0)
                     (seg vx 0
                          (+ vx (* k (- vx bx))) (* (- k) (p :x))
                          (+ ex (* 0.45 (- vx ex))) ey
                          ex ey)))))
      ;; Both ends of the descending arm are trimmed: its foot lands in
      ;; open air at the descender, not inside another stroke, so an
      ;; untrimmed butt cap just hangs below the line -- y was reaching
      ;; -507 where every other descender stopped at -471.
      (let ((rx (bowl-rx)) (cx (centre-x)))
        (list (diag (- cx rx) (p :x) (+ cx (* rx 0.18)) 0)
              (diag (+ cx rx) (p :x) (- cx (* rx 0.55)) (p :desc))))))

(defglyph "z" 122
  (let ((rx (bowl-rx)) (cx (centre-x)) (h (/ (p :thin) 2)))
    (list ;; bars placed so their ink lands on the lines, not centred on them
          (stroke-open (list (line-seg (- cx rx) (- (p :x) h) (+ cx rx) (- (p :x) h))))
          (diag (+ cx rx) (p :x) (- cx rx) 0)
          (stroke-open (list (line-seg (- cx rx) h (+ cx rx) h))))))

(defglyph "k" 107
  (let* ((rx (bowl-rx)) (cx (centre-x)) (knee (* (p :x) 0.42)))
    (list* (stroke-open (list (line-seg (- cx rx) 0 (- cx rx) (p :asc))))
           (if (p :italic)
               ;; The arm curves into the stem rather than striking it --
               ;; but it must still LAND on the stem at the knee, which
               ;; is where the leg departs.  Swept as a free arc it ended
               ;; in mid-air out to the right while the leg started back
               ;; at the stem, so the two never met and the arm closed
               ;; against the stem into something nearer a b.
               (list (stroke-open
                      (list (seg (+ cx rx) (p :x)
                                 (+ cx (* rx 0.35)) (* (p :x) 0.92)
                                 (- cx (* rx 0.30)) (* (p :x) 0.70)
                                 (- cx rx) knee)))
                     (diag (- cx rx) knee (+ cx rx) 0 :trim :end))
               (list (diag (+ cx rx) (p :x) (- cx rx) knee :trim :start)
                     (diag (- cx rx) knee (+ cx rx) 0 :trim :end))))))

;;; --- stem family: a stem that turns ---------------------------------------

(defglyph "t" 116
  :shift -17
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
  ;; In the italic f descends and hooks left -- the single reshaping
  ;; that JetBrains, Adwaita and most others all make.
  (let* ((cx (- (centre-x) (u 36)))
         (r (* (p :tail) 0.86))
         (cy (- (p :asc) r (/ (p :thin) 2)))
         (foot (if (p :italic) (+ (p :desc) r (/ (p :thin) 2)) 0)))
    (list (stroke-open
           (append (if (p :italic)
                       (list (line-seg cx cy cx foot))
                       (list (line-seg cx foot cx cy)))
                   (if (p :italic)
                       (arc-path (- cx r) foot r r 0 -110)
                       nil)))
          (stroke-open (arc-path (+ cx r) cy r r 180 35))
          (stroke-open (list (line-seg (- cx (u 204)) (p :x)
                                       (+ cx (u 228)) (p :x)))))))

(defglyph "j" 106
  :shift -45
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
  ;; Double-storey upright; SINGLE-STOREY in the italic, which is the
  ;; signature difference of a drawn italic and the first thing a reader
  ;; registers as "this is emphasised" rather than "this is leaning".
  (if (p :italic)
      (append (list (stroke-open (list (line-seg (+ (centre-x) (bowl-rx)) 0
                                                 (+ (centre-x) (bowl-rx)) (p :x)))))
              (stroke-closed (arc-path (centre-x) (bowl-cy) (bowl-rx)
                                       (bowl-ry) 0 360)))
      (let* ((rx (bowl-rx))
             ;; 0.30 squashed the upper counter flat.  The gap between
             ;; the bowl and the arm works out to x - 2*thin - 2*ry-b +
             ;; over, so the bowl's radius sets it directly: at 0.30
             ;; that gap was 15.2% of the x-height against a field of
             ;; 25.2-28.6%, barely half the narrowest of seven
             ;; references, while the bowl's own counter ran 46.1%
             ;; against their 32.2-38.9%.  One number was wrong in both
             ;; directions -- the bowl was eating the arm's room.
             ;; 0.24 puts both inside the field: upper 27.1%, lower 34.4%.
             (ry-b (* (p :x) 0.24))
             (cy-b (+ ry-b (/ (p :thin) 2) (- (p :over))))
             (ry-t (* (p :x) 0.21))
             (cy-t (- (p :x) ry-t (/ (p :thin) 2))))
        (append (list (stroke-open (list (line-seg (+ (centre-x) rx) 0
                                                   (+ (centre-x) rx) cy-t))))
                (stroke-closed (arc-path (centre-x) cy-b rx ry-b 0 360))
                (list (stroke-open (arc-path (centre-x) cy-t rx ry-t 0 168)))))))

(defglyph "g" 103
  ;; Single-storey: o's bowl, then the stem carries on below the line
  ;; and hooks left, the same turn j makes.
  ;; The tail radius sets how long the turn is: CY is pinned so the
  ;; bottom of the hook lands on the descender, so a larger radius
  ;; starts the curve higher and sweeps further left.  At 1.42 with a
  ;; quarter turn it read as a bend in the stem rather than a tail, and
  ;; g was separated from q by NOTHING: measured, the two had exactly
  ;; the same below-baseline ink width, 541 units each.  2.15 is the
  ;; longest turn the cell allows -- it brings the tail out to x=119,
  ;; which is the sidebearing exactly.
  (let* ((rx (bowl-rx))
         (r (* (p :tail) 2.15))
         (cy (+ (p :desc) r (/ (p :thin) 2))))
    (append (stroke-closed (arc-path (centre-x) (bowl-cy) rx (bowl-ry) 0 360))
            (list (stroke-open
                   (append (list (line-seg (+ (centre-x) rx) (p :x)
                                           (+ (centre-x) rx) cy))
                           (arc-path (- (+ (centre-x) rx) r) cy r r 0 -124)))))))

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

;;; ==========================================================================
;;; UPPERCASE
;;; ==========================================================================

(defun cap-cy () (/ (p :cap) 2))
(defun cap-ry () (- (+ (cap-cy) (p :over)) (/ (p :thin) 2)))

(defun hbar (y x0 x1 &key (align :centre) (scale 1d0))
  "A horizontal bar.  ALIGN :on puts its ink above Y, :under below, so
bars land on the cap line and the baseline instead of straddling them."
  (with-pen-scale (scale)
    (let* ((w (* (p :thin) scale))
           (yy (ecase align (:centre y) (:on (+ y (/ w 2))) (:under (- y (/ w 2))))))
      (stroke-open (list (line-seg x0 yy x1 yy))))))

(defun vstem (x y0 y1) (stroke-open (list (line-seg x y0 x y1))))

;;; --- round caps ------------------------------------------------------------

(defglyph "O" 79
  (stroke-closed (arc-path (centre-x) (cap-cy) (bowl-rx) (cap-ry) 0 360)))

(defglyph "Q" 81
  (append (stroke-closed (arc-path (centre-x) (cap-cy) (bowl-rx) (cap-ry) 0 360))
          (list (diag (+ (centre-x) (* (bowl-rx) 0.10)) (* (p :cap) 0.30)
                      (+ (centre-x) (bowl-rx)) (* (p :cap) -0.06) :trim nil))))

(defglyph "C" 67
  (let ((half (/ (p :aperture) 2)))
    (list (stroke-open (arc-path (centre-x) (cap-cy) (* (bowl-rx) 1.03) (cap-ry)
                                 half (- 360 half))))))

(defglyph "G" 71
  ;; The bar and spur have to land on the arc's TERMINALS, which sit at
  ;; rx*cos(half-aperture) -- not at the bowl's outer radius.  Putting
  ;; them at rx left the spur floating in the opening, touching nothing.
  (let* ((half (/ (p :aperture) 2))
         (rx (* (bowl-rx) 1.03))
         (xr (* rx (cos (deg half))))
         (y-low (+ (cap-cy) (* (cap-ry) (sin (deg (- half)))))))
    (list (stroke-open (arc-path (centre-x) (cap-cy) rx (cap-ry) half (- 360 half)))
          (vstem (+ (centre-x) xr) y-low (cap-cy))
          (hbar (cap-cy) (+ (centre-x) (* rx 0.14)) (+ (centre-x) xr)))))

(defglyph "S" 83
  (let* ((rx (* (bowl-rx) 0.95))
         (ry (/ (+ (p :cap) (* 2 (p :over)) (- (p :thin))) 4))
         (cy-u (/ (+ (p :cap) (* 2 ry)) 2))
         (cy-l (/ (- (p :cap) (* 2 ry)) 2)))
    (list (stroke-open (append (arc-path (centre-x) cy-u rx ry 10 270)
                               (arc-path (centre-x) cy-l rx ry 90 -170))))))

;;; --- stem and bowl ----------------------------------------------------------

(defun left-stem-x () (- (centre-x) (bowl-rx)))

(defglyph "D" 68
  ;; B D P R are FLAT-topped: all seven reference monospaces give them
  ;; exactly zero overshoot, aligning them with H E T.  Ours stood 36
  ;; per mille proud of the flat caps and hung 36 below the baseline,
  ;; three times the overshoot parameter and in the wrong places --
  ;; because the top and bottom strokes were drawn as CENTRELINES at
  ;; cap and at 0, so the pen laid half its width outside each.  A
  ;; horizontal stroke has to be inset by thin/2 to land ON a line;
  ;; that is what HBAR's :on and :under do for the glyphs that use it,
  ;; and these four could not use it because their bar runs
  ;; continuously into the bowl.
  (let* ((x0 (left-stem-x))
         (y1 (- (p :cap) (/ (p :thin) 2)))   ; ink tops out at cap
         (y0 (/ (p :thin) 2)))               ; ink bottoms out at baseline
    (list (vstem x0 0 (p :cap))
          (stroke-open (append (list (line-seg x0 y1 (centre-x) y1))
                               (arc-path (centre-x) (cap-cy) (bowl-rx)
                                         (- (cap-cy) (/ (p :thin) 2)) 90 -90)
                               (list (line-seg (centre-x) y0 x0 y0)))))))

(defglyph "P" 80
  (let* ((x0 (left-stem-x))
         (y1 (- (p :cap) (/ (p :thin) 2)))   ; see D
         ;; 0.262, not 0.27.  Lowering the bowl's apex by thin/2 to sit
         ;; on the cap line carried the waist down with it, to 38.4% of
         ;; cap against a field of 40.6-43.1 -- below all seven.  A
         ;; slightly shallower bowl puts the waist back at 42.5%.
         (ry (* (p :cap) 0.262))
         (waist (- y1 (* 2 ry))))
    (list (vstem x0 0 (p :cap))
          (stroke-open (append (list (line-seg x0 y1 (centre-x) y1))
                               (arc-path (centre-x) (- y1 ry) (bowl-rx) ry 90 -90)
                               (list (line-seg (centre-x) waist x0 waist)))))))

(defglyph "B" 66
  ;; Each bowl is now derived from the two lines it joins, so the arc
  ;; lands exactly on them.  It did not before: the upper arc's nadir
  ;; sat at cap - 2*0.25cap = 0.50cap while the bar it ran into was at
  ;; 0.52cap, a step the pen had to paper over.  See D for the
  ;; cap-alignment half of this.
  (let* ((x0 (left-stem-x))
         (y1 (- (p :cap) (/ (p :thin) 2)))
         (y0 (/ (p :thin) 2))
         (mid (* (p :cap) 0.52)))
    (list (vstem x0 0 (p :cap))
          (stroke-open (append (list (line-seg x0 y1 (centre-x) y1))
                               (arc-path (centre-x) (/ (+ y1 mid) 2) (* (bowl-rx) 0.92)
                                         (/ (- y1 mid) 2) 90 -90)
                               (list (line-seg (centre-x) mid x0 mid))))
          (stroke-open (append (list (line-seg x0 mid (centre-x) mid))
                               (arc-path (centre-x) (/ (+ mid y0) 2) (bowl-rx)
                                         (/ (- mid y0) 2) 90 -90)
                               (list (line-seg (centre-x) y0 x0 y0)))))))

(defglyph "R" 82
  (let* ((x0 (left-stem-x))
         (y1 (- (p :cap) (/ (p :thin) 2)))   ; see D
         (ry (* (p :cap) 0.262))             ; see P
         (waist (- y1 (* 2 ry))))
    (list (vstem x0 0 (p :cap))
          (stroke-open (append (list (line-seg x0 y1 (centre-x) y1))
                               (arc-path (centre-x) (- y1 ry) (bowl-rx) ry 90 -90)
                               (list (line-seg (centre-x) waist x0 waist))))
          (diag (+ x0 (* (bowl-rx) 0.55)) waist (+ (centre-x) (bowl-rx)) 0 :trim :end))))

;;; --- stems and bars ----------------------------------------------------------

(defglyph "E" 69
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (* (bowl-rx) 0.88))))
    (list (vstem x0 0 (p :cap))
          (hbar (p :cap) x0 x1 :align :under)
          (hbar (* (p :cap) 0.52) x0 (- x1 (u 30)))
          (hbar 0 x0 x1 :align :on))))

(defglyph "F" 70
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (* (bowl-rx) 0.88))))
    (list (vstem x0 0 (p :cap))
          (hbar (p :cap) x0 x1 :align :under)
          (hbar (* (p :cap) 0.54) x0 (- x1 (u 30))))))

(defglyph "H" 72
  (list (vstem (left-stem-x) 0 (p :cap))
        (vstem (+ (centre-x) (bowl-rx)) 0 (p :cap))
        (hbar (* (p :cap) 0.52) (left-stem-x) (+ (centre-x) (bowl-rx)))))

(defglyph "I" 73
  ;; serifs, or a bare stem floats in the cell
  (list (vstem (centre-x) 0 (p :cap))
        (hbar (p :cap) (- (centre-x) (p :serif)) (+ (centre-x) (p :serif))
              :align :under :scale (p :serif-weight))
        (hbar 0 (- (centre-x) (p :serif)) (+ (centre-x) (p :serif))
              :align :on :scale (p :serif-weight))))

(defglyph "J" 74
  ;; No crossbar.  Rendered J from the seven monospaces on this machine
  ;; -- JetBrains Mono, Fira Code, Hack, Adwaita Mono, Noto Sans Mono,
  ;; Liberation Mono, Source Code Pro -- and not one has a top bar.
  ;; Ours did, because (p :serif) is documented as the arms on i j l,
  ;; narrow glyphs earning their width in a monospace.  J is not narrow:
  ;; given a real hook it is as wide as U, so it never needed to earn
  ;; anything, and the bar was simply the widest thing in the letter.
  ;; That is what made it read as a T.
  ;;
  ;; Taking the bar away leaves the hook to carry the width alone, so
  ;; the hook grew: the stem moves three-quarters of the way out to
  ;; where U's right stem stands, and the bowl sweeps back to where the
  ;; round letters end.  Removing the bar WITHOUT this would have left
  ;; J at 0.53 of the advance against a field of 0.61-0.75 -- the
  ;; narrowest J anywhere, floating in its cell.
  (let* ((cx (+ (centre-x) (* (bowl-rx) 0.75)))
         (rx (- cx (p :sidebearing) (/ (p :thin) 2)))
         (ry (* (p :cap) 0.34))
         (cy (+ ry (/ (p :thin) 2))))
    (list (stroke-open (cons (line-seg cx (p :cap) cx cy)
                             (arc-path (- cx rx) cy rx ry 0 -90))))))

(defglyph "L" 76
  (list (vstem (left-stem-x) 0 (p :cap))
        (hbar 0 (left-stem-x) (+ (centre-x) (* (bowl-rx) 0.88)) :align :on)))

(defglyph "T" 84
  (list (vstem (centre-x) 0 (p :cap))
        (hbar (p :cap) (- (centre-x) (bowl-rx)) (+ (centre-x) (bowl-rx)) :align :under)))

(defglyph "U" 85
  (let* ((rx (bowl-rx))
         (ry (* (p :cap) 0.32))
         (cy (+ ry (/ (p :thin) 2))))
    (list (stroke-open
           (append (list (line-seg (- (centre-x) rx) (p :cap) (- (centre-x) rx) cy))
                   (arc-path (centre-x) cy rx ry 180 360)
                   (list (line-seg (+ (centre-x) rx) cy (+ (centre-x) rx) (p :cap))))))))

;;; --- diagonal caps -------------------------------------------------------------

(defglyph "A" 65
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (diag (- cx rx) 0 cx (p :cap))
          (diag (+ cx rx) 0 cx (p :cap))
          (hbar (* (p :cap) 0.28) (- cx (* rx 0.62)) (+ cx (* rx 0.62))))))

(defglyph "V" 86
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (diag (- cx rx) (p :cap) cx 0) (diag (+ cx rx) (p :cap) cx 0))))

(defglyph "W" 87
  (with-pen-scale ((crowded 0.95d0))
    (let* ((rx (bowl-rx)) (cx (centre-x)) (mid (* (p :cap) 0.70)))
      (list (diag (- cx rx) (p :cap) (- cx (* rx 0.46)) 0)
            (diag (- cx (* rx 0.46)) 0 cx mid :trim :start)
            (diag cx mid (+ cx (* rx 0.46)) 0 :trim :end)
            (diag (+ cx (* rx 0.46)) 0 (+ cx rx) (p :cap))))))

(defglyph "X" 88
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (diag (- cx rx) (p :cap) (+ cx rx) 0) (diag (+ cx rx) (p :cap) (- cx rx) 0))))

(defglyph "Y" 89
  (let ((rx (bowl-rx)) (cx (centre-x)) (mid (* (p :cap) 0.46)))
    (list (diag (- cx rx) (p :cap) cx mid :trim :start)
          (diag (+ cx rx) (p :cap) cx mid :trim :start)
          (vstem cx 0 mid))))

(defglyph "Z" 90
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (hbar (p :cap) (- cx rx) (+ cx rx) :align :under)
          (diag (+ cx rx) (p :cap) (- cx rx) 0)
          (hbar 0 (- cx rx) (+ cx rx) :align :on))))

(defglyph "K" 75
  (let* ((rx (bowl-rx)) (cx (centre-x)) (knee (* (p :cap) 0.42)))
    (list (vstem (- cx rx) 0 (p :cap))
          (diag (+ cx rx) (p :cap) (- cx rx) knee :trim :start)
          (diag (- cx rx) knee (+ cx rx) 0 :trim :end))))

(defglyph "M" 77
  (with-pen-scale ((crowded 0.92d0))
    (let* ((rx (bowl-rx)) (cx (centre-x)) (vee (* (p :cap) 0.26)))
      (list (vstem (- cx rx) 0 (p :cap))
            (vstem (+ cx rx) 0 (p :cap))
            (diag (- cx rx) (p :cap) cx vee :trim :start)
            (diag (+ cx rx) (p :cap) cx vee :trim :start)))))

(defglyph "N" 78
  (with-pen-scale ((crowded 0.95d0))
    (let ((rx (bowl-rx)) (cx (centre-x)))
      (list (vstem (- cx rx) 0 (p :cap))
            (vstem (+ cx rx) 0 (p :cap))
            (diag (- cx rx) (p :cap) (+ cx rx) 0)))))

;;; ==========================================================================
;;; FIGURES -- cap height, slightly narrower than the round caps
;;; ==========================================================================

(defun fig-rx () (- (round-width) (u 6) (/ (p :stem) 2)))

(defglyph "two" 50
  ;; One stroke: over the top, down the right, then the diagonal away
  ;; to the baseline.  The diagonal starts at the arc's COMPUTED
  ;; endpoint -- it used to start at a guessed point short of it, which
  ;; left a step in the middle where the two were meant to join.
  (let* ((rx (fig-rx)) (cx (centre-x))
         ;; A figure's arch is FLATTER than it is wide.  At 0.27 of the
         ;; cap this was 393 x 387 -- a circle -- which read as a bulb,
         ;; and it sat 46 units above every other figure because the
         ;; centre was placed without allowing for the overshoot or the
         ;; stroke.
         (ry (* (p :cap) 0.21))
         (cy (- (+ (p :cap) (p :over)) ry (/ (p :thin) 2)))
         (a-end -42)
         (ex (+ cx (* rx (cos (deg a-end)))))
         (ey (+ cy (* ry (sin (deg a-end)))))
         (by (/ (p :thin) 2)))
    (list (stroke-open
           (append (arc-path cx cy rx ry 190 a-end)
                   (list (line-seg ex ey (- cx rx) (+ by (u 12))))))
          (hbar 0 (- cx rx) (+ cx rx) :align :on))))

(defglyph "three" 51
  ;; Two bowls that OVERLAP at the waist.  Centres are placed by the
  ;; closed form that fixed s, so the circles are tangent at mid-height;
  ;; each arc then runs 30 degrees past that point, and the union of the
  ;; two strokes forms the waist.
  ;;
  ;; The join is a 180-degree reversal whichever way it is cut -- the
  ;; upper arc arrives travelling left and the lower departs travelling
  ;; right -- and a cusp in a skeleton puts a spur in the stroke.
  ;; Running the arcs further past the tangent point makes it worse, not
  ;; better: tangent circles diverge on both sides of the touch.  So the
  ;; arcs stop just short and a disc of the local stroke width fills the
  ;; join, which is what a round cap would do.
  (let* ((rx (* (fig-rx) 0.96)) (cx (centre-x))
         (ry (/ (+ (p :cap) (* 2 (p :over)) (- (p :thin))) 4))
         (cy-u (/ (+ (p :cap) (* 2 ry)) 2))
         (cy-l (/ (- (p :cap) (* 2 ry)) 2)))
    (let* ((a 96)
           (wx (+ cx (* rx (cos (deg (- a))))))
           (wy (+ cy-u (* ry (sin (deg (- a)))))))
      (declare (ignorable wx wy))
      (list (stroke-open (arc-path cx cy-u rx ry 168 (- a)))
            (stroke-open (arc-path cx cy-l rx ry a -168))))))

(defglyph "four" 52
  (let* ((rx (fig-rx)) (cx (centre-x)) (bar (* (p :cap) 0.28)))
    (list (diag (+ cx (* rx 0.34)) (p :cap) (- cx rx) bar :trim :start)
          (hbar bar (- cx rx) (+ cx rx))
          (vstem (+ cx (* rx 0.34)) 0 (p :cap)))))

(defglyph "five" 53
  (let* ((rx (fig-rx)) (cx (centre-x)) (x0 (- cx rx))
         (ry (* (p :cap) 0.29))
         (cy (+ ry (/ (p :thin) 2) (- (p :over))))
         (waist (+ cy ry)))
    (list (hbar (p :cap) x0 (+ cx (* rx 0.86)) :align :under)
          (vstem x0 waist (p :cap))
          (stroke-open (cons (line-seg x0 waist cx waist)
                             (arc-path cx cy rx ry 90 -160))))))

(defglyph "six" 54
  (let* ((rx (fig-rx)) (cx (centre-x)) (ry (* (p :cap) 0.30)))
    (append (stroke-closed (arc-path cx (- ry (p :over)) rx
                                     (- ry (/ (p :thin) 2)) 0 360))
            (list (stroke-open (arc-path cx ry rx (* (p :cap) 0.671) 180 75))))))

(defglyph "seven" 55
  (let* ((rx (fig-rx)) (cx (centre-x)))
    (list (hbar (p :cap) (- cx rx) (+ cx rx) :align :under)
          (diag (+ cx rx) (p :cap) (- cx (* rx 0.30)) 0))))

(defglyph "eight" 56
  (let* ((rx (fig-rx)) (cx (centre-x))
         (ryl (* (p :cap) 0.29)) (ryu (* (p :cap) 0.24)))
    (append (stroke-closed (arc-path cx (- ryl (p :over)) rx
                                     (- ryl (/ (p :thin) 2)) 0 360))
            (stroke-closed (arc-path cx (- (+ (p :cap) (p :over)) ryu)
                                     (* rx 0.86) (- ryu (/ (p :thin) 2)) 0 360)))))

(defglyph "nine" 57
  (let* ((rx (fig-rx)) (cx (centre-x)) (ry (* (p :cap) 0.30)))
    (append (stroke-closed (arc-path cx (- (+ (p :cap) (p :over)) ry) rx
                                     (- ry (/ (p :thin) 2)) 0 360))
            (list (stroke-open (arc-path cx (- (p :cap) ry) rx (* (p :cap) 0.671) 0 -105))))))

;;; ==========================================================================
;;; PUNCTUATION AND SYMBOLS
;;; ==========================================================================

(defglyph "exclam" 33
  (list (stroke-open (list (line-seg (centre-x) (* (p :cap) 0.30)
                                     (centre-x) (p :cap))))
        (dot-at (centre-x) (p :dot))))

(defglyph "quotedbl" 34
  ;; Taller than they were: at 0.66 of the cap these measured 238 per
  ;; mille where the field runs 271-330, and short quotes read as specks
  ;; in a line of strings.
  (let ((d (u 95)))
    (list (vstem (- (centre-x) d) (* (p :cap) 0.57) (p :cap))
          (vstem (+ (centre-x) d) (* (p :cap) 0.57) (p :cap)))))

(defglyph "quotesingle" 39
  (list (vstem (centre-x) (* (p :cap) 0.57) (p :cap))))

(defglyph "numbersign" 35
  (let* ((cx (centre-x)) (rx (* (bowl-rx) 0.92)) (lean (u 48)))
    (list (diag (+ (- cx (* rx 0.38)) lean) (p :cap) (- (- cx (* rx 0.38)) lean) 0 :trim nil)
          (diag (+ (+ cx (* rx 0.38)) lean) (p :cap) (- (+ cx (* rx 0.38)) lean) 0 :trim nil)
          (hbar (* (p :cap) 0.66) (- cx rx) (+ cx rx))
          (hbar (* (p :cap) 0.30) (- cx rx) (+ cx rx)))))

(defglyph "dollar" 36
  (let* ((rx (* (bowl-rx) 0.84))
         (ry (/ (+ (p :cap) (* 2 (p :over)) (- (p :thin))) 4))
         (cy-u (/ (+ (p :cap) (* 2 ry)) 2))
         (cy-l (/ (- (p :cap) (* 2 ry)) 2)))
    (list (stroke-open (append (arc-path (centre-x) cy-u rx ry 10 270)
                               (arc-path (centre-x) cy-l rx ry 90 -170)))
          (vstem (centre-x) (u -40) (+ (p :cap) (u 40))))))

(defglyph "percent" 37
  (let* ((cx (centre-x)) (rx (bowl-rx)) (r (* (p :cap) 0.17)))
    (append (stroke-closed (arc-path (- cx (* rx 0.52)) (- (p :cap) r)
                                     (* r 0.92) (- r (/ (p :thin) 2)) 0 360))
            (stroke-closed (arc-path (+ cx (* rx 0.52)) r
                                     (* r 0.92) (- r (/ (p :thin) 2)) 0 360))
            (list (diag (+ cx rx) (p :cap) (- cx rx) 0)))))

(defglyph "ampersand" 38
  ;; Loop and bowl are WIDER than they are tall.  The previous attempt
  ;; had both taller than wide, which left the loop's counter narrower
  ;; than the stroke that made it -- so it filled in and the glyph read
  ;; as a blob with a line through it.
  (let* ((cx (centre-x)) (rx (bowl-rx)) (half (/ (p :stem) 2))
         (r1x (* rx 0.50)) (r1y (* (p :cap) 0.155))
         (cy1 (- (p :cap) r1y half))
         (r2x (* rx 0.80)) (r2y (* (p :cap) 0.235))
         (cy2 (+ r2y half)))
    (append
     (stroke-closed (arc-path (- cx (* rx 0.26)) cy1 r1x r1y 0 360))
     (list (stroke-open (arc-path (- cx (* rx 0.10)) cy2 r2x r2y 48 330)))
     (list (diag (- cx (* rx 0.44)) (* (p :cap) 0.60)
                 (+ cx (* rx 0.95)) (u 35) :trim :end)))))

(defglyph "parenleft" 40
  (list (stroke-open (arc-path (+ (centre-x) (u 150)) (* (p :cap) 0.40)
                               (u 224) (* (p :cap) 0.73) 118 242))))

(defglyph "parenright" 41
  (list (stroke-open (arc-path (- (centre-x) (u 150)) (* (p :cap) 0.40)
                               (u 224) (* (p :cap) 0.73) -62 62))))

(defglyph "bracketleft" 91
  (let ((x0 (- (centre-x) (u 90))) (x1 (+ (centre-x) (u 130)))
        (y0 (u -90)) (y1 (* (p :cap) 0.98)))
    (list (vstem x0 y0 y1) (hbar y1 x0 x1 :align :under) (hbar y0 x0 x1 :align :on))))

(defglyph "bracketright" 93
  (let ((x1 (+ (centre-x) (u 90))) (x0 (- (centre-x) (u 130)))
        (y0 (u -90)) (y1 (* (p :cap) 0.98)))
    (list (vstem x1 y0 y1) (hbar y1 x0 x1 :align :under) (hbar y0 x0 x1 :align :on))))

(defglyph "braceleft" 123
  ;; One continuous path -- two quarter turns at the ends and two at the
  ;; waist -- so the spine bends rather than showing six butt joints.
  (let* ((cx (centre-x)) (r (u 125))
         (top (* (p :cap) 0.98)) (bot (u -80))
         (mid (/ (+ top bot) 2)))
    (list (stroke-open
           (append (arc-path (+ cx r) (- top r) r r 90 180)
                   (list (line-seg cx (- top r) cx (+ mid r)))
                   (arc-path (- cx r) (+ mid r) r r 0 -90)
                   (arc-path (- cx r) (- mid r) r r 90 0)
                   (list (line-seg cx (- mid r) cx (+ bot r)))
                   (arc-path (+ cx r) (+ bot r) r r 180 270))))))

(defglyph "braceright" 125
  (let* ((cx (centre-x)) (r (u 125))
         (top (* (p :cap) 0.98)) (bot (u -80))
         (mid (/ (+ top bot) 2)))
    (list (stroke-open
           (append (arc-path (- cx r) (- top r) r r 90 0)
                   (list (line-seg cx (- top r) cx (+ mid r)))
                   (arc-path (+ cx r) (+ mid r) r r 180 270)
                   (arc-path (+ cx r) (- mid r) r r 90 180)
                   (list (line-seg cx (- mid r) cx (+ bot r)))
                   (arc-path (- cx r) (+ bot r) r r 0 -90))))))

(defglyph "asterisk" 42
  (let ((cx (centre-x)) (cy (* (p :cap) 0.74)) (r (u 190)))
    (loop for k from 0 below 3
          collect (let ((a (deg (+ 90 (* k 60)))))
                    (diag (- cx (* r (cos a))) (- cy (* r (sin a)))
                          (+ cx (* r (cos a))) (+ cy (* r (sin a))) :trim nil)))))

(defglyph "plus" 43
  (let ((cy (* (p :cap) 0.46)) (r (u 215)))
    (list (hbar cy (- (centre-x) r) (+ (centre-x) r))
          (vstem (centre-x) (- cy r) (+ cy r)))))


(defglyph "equal" 61
  (let ((cy (* (p :cap) 0.46)) (r (u 215)) (g (u 105)))
    (list (hbar (+ cy g) (- (centre-x) r) (+ (centre-x) r))
          (hbar (- cy g) (- (centre-x) r) (+ (centre-x) r)))))

(defglyph "less" 60
  (let ((cy (* (p :cap) 0.46)) (r (u 215)))
    (list (diag (+ (centre-x) r) (+ cy r) (- (centre-x) r) cy :trim nil)
          (diag (- (centre-x) r) cy (+ (centre-x) r) (- cy r) :trim nil))))

(defglyph "greater" 62
  (let ((cy (* (p :cap) 0.46)) (r (u 215)))
    (list (diag (- (centre-x) r) (+ cy r) (+ (centre-x) r) cy :trim nil)
          (diag (+ (centre-x) r) cy (- (centre-x) r) (- cy r) :trim nil))))

(defglyph "comma" 44
  (let ((r (* (p :tail) 0.5)))
    (list (stroke-open (list (line-seg (+ (centre-x) (u 40)) (* (p :x) 0.26)
                                       (- (centre-x) (u 70)) (u -150)))))))

(defglyph "colon" 58
  (list (dot-at (centre-x) (p :dot))
        (dot-at (centre-x) (* (p :x) 0.62))))

(defglyph "semicolon" 59
  (list (dot-at (centre-x) (* (p :x) 0.62))
        (stroke-open (list (line-seg (+ (centre-x) (u 40)) (* (p :x) 0.26)
                                     (- (centre-x) (u 70)) (u -150))))))

(defglyph "question" 63
  ;; The stem starts at the arc's COMPUTED terminal.  It used to start
  ;; at a guessed point well away from it, leaving the hook and the stem
  ;; as two pieces crossing near each other.
  (let* ((cx (centre-x)) (rx (* (bowl-rx) 0.78)) (ry (* (p :cap) 0.22))
         (cy (- (p :cap) ry)) (a -38)
         (ex (+ cx (* rx (cos (deg a))))) (ey (+ cy (* ry (sin (deg a))))))
    (list (stroke-open (append (arc-path cx cy rx ry 180 a)
                               (list (line-seg ex ey cx (* (p :cap) 0.34)))))
          (dot-at cx (p :dot)))))

(defglyph "at" 64
  ;; An outer ring left OPEN at the lower right, around an inner bowl
  ;; big enough to keep a counter.  A full-circle outer and a bowl
  ;; narrower than the stroke made this a filled blob.
  ;; The at-sign packs a ring round a bowl round a counter, so it takes
  ;; the crowding allowance too.
  (with-pen-scale ((crowded 0.80d0))
   (let* ((cx (centre-x)) (rx (bowl-rx)) (ry (* (p :cap) 0.44))
          (cy (* (p :cap) 0.46))
          (irx (* rx 0.46)) (iry (* ry 0.50)))
    (append (list (stroke-open (arc-path cx cy rx ry -28 268)))
            (stroke-closed (arc-path cx cy irx iry 0 360))
            ;; the inner bowl's right wall, carried down and out
            (list (stroke-open
                   (append (list (line-seg (+ cx irx) (+ cy iry) (+ cx irx) (- cy iry)))
                           (arc-path (+ cx (* irx 0.1)) (- cy iry)
                                     (* irx 0.9) (* iry 0.5) 0 -70))))))))

(defglyph "slash" 47
  (list (diag (- (centre-x) (u 200)) (u -90) (+ (centre-x) (u 200)) (p :cap))))

(defglyph "backslash" 92
  (list (diag (+ (centre-x) (u 200)) (u -90) (- (centre-x) (u 200)) (p :cap))))

(defglyph "bar" 124
  (list (vstem (centre-x) (u -110) (* (p :cap) 1.02))))

(defglyph "underscore" 95
  (list (hbar (u -180) (- (centre-x) (/ (p :advance) 2))
              (+ (centre-x) (/ (p :advance) 2)) :align :on)))

(defglyph "asciicircum" 94
  (let ((r (u 200)) (cy (* (p :cap) 0.62)))
    (list (diag (- (centre-x) r) cy (centre-x) (p :cap))
          (diag (+ (centre-x) r) cy (centre-x) (p :cap)))))

(defglyph "grave" 96
  (list (diag (- (centre-x) (u 110)) (p :cap) (+ (centre-x) (u 60)) (* (p :cap) 0.74))))

(defglyph "asciitilde" 126
  (let* ((cx (centre-x)) (r (u 215)) (cy (* (p :cap) 0.46)) (h (u 85)))
    (list (stroke-open (append (arc-path (- cx (/ r 2)) cy (/ r 2) h 180 0)
                               (arc-path (+ cx (/ r 2)) cy (/ r 2) h 180 360))))))

;;; --- .notdef ----------------------------------------------------------
;;;
;;; Without this, fontmake synthesises a .notdef at upm/2 -- an advance
;;; no other glyph in the font has.  Since fixed pitch is derived from
;;; every advance matching, that one glyph is enough to stop the font
;;; declaring itself monospaced, which is the flag a font picker filters
;;; on.  Drawn rather than left blank so an unmapped codepoint shows a
;;; box instead of a hole.

(defglyph ".notdef" nil
  :no-centre
  ;; P already scales a metric to the em; U would scale it a second time.
  (let* ((x0 (p :sidebearing))
         (x1 (- (p :advance) (p :sidebearing)))
         (y1 (p :cap))
         (w (p :thin)))
    (list (rect x0 0 x1 y1)
          (ensure-cw (rect (+ x0 w) w (- x1 w) (- y1 w))))))
