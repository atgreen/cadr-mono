;;;; boxes.lisp -- U+2500 box drawing and U+2580 block elements.
;;;;
;;;; These are not letters and do not sit in the letter band.  A rule
;;;; must run edge to edge across the FULL cell -- the advance
;;;; horizontally, ascent to descent vertically -- or it breaks where
;;;; one cell meets the next.  That break is the gap you see in lazygit
;;;; and tmux borders when a font gets this wrong.
;;;;
;;;; They are uniform weight, drawn as rectangles rather than with the
;;;; pen: a modulated rule would change thickness as it turned.

(in-package #:muskako)

(defun cell-left   () 0)
(defun cell-right  () (p :advance))
(defun cell-top    () (p :line-asc))
(defun cell-bottom () (p :line-desc))
(defun cell-mid-x  () (round (/ (p :advance) 2)))
(defun cell-mid-y  () (round (/ (+ (p :line-asc) (p :line-desc)) 2)))

(defun w-of (weight)
  (case weight ((nil :n) 0) (:h (round (* (p :rule) 1.85))) (t (p :rule))))

(defun dgap () (round (* (p :rule) 1.6)))

(defun hrule (y x0 x1 &optional (w (p :rule)))
  (rect x0 (- y (/ w 2)) x1 (+ y (/ w 2))))

(defun vrule (x y0 y1 &optional (w (p :rule)))
  (rect (- x (/ w 2)) y0 (+ x (/ w 2)) y1))

(defun put-box (code fn)
  (setf (gethash (format nil "uni~4,'0X" code) *glyph-builders*)
        (list code nil fn)))

;;; --- light and heavy junctions -------------------------------------------

(defun box-arms (up down left right)
  "Arms from the centre to each edge.  Each overruns the junction by
half the crossing rule's weight, so corners close square."
  (let* ((mx (cell-mid-x)) (my (cell-mid-y))
         (wu (w-of up)) (wd (w-of down)) (wl (w-of left)) (wr (w-of right))
         (wv (max wu wd)) (wh (max wl wr))
         (out '()))
    (when (plusp wu) (push (vrule mx (- my (/ wh 2)) (cell-top) wu) out))
    (when (plusp wd) (push (vrule mx (cell-bottom) (+ my (/ wh 2)) wd) out))
    (when (plusp wl) (push (hrule my (cell-left) (+ mx (/ wv 2)) wl) out))
    (when (plusp wr) (push (hrule my (- mx (/ wv 2)) (cell-right) wr) out))
    out))

(defmacro defbox (code up down left right)
  `(put-box ,code (lambda () (box-arms ,up ,down ,left ,right))))

;;; U+2500-2503 plain rules
(defbox #x2500 :n :n :l :l)   (defbox #x2501 :n :n :h :h)
(defbox #x2502 :l :l :n :n)   (defbox #x2503 :h :h :n :n)

;;; U+250C-251B corners.  Within each group of four the low bit is the
;;; horizontal arm's weight and the next the vertical's.
(macrolet ((corners (base up down left right)
             `(progn
                ,@(loop for i from 0 below 4
                        collect `(defbox (+ ,base ,i)
                                   ,(if (eq up :n) :n (if (logbitp 1 i) :h :l))
                                   ,(if (eq down :n) :n (if (logbitp 1 i) :h :l))
                                   ,(if (eq left :n) :n (if (logbitp 0 i) :h :l))
                                   ,(if (eq right :n) :n (if (logbitp 0 i) :h :l)))))))
  (corners #x250C :n :y :n :y)    ; down and right
  (corners #x2510 :n :y :y :n)    ; down and left
  (corners #x2514 :y :n :n :y)    ; up and right
  (corners #x2518 :y :n :y :n))   ; up and left

;;; U+251C-253B tees.  The two pairs of families do NOT share an
;;; enumeration, which is the kind of thing that looks fine until you
;;; set a table and find every T-joint wrong.
;;;
;;; The vertical-stem families (251C, 2524) run: none, branch, up,
;;; down, up+down, up+branch, down+branch, all.
;;; The horizontal-stem families (252C, 2534) are a plain bitfield:
;;; bit 0 left, bit 1 right, bit 2 stem.

(defparameter +vstem-tees+
  '((:l :l :l) (:l :l :h) (:h :l :l) (:l :h :l)
    (:h :h :l) (:h :l :h) (:l :h :h) (:h :h :h))
  "(up down branch) for each step of the 251C and 2524 families.")

(dotimes (i 8)
  (destructuring-bind (up down branch) (nth i +vstem-tees+)
    (put-box (+ #x251C i) (lambda () (box-arms up down :n branch)))
    (put-box (+ #x2524 i) (lambda () (box-arms up down branch :n)))))

(dotimes (i 8)
  (let ((left  (if (logbitp 0 i) :h :l))
        (right (if (logbitp 1 i) :h :l))
        (stem  (if (logbitp 2 i) :h :l)))
    (put-box (+ #x252C i) (lambda () (box-arms :n stem left right)))
    (put-box (+ #x2534 i) (lambda () (box-arms stem :n left right)))))

;;; U+253C-254B crosses.  Sixteen combinations, and not a bitfield
;;; either -- the heavy arms are added in a specific order.
(defparameter +crosses+
  '((:l :l :l :l) (:l :l :h :l) (:l :l :l :h) (:l :l :h :h)
    (:h :l :l :l) (:l :h :l :l) (:h :h :l :l) (:h :l :h :l)
    (:h :l :l :h) (:l :h :h :l) (:l :h :l :h) (:h :l :h :h)
    (:l :h :h :h) (:h :h :h :l) (:h :h :l :h) (:h :h :h :h))
  "(up down left right) for each step of the 253C family.")

(dotimes (i 16)
  (destructuring-bind (up down left right) (nth i +crosses+)
    (put-box (+ #x253C i) (lambda () (box-arms up down left right)))))

;;; --- dashes ----------------------------------------------------------------

(defun dash-run (n weight vertical)
  (let* ((w (w-of weight)) (mx (cell-mid-x)) (my (cell-mid-y))
         (a (if vertical (cell-bottom) (cell-left)))
         (b (if vertical (cell-top) (cell-right)))
         (pitch (/ (- b a) n)) (ink (* pitch 0.62)))
    (loop for i from 0 below n
          for s = (+ a (* i pitch) (/ (- pitch ink) 2))
          collect (if vertical
                      (vrule mx s (+ s ink) w)
                      (hrule my s (+ s ink) w)))))

(macrolet ((dash (code n weight vertical)
             `(put-box ,code (lambda () (dash-run ,n ,weight ,vertical)))))
  (dash #x2504 3 :l nil) (dash #x2505 3 :h nil)
  (dash #x2506 3 :l t)   (dash #x2507 3 :h t)
  (dash #x2508 4 :l nil) (dash #x2509 4 :h nil)
  (dash #x250A 4 :l t)   (dash #x250B 4 :h t)
  (dash #x254C 2 :l nil) (dash #x254D 2 :h nil)
  (dash #x254E 2 :l t)   (dash #x254F 2 :h t))

;;; --- double rules -----------------------------------------------------------
;;;
;;; Each double junction is spelled out.  A double corner is two L
;;; shapes -- outer rules meeting at the outer corner, inner at the
;;; inner one -- and drawing both arms full length instead would put a
;;; cross in the middle of every corner.

(defmacro defdouble (code &body body)
  `(put-box ,code (lambda ()
                    (let* ((mx (cell-mid-x)) (my (cell-mid-y))
                           (g (round (/ (dgap) 2)))
                           (h (round (/ (p :rule) 2)))
                           (l (cell-left)) (r (cell-right))
                           (tp (cell-top)) (bt (cell-bottom)))
                      (declare (ignorable mx my g h l r tp bt))
                      (list ,@body)))))

(defdouble #x2550 (hrule (+ my g) l r) (hrule (- my g) l r))
(defdouble #x2551 (vrule (- mx g) bt tp) (vrule (+ mx g) bt tp))

(defdouble #x2552 (hrule (+ my g) (- mx h) r) (hrule (- my g) (- mx h) r)
                  (vrule mx bt (+ my g h)))
(defdouble #x2553 (hrule my (- mx g h) r)
                  (vrule (- mx g) bt (+ my h)) (vrule (+ mx g) bt (+ my h)))
(defdouble #x2554 (hrule (+ my g) (- mx g) r) (hrule (- my g) (+ mx g) r)
                  (vrule (- mx g) bt (+ my g)) (vrule (+ mx g) bt (- my g)))

(defdouble #x2555 (hrule (+ my g) l (+ mx h)) (hrule (- my g) l (+ mx h))
                  (vrule mx bt (+ my g h)))
(defdouble #x2556 (hrule my l (+ mx g h))
                  (vrule (- mx g) bt (+ my h)) (vrule (+ mx g) bt (+ my h)))
(defdouble #x2557 (hrule (+ my g) l (+ mx g)) (hrule (- my g) l (- mx g))
                  (vrule (+ mx g) bt (+ my g)) (vrule (- mx g) bt (- my g)))

(defdouble #x2558 (hrule (+ my g) (- mx h) r) (hrule (- my g) (- mx h) r)
                  (vrule mx (- my g h) tp))
(defdouble #x2559 (hrule my (- mx g h) r)
                  (vrule (- mx g) (- my h) tp) (vrule (+ mx g) (- my h) tp))
(defdouble #x255A (hrule (+ my g) (+ mx g) r) (hrule (- my g) (- mx g) r)
                  (vrule (- mx g) (- my g) tp) (vrule (+ mx g) (+ my g) tp))

(defdouble #x255B (hrule (+ my g) l (+ mx h)) (hrule (- my g) l (+ mx h))
                  (vrule mx (- my g h) tp))
(defdouble #x255C (hrule my l (+ mx g h))
                  (vrule (- mx g) (- my h) tp) (vrule (+ mx g) (- my h) tp))
(defdouble #x255D (hrule (+ my g) l (- mx g)) (hrule (- my g) l (+ mx g))
                  (vrule (- mx g) (+ my g) tp) (vrule (+ mx g) (- my g) tp))

(defdouble #x255E (vrule mx bt tp) (hrule (+ my g) mx r) (hrule (- my g) mx r))
(defdouble #x255F (vrule (- mx g) bt tp) (vrule (+ mx g) bt tp) (hrule my (+ mx g) r))
(defdouble #x2560 (vrule (- mx g) bt tp)
                  (vrule (+ mx g) bt (- my g)) (vrule (+ mx g) (+ my g) tp)
                  (hrule (+ my g) (+ mx g) r) (hrule (- my g) (+ mx g) r))

(defdouble #x2561 (vrule mx bt tp) (hrule (+ my g) l mx) (hrule (- my g) l mx))
(defdouble #x2562 (vrule (- mx g) bt tp) (vrule (+ mx g) bt tp) (hrule my l (- mx g)))
(defdouble #x2563 (vrule (+ mx g) bt tp)
                  (vrule (- mx g) bt (- my g)) (vrule (- mx g) (+ my g) tp)
                  (hrule (+ my g) l (- mx g)) (hrule (- my g) l (- mx g)))

(defdouble #x2564 (hrule (+ my g) l r) (hrule (- my g) l r) (vrule mx bt (- my g)))
(defdouble #x2565 (hrule my l r) (vrule (- mx g) bt my) (vrule (+ mx g) bt my))
(defdouble #x2566 (hrule (+ my g) l r)
                  (hrule (- my g) l (- mx g)) (hrule (- my g) (+ mx g) r)
                  (vrule (- mx g) bt (- my g)) (vrule (+ mx g) bt (- my g)))

(defdouble #x2567 (hrule (+ my g) l r) (hrule (- my g) l r) (vrule mx (+ my g) tp))
(defdouble #x2568 (hrule my l r) (vrule (- mx g) my tp) (vrule (+ mx g) my tp))
(defdouble #x2569 (hrule (- my g) l r)
                  (hrule (+ my g) l (- mx g)) (hrule (+ my g) (+ mx g) r)
                  (vrule (- mx g) (+ my g) tp) (vrule (+ mx g) (+ my g) tp))

(defdouble #x256A (hrule (+ my g) l r) (hrule (- my g) l r) (vrule mx bt tp))
(defdouble #x256B (hrule my l r) (vrule (- mx g) bt tp) (vrule (+ mx g) bt tp))
(defdouble #x256C (hrule (+ my g) l (- mx g)) (hrule (+ my g) (+ mx g) r)
                  (hrule (- my g) l (- mx g)) (hrule (- my g) (+ mx g) r)
                  (vrule (- mx g) bt (- my g)) (vrule (- mx g) (+ my g) tp)
                  (vrule (+ mx g) bt (- my g)) (vrule (+ mx g) (+ my g) tp))

;;; --- rounded corners --------------------------------------------------------

(defun rounded (vdir hdir)
  "VDIR and HDIR are +1 or -1 for which way the arms leave the centre."
  (let* ((mx (cell-mid-x)) (my (cell-mid-y))
         (rr (* (p :rule) 3))
         (w (p :rule))
         (cx (+ mx (* hdir rr))) (cy (+ my (* vdir rr)))
         (a0 (if (plusp hdir) 180 0))
         (a1 (if (plusp vdir) (if (plusp hdir) 270 -90) (if (plusp hdir) 90 90))))
    (list (vrule mx (if (plusp vdir) (cell-bottom) (+ my rr))
                    (if (plusp vdir) (- my rr) (cell-top)))
          (hrule my (if (plusp hdir) (+ mx rr) (cell-left))
                    (if (plusp hdir) (cell-right) (- mx rr)))
          (arc-annulus cx cy (+ rr (/ w 2)) (+ rr (/ w 2)) w w a0 a1 :stress 0))))

;; The arcs sweep between the two straight arms; signs chosen per corner.
(put-box #x256D (lambda ()                       ; down and right
                  (let* ((mx (cell-mid-x)) (my (cell-mid-y))
                         (rr (* (p :rule) 3)) (w (p :rule)))
                    (list (vrule mx (cell-bottom) (- my rr))
                          (hrule my (+ mx rr) (cell-right))
                          (arc-annulus (+ mx rr) (- my rr)
                                       (+ rr (/ w 2)) (+ rr (/ w 2)) w w 180 90 :stress 0)))))
(put-box #x256E (lambda ()                       ; down and left
                  (let* ((mx (cell-mid-x)) (my (cell-mid-y))
                         (rr (* (p :rule) 3)) (w (p :rule)))
                    (list (vrule mx (cell-bottom) (- my rr))
                          (hrule my (cell-left) (- mx rr))
                          (arc-annulus (- mx rr) (- my rr)
                                       (+ rr (/ w 2)) (+ rr (/ w 2)) w w 0 90 :stress 0)))))
(put-box #x256F (lambda ()                       ; up and left
                  (let* ((mx (cell-mid-x)) (my (cell-mid-y))
                         (rr (* (p :rule) 3)) (w (p :rule)))
                    (list (vrule mx (+ my rr) (cell-top))
                          (hrule my (cell-left) (- mx rr))
                          (arc-annulus (- mx rr) (+ my rr)
                                       (+ rr (/ w 2)) (+ rr (/ w 2)) w w 0 -90 :stress 0)))))
(put-box #x2570 (lambda ()                       ; up and right
                  (let* ((mx (cell-mid-x)) (my (cell-mid-y))
                         (rr (* (p :rule) 3)) (w (p :rule)))
                    (list (vrule mx (+ my rr) (cell-top))
                          (hrule my (+ mx rr) (cell-right))
                          (arc-annulus (+ mx rr) (+ my rr)
                                       (+ rr (/ w 2)) (+ rr (/ w 2)) w w 180 270 :stress 0)))))

;;; --- diagonals ---------------------------------------------------------------

(put-box #x2571 (lambda () (list (stroke-between (cell-left) (cell-bottom)
                                                 (cell-right) (cell-top) (p :rule)))))
(put-box #x2572 (lambda () (list (stroke-between (cell-left) (cell-top)
                                                 (cell-right) (cell-bottom) (p :rule)))))
(put-box #x2573 (lambda () (list (stroke-between (cell-left) (cell-bottom)
                                                 (cell-right) (cell-top) (p :rule))
                                 (stroke-between (cell-left) (cell-top)
                                                 (cell-right) (cell-bottom) (p :rule)))))

;;; --- stubs and mixed rules -----------------------------------------------------

(macrolet ((stub (code dir weight)
             `(put-box ,code
                (lambda ()
                  (let ((mx (cell-mid-x)) (my (cell-mid-y)) (w (w-of ,weight)))
                    (list (ecase ,dir
                            (:left  (hrule my (cell-left) mx w))
                            (:right (hrule my mx (cell-right) w))
                            (:up    (vrule mx my (cell-top) w))
                            (:down  (vrule mx (cell-bottom) my w)))))))))
  (stub #x2574 :left :l) (stub #x2575 :up :l)
  (stub #x2576 :right :l) (stub #x2577 :down :l)
  (stub #x2578 :left :h) (stub #x2579 :up :h)
  (stub #x257A :right :h) (stub #x257B :down :h))

(defbox #x257C :n :n :l :h)   ; left light, right heavy
(defbox #x257D :l :h :n :n)   ; up light, down heavy
(defbox #x257E :n :n :h :l)
(defbox #x257F :h :l :n :n)

;;; --- block elements U+2580-259F --------------------------------------------------

(defun block-rect (x0 y0 x1 y1)
  "A fraction of the cell, in 0..1 coordinates from its bottom left."
  (let ((w (- (cell-right) (cell-left)))
        (h (- (cell-top) (cell-bottom))))
    (rect (+ (cell-left) (* x0 w)) (+ (cell-bottom) (* y0 h))
          (+ (cell-left) (* x1 w)) (+ (cell-bottom) (* y1 h)))))

(put-box #x2580 (lambda () (list (block-rect 0 1/2 1 1))))      ; upper half
(dotimes (i 7)                                                   ; lower eighths
  (let ((f (/ (1+ i) 8)))
    (put-box (+ #x2581 i) (lambda () (list (block-rect 0 0 1 f))))))
(put-box #x2588 (lambda () (list (block-rect 0 0 1 1))))         ; full
(dotimes (i 7)                                                   ; left eighths
  (let ((f (/ (- 7 i) 8)))
    (put-box (+ #x2589 i) (lambda () (list (block-rect 0 0 f 1))))))
(put-box #x2590 (lambda () (list (block-rect 1/2 0 1 1))))       ; right half
(put-box #x2594 (lambda () (list (block-rect 0 7/8 1 1))))       ; upper eighth
(put-box #x2595 (lambda () (list (block-rect 7/8 0 1 1))))       ; right eighth

(defun shade (fraction)
  "A dither at FRACTION coverage.  Terminals draw gradients with these,
so what matters is that the three steps read as distinct greys."
  (let ((n 8) (out '()))
    (dotimes (row n)
      (dotimes (col n)
        (when (< (mod (+ (* row 5) (* col 3)) 8) (* fraction 8))
          (push (block-rect (/ col n) (/ row n) (/ (1+ col) n) (/ (1+ row) n)) out))))
    out))

(put-box #x2591 (lambda () (shade 0.25)))
(put-box #x2592 (lambda () (shade 0.50)))
(put-box #x2593 (lambda () (shade 0.75)))

;;; quadrants U+2596-259F, each a combination of the four corners
(defparameter +quadrants+
  '((#x2596 (:bl)) (#x2597 (:br)) (#x2598 (:tl))
    (#x2599 (:tl :bl :br)) (#x259A (:tl :br)) (#x259B (:tl :tr :bl))
    (#x259C (:tl :tr :br)) (#x259D (:tr)) (#x259E (:tr :bl))
    (#x259F (:tr :bl :br))))

(dolist (entry +quadrants+)
  (destructuring-bind (code parts) entry
    (put-box code
             (lambda ()
               (mapcar (lambda (q)
                         (ecase q
                           (:tl (block-rect 0 1/2 1/2 1)) (:tr (block-rect 1/2 1/2 1 1))
                           (:bl (block-rect 0 0 1/2 1/2)) (:br (block-rect 1/2 0 1 1/2))))
                       parts)))))
