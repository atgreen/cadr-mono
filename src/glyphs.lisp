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
  (let ((a (p :aperture)))
    (list (stroke-open (arc-path (centre-x) (bowl-cy) (bowl-rx) (bowl-ry)
                                 (- 90 a) (+ 270 a))))))

(defglyph "e" 101
  (let* ((cy (bowl-cy)) (rx (bowl-rx)) (ry (bowl-ry)) (a (p :aperture)))
    (list
     ;; The bowl starts where the bar meets it on the right, runs over
     ;; the top, down the left, round the bottom and out at the lower
     ;; right -- the whole letter in one sweep.
     (stroke-open (arc-path (centre-x) cy rx ry 0 (+ 270 a)))
     (stroke-open (list (line-seg (- (centre-x) rx) cy (+ (centre-x) rx) cy))))))

;;; --- stems and shoulders -------------------------------------------------

(defglyph "n" 110
  (let* ((rx (bowl-rx))
         (ry (* (p :x) (p :ease)))
         (cy (- (p :x) ry (/ (p :thin) 2))))
    (list (stroke-open (list (line-seg (- (centre-x) rx) 0 (- (centre-x) rx) (p :x))))
          ;; shoulder and right leg are a single continuous path
          (stroke-open (append (arc-path (centre-x) cy rx ry 180 0)
                               (list (line-seg (+ (centre-x) rx) cy
                                               (+ (centre-x) rx) 0)))))))

(defglyph "i" 105
  (list (stroke-open (list (line-seg (centre-x) 0 (centre-x) (p :x))))
        (dot-at (centre-x) (- (p :cap) 14 (p :dot)))))

(defglyph "l" 108
  (let* ((r (p :tail))
         (cy (+ r (/ (p :thin) 2))))
    (list (stroke-open
           (if (p :l-tail)
               ;; stem and tail: one path, so the turn is a bend in a
               ;; stroke rather than a quarter-circle glued to a stick
               (cons (line-seg (centre-x) (p :asc) (centre-x) cy)
                     (arc-path (+ (centre-x) r) cy r r 180 270))
               (list (line-seg (centre-x) (p :asc) (centre-x) 0)))))))

;;; --- figures --------------------------------------------------------------

(defglyph "zero" 48
  (let* ((cy (/ (p :cap) 2))
         (rx (- (round-width) 6 (/ (p :stem) 2)))
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
            (list (stroke-open (list (line-seg cx (- h 26) (- cx 168) (- h 196)))))
            (when (p :one-serif)
              ;; centred on thin/2 so the serif sits ON the baseline
              ;; rather than straddling it
              (let ((y (/ (p :thin) 2)))
                (list (stroke-open (list (line-seg (- cx 150) y (+ cx 150) y)))))))))

;;; --- punctuation ------------------------------------------------------------

(defglyph "period" 46 (list (dot-at (centre-x) (p :dot))))
(defglyph "hyphen" 45
  (list (stroke-open (list (line-seg (- (centre-x) 150) (* 0.42 (p :x))
                                     (+ (centre-x) 150) (* 0.42 (p :x)))))))
