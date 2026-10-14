;;;; glyphs.lisp -- the letters, assembled from primitives.

(in-package #:muskako)

(defstruct glyph name code advance contours)

(defvar *glyph-builders* (make-hash-table :test #'equal))

(defmacro defglyph (name code &body body)
  "Define a glyph.  BODY runs under the current design and returns a
list of contours, so every glyph is a pure function of the parameters."
  `(setf (gethash ,name *glyph-builders*)
         (cons ,code (lambda () (progn ,@body)))))

(defun all-glyphs ()
  (let (out)
    (maphash (lambda (name entry)
               (push (make-glyph :name name :code (car entry)
                                 :advance (p :advance)
                                 :contours (funcall (cdr entry)))
                     out))
             *glyph-builders*)
    (sort out #'< :key (lambda (g) (or (glyph-code g) 0)))))

(defun find-glyph (name)
  (let ((e (gethash name *glyph-builders*)))
    (when e
      (make-glyph :name name :code (car e) :advance (p :advance)
                  :contours (funcall (cdr e))))))

;;; --- blank -----------------------------------------------------------

(defglyph "space" 32 nil)

;;; --- round letters ---------------------------------------------------

(defglyph "o" 111
  (ring (centre-x) (/ (p :x) 2) (round-width) (+ (/ (p :x) 2) (p :over))))

(defglyph "c" 99
  (let ((a (p :aperture)))
    (list (arc-annulus (centre-x) (/ (p :x) 2) (round-width)
                       (+ (/ (p :x) 2) (p :over))
                       (p :stem) (p :thin) (- 90 a) (+ 270 a)))))

(defglyph "e" 101
  (let* ((cy (/ (p :x) 2))
         (rx (round-width))
         (ry (+ cy (p :over)))
         (a (p :aperture)))
    (list (arc-annulus (centre-x) cy rx ry (p :stem) (p :thin)
                       (- (p :stress)) (+ 270 a))
          (bar-at cy (- (centre-x) rx) (+ (centre-x) (- rx (/ (p :stem) 3)))))))

;;; --- stems and shoulders ---------------------------------------------

(defglyph "n" 110
  (let* ((rx (round-width))
         (ry (* (p :x) (p :ease)))
         ;; Place the arch so its CROWN lands one overshoot above the
         ;; x-height, level with the flat top of the stem beside it.
         (cy (- (+ (p :x) (p :over)) ry (/ (p :thin) 2))))
    (list (stem-at (- (centre-x) rx) 0 (p :x))
          (stem-at (+ (centre-x) rx) 0 cy)
          (shoulder (centre-x) cy rx ry))))

(defglyph "i" 105
  (list (stem-at (centre-x) 0 (p :x))
        (dot-at (centre-x) (+ (p :x) 100 (p :dot)))))

(defglyph "l" 108
  (let* ((r (p :tail))                      ; centreline radius of the tail
         (half (/ (p :stem) 2))
         (cy (+ half r)))                   ; puts the tail's foot on the baseline
    (cons (stem-at (centre-x) (if (p :l-tail) cy 0) (p :asc))
          (when (p :l-tail)
            (list (arc-annulus (+ (centre-x) r) cy (+ r half) (+ r half)
                               (p :stem) (p :stem) 180 270 :stress 0))))))

;;; --- figures ----------------------------------------------------------

(defglyph "zero" 48
  (let* ((cy (/ (p :cap) 2))
         (rx (- (round-width) 6))
         (ry (+ cy (p :over))))
    (append (ring (centre-x) cy rx ry)
            (ecase (p :zero)
              (:none nil)
              (:dot (list (dot-at (centre-x) cy (* 0.62 (p :dot)))))
              (:slash (list (rect (- (centre-x) (/ (p :thin) 2)) (- cy (* ry 0.78))
                                  (+ (centre-x) (/ (p :thin) 2)) (+ cy (* ry 0.78)))))))))

(defglyph "one" 49
  (let ((cx (centre-x)) (h (p :cap)))
    (list* (stem-at cx 0 h)
           ;; the flag: a parallelogram off the top left of the stem
           (let ((w (* 1.6 (p :thin))))
             (list (pt :line (- cx (/ (p :stem) 2)) h)
                   (pt :line (- cx 170) (- h 178))
                   (pt :line (- cx 170) (- h 178 w))
                   (pt :line (- cx (/ (p :stem) 2)) (- h w))))
           (when (p :one-serif)
             (list (bar-at (/ (p :thin) 2) (- cx 150) (+ cx 150)))))))

;;; --- punctuation -------------------------------------------------------

(defglyph "period" 46 (list (dot-at (centre-x) (p :dot))))
(defglyph "hyphen" 45
  (list (bar-at (* 0.42 (p :x)) (- (centre-x) 150) (+ (centre-x) 150))))
