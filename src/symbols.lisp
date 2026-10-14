;;;; symbols.lisp -- arrows, comparisons, set and logic operators, and
;;;; the typographic marks prose needs.

(in-package #:muskako)

(defun math-cy () (* (p :cap) 0.46))
(defun math-r  () (u 215))

(defun arrow (dir)
  "An arrow of length 2r pointing DIR, with a barbed head."
  (let* ((cx (centre-x)) (cy (math-cy)) (r (math-r)) (h (u 125))
         ;; CASE, not ECASE: the (t ...) clause here is a default, and
         ;; ecase has none -- it reads t as just another key.
         (hx (case dir (:left (- cx r)) (:right (+ cx r)) (t cx)))
         (hy (case dir (:up (+ cy r)) (:down (- cy r)) (t cy))))
    (list* (ecase dir
             ((:left :right) (hbar cy (- cx r) (+ cx r)))
             ((:up :down) (vstem cx (- cy r) (+ cy r))))
           (ecase dir
             (:right (list (diag (- hx h) (+ cy h) hx cy :trim nil)
                           (diag (- hx h) (- cy h) hx cy :trim nil)))
             (:left  (list (diag (+ hx h) (+ cy h) hx cy :trim nil)
                           (diag (+ hx h) (- cy h) hx cy :trim nil)))
             (:up    (list (diag (- cx h) (- hy h) cx hy :trim nil)
                           (diag (+ cx h) (- hy h) cx hy :trim nil)))
             (:down  (list (diag (- cx h) (+ hy h) cx hy :trim nil)
                           (diag (+ cx h) (+ hy h) cx hy :trim nil)))))))

(defglyph "arrowright" 8594 (arrow :right))
(defglyph "arrowleft"  8592 (arrow :left))
(defglyph "arrowup"    8593 (arrow :up))
(defglyph "arrowdown"  8595 (arrow :down))

(defglyph "arrowboth" 8596
  (let* ((cx (centre-x)) (cy (math-cy)) (r (math-r)) (h (u 115)))
    (list (hbar cy (- cx r) (+ cx r))
          (diag (- (+ cx r) h) (+ cy h) (+ cx r) cy :trim nil)
          (diag (- (+ cx r) h) (- cy h) (+ cx r) cy :trim nil)
          (diag (+ (- cx r) h) (+ cy h) (- cx r) cy :trim nil)
          (diag (+ (- cx r) h) (- cy h) (- cx r) cy :trim nil))))

(defglyph "arrowdblright" 8658
  (let* ((cx (centre-x)) (cy (math-cy)) (r (math-r)) (h (u 125)) (g (u 80)))
    (list (hbar (+ cy g) (- cx r) (+ cx (* r 0.3)))
          (hbar (- cy g) (- cx r) (+ cx (* r 0.3)))
          (diag (- (+ cx r) h) (+ cy h) (+ cx r) cy :trim nil)
          (diag (- (+ cx r) h) (- cy h) (+ cx r) cy :trim nil))))

;;; --- comparison ----------------------------------------------------------------

(defun rel-bars (ys &optional extra)
  (let ((r (math-r)))
    (append (mapcar (lambda (y) (hbar y (- (centre-x) r) (+ (centre-x) r))) ys)
            extra)))

(defglyph "equal2" 8801 (rel-bars (list (+ (math-cy) (u 150)) (math-cy)
                                        (- (math-cy) (u 150)))))

(defglyph "notequal" 8800
  (let ((r (math-r)) (g (u 105)))
    (append (rel-bars (list (+ (math-cy) g) (- (math-cy) g)))
            (list (diag (- (centre-x) (u 110)) (- (math-cy) (u 250))
                        (+ (centre-x) (u 110)) (+ (math-cy) (u 250)) :trim nil)))))

(defun lessequal (flip)
  (let* ((cx (centre-x)) (cy (+ (math-cy) (u 60))) (r (math-r)) (s (if flip -1 1)))
    (list (diag (+ cx (* s r)) (+ cy r) (- cx (* s r)) cy :trim nil)
          (diag (- cx (* s r)) cy (+ cx (* s r)) (- cy r) :trim nil)
          (hbar (- cy r (u 110)) (- cx r) (+ cx r)))))

(defglyph "lessequal" 8804 (lessequal nil))
(defglyph "greaterequal" 8805 (lessequal t))

(defglyph "approxequal" 8776
  (let* ((cx (centre-x)) (r (math-r)) (h (u 62)) (g (u 115)))
    (list (stroke-open (append (arc-path (- cx (/ r 2)) (+ (math-cy) g) (/ r 2) h 180 0)
                               (arc-path (+ cx (/ r 2)) (+ (math-cy) g) (/ r 2) h 180 360)))
          (stroke-open (append (arc-path (- cx (/ r 2)) (- (math-cy) g) (/ r 2) h 180 0)
                               (arc-path (+ cx (/ r 2)) (- (math-cy) g) (/ r 2) h 180 360))))))

;;; --- set and logic ---------------------------------------------------------------

(defun wedge (up)
  (let* ((cx (centre-x)) (r (math-r)) (lo (* (p :cap) 0.14)) (hi (* (p :cap) 0.80)))
    (if up
        (list (diag (- cx r) lo cx hi) (diag (+ cx r) lo cx hi))
        (list (diag (- cx r) hi cx lo) (diag (+ cx r) hi cx lo)))))

(defglyph "logicaland" 8743 (wedge t))
(defglyph "logicalor"  8744 (wedge nil))

(defun cupcap (up)
  (let* ((cx (centre-x)) (r (math-r)) (ry (* (p :cap) 0.30))
         (cy (if up (+ (* (p :cap) 0.16) ry) (- (* (p :cap) 0.80) ry))))
    (list (stroke-open
           (append (list (line-seg (- cx r) (if up (* (p :cap) 0.80) (* (p :cap) 0.16))
                                   (- cx r) cy))
                   (arc-path cx cy r ry (if up 180 180) (if up 360 0))
                   (list (line-seg (+ cx r) cy
                                   (+ cx r) (if up (* (p :cap) 0.80) (* (p :cap) 0.16)))))))))

(defglyph "union" 8746 (cupcap t))
(defglyph "intersection" 8745 (cupcap nil))

(defglyph "element" 8712
  (let ((half (/ (p :aperture) 2)) (r (* (bowl-rx) 0.84)))
    (list (stroke-open (arc-path (centre-x) (math-cy) r (* (p :cap) 0.34)
                                 (- 180 (- 180 half)) (+ 180 (- 180 half))))
          (hbar (math-cy) (- (centre-x) (* r 0.86)) (+ (centre-x) (* r 0.5))))))

(defglyph "emptyset" 8709
  (append (stroke-closed (arc-path (centre-x) (cap-cy) (* (bowl-rx) 0.84)
                                   (* (cap-ry) 0.86) 0 360))
          (list (diag (- (centre-x) (bowl-rx)) (u -30)
                      (+ (centre-x) (bowl-rx)) (+ (p :cap) (u 30)) :trim nil))))

(defglyph "universal" 8704
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (diag (- cx rx) (p :cap) cx 0) (diag (+ cx rx) (p :cap) cx 0)
          (hbar (* (p :cap) 0.34) (- cx (* rx 0.62)) (+ cx (* rx 0.62))))))

(defglyph "existential" 8707
  (let ((rx (* (bowl-rx) 0.8)) (cx (centre-x)))
    (list (hbar (p :cap) (- cx rx) (+ cx rx) :align :under)
          (hbar (* (p :cap) 0.5) (- cx rx) (+ cx rx))
          (hbar 0 (- cx rx) (+ cx rx) :align :on)
          (vstem (+ cx rx) 0 (p :cap)))))

;;; --- maths ------------------------------------------------------------------------

(defglyph "infinity" 8734
  ;; Wide and flat.  Every font in the comparison draws infinity about
  ;; one and a half times wider than tall; ours was square, which is why
  ;; it read as undersized next to the inequalities.
  (with-pen-scale (0.70d0)
    (let ((rx (u 110)) (ry (u 138)) (d (u 105)) (cy (u 330)))
      (append (stroke-closed (arc-path (- (centre-x) d) cy rx ry 0 360))
              (stroke-closed (arc-path (+ (centre-x) d) cy rx ry 0 360))))))

(defglyph "radical" 8730
  ;; Taller and wider: the field runs 523-586 by 730-976 where ours was
  ;; 430 by 612.
  (let* ((cx (centre-x)) (rx (- (/ (p :advance) 2) (u 50)))
         (top (u 780)) (dip (u 40)))
    (list (diag (- cx rx) (u 330) (- cx (* rx 0.52)) dip :trim nil)
          (diag (- cx (* rx 0.52)) dip (- cx (* rx 0.14)) top :trim nil)
          (hbar top (- cx (* rx 0.14)) (+ cx rx) :align :under))))

(defglyph "summation" 8721
  (let ((rx (* (bowl-rx) 0.86)) (cx (centre-x)))
    (list (hbar (p :cap) (- cx rx) (+ cx rx) :align :under)
          (diag (- cx rx) (p :cap) (+ cx (* rx 0.48)) (* (p :cap) 0.5) :trim :start)
          (diag (+ cx (* rx 0.48)) (* (p :cap) 0.5) (- cx rx) 0 :trim :end)
          (hbar 0 (- cx rx) (+ cx rx) :align :on))))

(defglyph "product" 8719
  (let ((rx (* (bowl-rx) 0.86)) (cx (centre-x)))
    (list (hbar (p :cap) (- cx rx) (+ cx rx) :align :under)
          (vstem (- cx (* rx 0.62)) 0 (p :cap))
          (vstem (+ cx (* rx 0.62)) 0 (p :cap)))))

(defglyph "integral" 8747
  (let ((r (* (p :tail) 1.0)) (cx (centre-x)))
    (list (stroke-open
           (append (arc-path (+ cx r) (- (p :asc) r) r r 180 80)
                   (list (line-seg cx (- (p :asc) r) cx (+ (p :desc) r)))
                   (arc-path (- cx r) (+ (p :desc) r) r r 0 -100))))))

(defglyph "partialdiff" 8706
  (append (stroke-closed (arc-path (centre-x) (* (p :cap) 0.30) (* (bowl-rx) 0.86)
                                   (* (p :cap) 0.30) 0 360))
          (list (stroke-open
                 (arc-path (centre-x) (* (p :cap) 0.74) (* (bowl-rx) 0.8)
                           (* (p :cap) 0.28) 200 20)))))

;;; --- typographic ---------------------------------------------------------------------

(defglyph "endash" 8211
  (list (hbar (* (p :x) 0.46) (- (centre-x) (u 200)) (+ (centre-x) (u 200)))))

(defglyph "emdash" 8212
  (list (hbar (* (p :x) 0.46) 0 (p :advance))))

(defun quote-mark (n reversed high)
  (let* ((d (u 92)) (y (if high (* (p :cap) 0.98) (* (p :x) 0.28)))
         (xs (if (= n 1) (list (centre-x)) (list (- (centre-x) d) (+ (centre-x) d)))))
    (mapcar (lambda (x)
              (if reversed
                  (diag x (- y (u 150)) (+ x (u 52)) y :trim nil)
                  (diag (+ x (u 52)) (- y (u 150)) x y :trim nil)))
            xs)))

(defglyph "quoteleft"      8216 (quote-mark 1 t   t))
(defglyph "quoteright"     8217 (quote-mark 1 nil t))
(defglyph "quotedblleft"   8220 (quote-mark 2 t   t))
(defglyph "quotedblright"  8221 (quote-mark 2 nil t))
(defglyph "quotesinglbase" 8218 (quote-mark 1 nil nil))
(defglyph "quotedblbase"   8222 (quote-mark 2 nil nil))

(defglyph "ellipsis" 8230
  (let ((d (u 190)) (r (* (p :dot) 0.86)))
    (list (dot-at (- (centre-x) d) r r) (dot-at (centre-x) r r)
          (dot-at (+ (centre-x) d) r r))))

(defglyph "bullet" 8226
  (list (dot-at (centre-x) (* (p :x) 0.52) (* (p :dot) 1.35))))

(defglyph "dagger" 8224
  (list (vstem (centre-x) (u -60) (p :cap))
        (hbar (* (p :cap) 0.74) (- (centre-x) (u 170)) (+ (centre-x) (u 170)))))

(defglyph "daggerdbl" 8225
  (list (vstem (centre-x) (u -60) (p :cap))
        (hbar (* (p :cap) 0.78) (- (centre-x) (u 170)) (+ (centre-x) (u 170)))
        (hbar (* (p :cap) 0.22) (- (centre-x) (u 170)) (+ (centre-x) (u 170)))))

(defglyph "guilsinglleft" 8249
  (let ((cy (* (p :x) 0.52)) (r (u 130)) (d (u 70)))
    (list (diag (+ (centre-x) d) (+ cy r) (- (centre-x) d) cy :trim nil)
          (diag (- (centre-x) d) cy (+ (centre-x) d) (- cy r) :trim nil))))

(defglyph "guilsinglright" 8250
  (let ((cy (* (p :x) 0.52)) (r (u 130)) (d (u 70)))
    (list (diag (- (centre-x) d) (+ cy r) (+ (centre-x) d) cy :trim nil)
          (diag (+ (centre-x) d) cy (- (centre-x) d) (- cy r) :trim nil))))

(defglyph "perthousand" 8240
  (let* ((cx (centre-x)) (rx (bowl-rx)) (r (* (p :cap) 0.15)))
    (append (stroke-closed (arc-path (- cx (* rx 0.62)) (- (p :cap) r)
                                     (* r 0.9) (- r (/ (p :thin) 2)) 0 360))
            (stroke-closed (arc-path (+ cx (* rx 0.10)) r
                                     (* r 0.9) (- r (/ (p :thin) 2)) 0 360))
            (stroke-closed (arc-path (+ cx (* rx 0.72)) r
                                     (* r 0.9) (- r (/ (p :thin) 2)) 0 360))
            (list (diag (+ cx rx) (p :cap) (- cx rx) 0)))))

;;; --- aliases -------------------------------------------------------------
;;;
;;; Distinct codepoints that share a shape.  Unicode keeps them apart;
;;; a font that only encodes one leaves the other as tofu, which is how
;;; U+2206 went missing while Greek capital delta was present.

(defglyph "increment" 8710 (glyph-contours (find-glyph "Delta")))
(defglyph "Ohm" 8486 (glyph-contours (find-glyph "Omega")))
(defglyph "minus" 8722
  (list (hbar (u 322) (- (centre-x) (u 215)) (+ (centre-x) (u 215)))))
(defglyph "asciitilde.op" 8764
  (glyph-contours (find-glyph "asciitilde")))
