;;;; greek.lisp -- Greek, which in a coding font is mostly a maths
;;;; alphabet: lambda, mu, pi, sigma, delta, theta, epsilon.

(in-package #:muskako)

(defun gx () (bowl-rx))
(defun gcy () (bowl-cy))
(defun gry () (bowl-ry))

;;; --- lowercase ---------------------------------------------------------------

(defglyph "alpha" 945
  (let ((rx (* (gx) 0.78)) (cx (- (centre-x) (u 40))))
    (append (stroke-closed (arc-path cx (gcy) rx (gry) 0 360))
            (list (stroke-open
                   (append (arc-path cx (gcy) rx (gry) 60 -60)
                           (list (line-seg (+ cx (* rx 0.5)) (* (p :x) 0.14)
                                           (+ (centre-x) (gx)) 0))))))))

(defglyph "beta" 946
  ;; Two D-shaped bowls hung off one stem.  Each arc is centred ON the
  ;; stem and swept from 90 to -90, so both ends land on it by
  ;; construction -- the previous pair were placed by eye and floated
  ;; clear of the stem entirely.
  (let* ((x0 (- (centre-x) (gx)))
         (top (* (p :x) 1.34))
         (mid (* (p :x) 0.56))
         (rx (* (gx) 1.5)))
    (list (vstem x0 (p :desc) top)
          (stroke-open (arc-path x0 (/ (+ top mid) 2) rx (/ (- top mid) 2) 90 -90))
          (stroke-open (arc-path x0 (/ mid 2) rx (/ mid 2) 90 -90)))))

(defglyph "gamma" 947
  (let ((rx (gx)) (cx (centre-x)))
    (list (diag (- cx rx) (p :x) (+ cx (* rx 0.18)) (p :desc))
          (diag (+ cx rx) (p :x) (- cx (* rx 0.22)) (* (p :x) 0.22) :trim :start))))

(defglyph "delta" 948
  (let ((rx (* (gx) 0.88)) (cx (centre-x)))
    (append (stroke-closed (arc-path cx (* (p :x) 0.34) rx (* (p :x) 0.34) 0 360))
            (list (stroke-open
                   (append (arc-path cx (* (p :x) 0.72) (* rx 0.72) (* (p :x) 0.28) 300 160)
                           (list (line-seg (- cx (* rx 0.55)) (* (p :x) 0.86)
                                           (+ cx (* rx 0.75)) (p :x)))))))))

(defglyph "epsilon" 949
  ;; The 3 construction mirrored: two arcs tangent at mid-height, each
  ;; running a little past the touch so their union forms the waist.
  (let* ((rx (* (gx) 0.86))
         (ry (/ (+ (p :x) (* 2 (p :over)) (- (p :thin))) 4))
         (cy-u (/ (+ (p :x) (* 2 ry)) 2))
         (cy-l (/ (- (p :x) (* 2 ry)) 2)))
    (list (stroke-open (arc-path (centre-x) cy-u rx ry 12 276))
          (stroke-open (arc-path (centre-x) cy-l rx ry 84 348)))))

(defglyph "zeta" 950
  ;; Bar, diagonal, descending hook -- each starting where the last
  ;; ends, computed rather than guessed.
  (let* ((rx (* (gx) 0.84)) (cx (centre-x))
         (kx (- cx (* rx 0.30))) (ky (* (p :x) 0.30))
         (r (* (p :tail) 0.9))
         (cy (+ (p :desc) r (/ (p :thin) 2))))
    (list (hbar (p :x) (- cx (* rx 0.62)) (+ cx rx) :align :under)
          (diag (+ cx rx) (- (p :x) (/ (p :thin) 2)) kx ky :trim nil)
          (stroke-open (append (list (line-seg kx ky (+ cx (* rx 0.2)) (+ cy r)))
                               (arc-path (+ cx (* rx 0.2) (- r)) (+ cy r) r r 0 -120))))))

(defglyph "eta" 951
  (list (stroke-open (list (line-seg (- (centre-x) (gx)) 0 (- (centre-x) (gx)) (p :x))))
        (stroke-open (append (arc-path (centre-x) (shoulder-cy) (gx) (shoulder-ry) 180 0)
                             (list (line-seg (+ (centre-x) (gx)) (shoulder-cy)
                                             (+ (centre-x) (gx)) (p :desc)))))))

(defglyph "theta" 952
  (append (stroke-closed (arc-path (centre-x) (cap-cy) (* (gx) 0.84) (cap-ry) 0 360))
          (list (hbar (cap-cy) (- (centre-x) (* (gx) 0.84)) (+ (centre-x) (* (gx) 0.84))))))

(defglyph "iota" 953
  (let ((r (* (p :tail) 0.9)))
    (list (stroke-open
           (cons (line-seg (- (centre-x) (u 60)) (p :x) (- (centre-x) (u 60))
                           (+ r (/ (p :thin) 2)))
                 (arc-path (+ (- (centre-x) (u 60)) r) (+ r (/ (p :thin) 2)) r r 180 290))))))

(defglyph "kappa" 954
  (let ((rx (gx)) (cx (centre-x)) (knee (* (p :x) 0.46)))
    (list (vstem (- cx rx) 0 (p :x))
          (diag (+ cx rx) (p :x) (- cx rx) knee :trim :start)
          (diag (- cx rx) knee (+ cx rx) 0 :trim :end))))

(defglyph "lambda" 955
  ;; The one everybody actually wants from this block.
  (let ((rx (gx)) (cx (centre-x)))
    (list (diag (- cx (* rx 0.94)) 0 (+ cx (* rx 0.30)) (p :asc))
          (diag (+ cx (* rx 0.94)) 0 (- cx (* rx 0.26)) (* (p :x) 0.62) :trim :start))))

;; U+03BC is MU and U+03BD is NU.  Labelling 956 "nu" gave Greek mu a
;; v-shape and left nu missing altogether.
(defglyph "mu.greek" 956 (glyph-contours (find-glyph "mu")))

(defglyph "nu" 957
  (let ((rx (gx)) (cx (centre-x)))
    (list (diag (- cx rx) (p :x) cx 0)
          (diag (+ cx rx) (p :x) cx 0))))

(defglyph "sigma1" 962
  ;; Final sigma: a c whose lower terminal CONTINUES into the
  ;; descender, rather than a separate stroke left near it.
  (let* ((a 320)
         (ex (+ (centre-x) (* (gx) (cos (deg a)))))
         (ey (+ (gcy) (* (gry) (sin (deg a))))))
    (list (stroke-open
           (append (arc-path (centre-x) (gcy) (gx) (gry) (- 90 (/ (p :aperture) 2)) a)
                   (list (line-seg ex ey (- (centre-x) (* (gx) 0.3))
                                   (+ (p :desc) (u 20)))))))))


(defglyph "xi" 958
  ;; Zeta with a middle bar.
  (let* ((rx (* (gx) 0.84)) (cx (centre-x))
         (kx (- cx (* rx 0.30))) (ky (* (p :x) 0.26))
         (r (* (p :tail) 0.9))
         (cy (+ (p :desc) r (/ (p :thin) 2))))
    (list (hbar (p :x) (- cx (* rx 0.62)) (+ cx rx) :align :under)
          (stroke-open (arc-path cx (* (p :x) 0.60) (* rx 0.8) (* (p :x) 0.16) 30 330))
          (diag (+ cx (* rx 0.74)) (* (p :x) 0.52) kx ky :trim nil)
          (stroke-open (append (list (line-seg kx ky (+ cx (* rx 0.2)) (+ cy r)))
                               (arc-path (+ cx (* rx 0.2) (- r)) (+ cy r) r r 0 -120))))))

(defglyph "omicron" 959 (glyph-contours (find-glyph "o")))

(defglyph "pi" 960
  (let ((rx (gx)) (cx (centre-x)))
    (list (hbar (p :x) (- cx rx) (+ cx rx) :align :under)
          (vstem (- cx (* rx 0.45)) 0 (p :x))
          (stroke-open (append (list (line-seg (+ cx (* rx 0.45)) (p :x)
                                               (+ cx (* rx 0.45)) (u 90)))
                               (arc-path (+ cx (* rx 0.75)) (u 90) (u 90) (u 90) 180 270))))))

(defglyph "rho" 961
  (append (stroke-closed (arc-path (centre-x) (gcy) (gx) (gry) 0 360))
          (list (vstem (- (centre-x) (gx)) (p :desc) (gcy)))))

(defglyph "sigma" 963
  (let ((half (/ (p :aperture) 2)))
    (list (stroke-open (arc-path (centre-x) (gcy) (gx) (gry) (- 90 half) 340))
          (stroke-open (list (line-seg (centre-x) (p :x)
                                       (+ (centre-x) (gx) (u 40)) (p :x)))))))

(defglyph "tau" 964
  (let ((rx (gx)) (cx (centre-x)))
    (list (hbar (p :x) (- cx rx) (+ cx rx) :align :under)
          (vstem (- cx (u 20)) 0 (p :x)))))

(defglyph "upsilon" 965
  (let* ((rx (gx)) (ry (* (p :x) 0.46)) (cy (+ ry (/ (p :thin) 2))))
    (list (stroke-open
           (append (list (line-seg (- (centre-x) rx) (p :x) (- (centre-x) rx) cy))
                   (arc-path (centre-x) cy rx ry 180 360)
                   (list (line-seg (+ (centre-x) rx) cy (+ (centre-x) rx) (p :x))))))))

(defglyph "phi" 966
  (append (stroke-closed (arc-path (centre-x) (gcy) (* (gx) 0.9) (* (gry) 0.86) 0 360))
          (list (vstem (centre-x) (p :desc) (p :asc)))))

(defglyph "chi" 967
  (let ((rx (gx)) (cx (centre-x)))
    (list (diag (- cx rx) (p :x) (+ cx rx) (p :desc))
          (diag (+ cx rx) (p :x) (- cx rx) (p :desc)))))

(defglyph "psi" 968
  (let* ((rx (gx)) (ry (* (p :x) 0.52)) (cy (+ ry (/ (p :thin) 2))))
    (list (stroke-open
           (append (list (line-seg (- (centre-x) rx) (p :x) (- (centre-x) rx) cy))
                   (arc-path (centre-x) cy rx ry 180 360)
                   (list (line-seg (+ (centre-x) rx) cy (+ (centre-x) rx) (p :x)))))
          (vstem (centre-x) (p :desc) (p :x)))))

(defglyph "omega" 969
  ;; Two bowls opening UPWARD whose inner ends meet at a peak in the
  ;; middle.  The previous pair swept the tops of their circles instead
  ;; of the bottoms, so it came out as two arches over a stem.
  ;;
  ;; Each arc runs 140 to 400 degrees, through 270 at the foot, so its
  ;; ends rise and turn inward.  The centres sit cos(140)*rx apart,
  ;; which is exactly where those ends coincide.
  (let* ((rx (u 130)) (ry (* (p :x) 0.55))
         (cy (- (+ ry (/ (p :thin) 2)) (p :over)))
         (d (* rx (abs (cos (deg 140))))))
    (list (stroke-open (arc-path (- (centre-x) d) cy rx ry 140 400))
          (stroke-open (arc-path (+ (centre-x) d) cy rx ry 140 400)))))

;;; --- uppercase ---------------------------------------------------------------
;;;
;;; Half of it is shaped like Latin, so those share outlines rather than
;;; being redrawn and drifting apart.

(macrolet ((same (code base) `(defglyph ,(format nil "uni~4,'0X" code) ,code
                                (glyph-contours (find-glyph ,base)))))
  (same 913 "A") (same 914 "B") (same 917 "E") (same 918 "Z")
  (same 919 "H") (same 921 "I") (same 922 "K") (same 924 "M")
  (same 925 "N") (same 927 "O") (same 929 "P") (same 932 "T")
  (same 933 "Y") (same 935 "X"))

(defglyph "Gamma" 915
  (list (vstem (left-stem-x) 0 (p :cap))
        (hbar (p :cap) (left-stem-x) (+ (centre-x) (bowl-rx)) :align :under)))

(defglyph "Delta" 916
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (diag (- cx rx) 0 cx (p :cap))
          (diag (+ cx rx) 0 cx (p :cap))
          (hbar 0 (- cx rx) (+ cx rx) :align :on))))

(defglyph "Theta" 920
  (append (stroke-closed (arc-path (centre-x) (cap-cy) (bowl-rx) (cap-ry) 0 360))
          (list (hbar (cap-cy) (- (centre-x) (* (bowl-rx) 0.5))
                      (+ (centre-x) (* (bowl-rx) 0.5))))))

(defglyph "Lambda" 923
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (diag (- cx rx) 0 cx (p :cap)) (diag (+ cx rx) 0 cx (p :cap)))))

(defglyph "Xi" 926
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (hbar (p :cap) (- cx rx) (+ cx rx) :align :under)
          (hbar (* (p :cap) 0.5) (- cx (* rx 0.74)) (+ cx (* rx 0.74)))
          (hbar 0 (- cx rx) (+ cx rx) :align :on))))

(defglyph "Pi" 928
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (hbar (p :cap) (- cx rx) (+ cx rx) :align :under)
          (vstem (- cx rx) 0 (p :cap)) (vstem (+ cx rx) 0 (p :cap)))))

(defglyph "Sigma" 931
  (let ((rx (bowl-rx)) (cx (centre-x)))
    (list (hbar (p :cap) (- cx rx) (+ cx rx) :align :under)
          ;; The diagonals run from the bars' LEFT ends to a vertex at
          ;; mid-right.  I had them the other way round, and a mirrored
          ;; sigma is the shape of the letter ezh -- which is exactly
          ;; what it kept rendering as.
          (diag (- cx rx) (p :cap) (+ cx (* rx 0.46)) (* (p :cap) 0.5) :trim :start)
          (diag (+ cx (* rx 0.46)) (* (p :cap) 0.5) (- cx rx) 0 :trim :end)
          (hbar 0 (- cx rx) (+ cx rx) :align :on))))

(defglyph "Phi" 934
  (append (stroke-closed (arc-path (centre-x) (cap-cy) (bowl-rx) (* (cap-ry) 0.72) 0 360))
          (list (vstem (centre-x) 0 (p :cap)))))

(defglyph "Psi" 936
  (let* ((rx (bowl-rx)) (ry (* (p :cap) 0.46)) (cy (+ ry (/ (p :thin) 2))))
    (list (stroke-open
           (append (list (line-seg (- (centre-x) rx) (p :cap) (- (centre-x) rx) cy))
                   (arc-path (centre-x) cy rx ry 180 360)
                   (list (line-seg (+ (centre-x) rx) cy (+ (centre-x) rx) (p :cap)))))
          (vstem (centre-x) 0 (p :cap)))))

(defglyph "Omega" 937
  ;; The arc's ends carry DOWN to the baseline before the feet turn out.
  ;; Previously the feet sat on the baseline while the arc stopped at
  ;; 0.32 of the cap, so they were three detached pieces.
  (let* ((rx (* (bowl-rx) 0.92)) (cx (centre-x))
         (ry (* (p :cap) 0.40)) (cy (+ ry (* (p :cap) 0.20)))
         ;; The bowl opens at the BOTTOM, so it sweeps nearly the whole
         ;; circle.  At 110 degrees it was a cap, not a bowl.
         (a -55)
         (ex (+ cx (* rx (cos (deg a)))))
         (ey (+ cy (* ry (sin (deg a))))))
    (list (stroke-open (arc-path cx cy rx ry a 235))
          (stroke-open (list (line-seg ex ey ex (/ (p :thin) 2))))
          (stroke-open (list (line-seg (- cx (- ex cx)) ey
                                       (- cx (- ex cx)) (/ (p :thin) 2))))
          (hbar 0 (- cx (bowl-rx)) (- cx (* (bowl-rx) 0.28)) :align :on)
          (hbar 0 (+ cx (* (bowl-rx) 0.28)) (+ cx (bowl-rx)) :align :on))))
