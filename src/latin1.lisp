;;;; latin1.lisp -- the rest of Latin-1: ligatures, slashed and
;;;; crossed letters, and the symbols that live in the same block.

(in-package #:muskako)

;;; --- ligatures and odd letters ---------------------------------------------

(defglyph "oe" 339
  ;; Needed for French: coeur, oeuvre, soeur.  The two bowls SHARE their
  ;; adjacent edge rather than merely overlapping, and the pen is
  ;; thinned, because at full weight two bowls in one cell leave
  ;; counters barely wider than the strokes around them -- which is why
  ;; it read as a dark blot in running text.
  (with-pen-scale (0.80d0)
    (let* ((r (u 112)) (cx (centre-x)) (half (/ (p :aperture) 2))
           (lx (- cx r)) (ex (+ cx r))
           (ry (bowl-ry)) (cy (bowl-cy)))
      (append (stroke-closed (arc-path lx cy r ry 0 360))
              (list (stroke-open (arc-path ex cy r ry half (- 360 half)))
                    (stroke-open (list (line-seg (- ex r) cy (+ ex (* r 0.72)) cy))))))))

(defglyph "OE" 338
  (with-pen-scale (0.80d0)
    (let* ((r (u 112)) (cx (centre-x))
           (lx (- cx r)) (ex (+ cx r))
           (ry (cap-ry)) (cy (cap-cy)))
      (append (stroke-closed (arc-path lx cy r ry 0 360))
              (list (vstem (- ex r) 0 (p :cap))
                    (hbar (p :cap) (- ex r) (+ ex r) :align :under)
                    (hbar (cap-cy) (- ex r) (+ ex (* r 0.6)))
                    (hbar 0 (- ex r) (+ ex r) :align :on))))))

(defglyph "ae" 230
  ;; The left half is built the way our own A is built -- lower bowl,
  ;; top arc, right stem -- just narrower, with the stem at the junction
  ;; the e shares.
  ;;
  ;; A plain half-ellipse with a flat right side made it distinct from
  ;; oe but read as c or D: this font's a is double-storey, so a
  ;; single-storey bowl is not an a HERE whatever it is elsewhere.  A
  ;; letter has to be recognisable against its own alphabet.
  (with-pen-scale (0.80d0)
    (let* ((cx (centre-x)) (ra (u 128)) (re (u 128))
           (lx (- cx ra))
           (ry-b (* (p :x) 0.30))
           (cy-b (- (+ ry-b (/ (p :thin) 2)) (p :over)))
           (ry-t (* (p :x) 0.21))
           (cy-t (- (p :x) ry-t (/ (p :thin) 2)))
           (half (/ (p :aperture) 2)))
      (append (list (vstem cx 0 cy-t))
              (stroke-closed (arc-path lx cy-b ra ry-b 0 360))
              (list (stroke-open (arc-path lx cy-t ra ry-t 0 168))
                    (stroke-open (arc-path (+ cx re) (bowl-cy) re (bowl-ry)
                                           half (- 360 half)))
                    (stroke-open (list (line-seg cx (bowl-cy)
                                                 (+ cx (* re 1.7)) (bowl-cy)))))))))

(defglyph "AE" 198
  (let* ((rx (bowl-rx)) (cx (centre-x)) (mid (* (p :cap) 0.30)))
    (list (diag (- cx rx) 0 (- cx (* rx 0.10)) (p :cap) :trim :both)
          (hbar mid (- cx (* rx 0.72)) (+ cx (* rx 0.10)))
          (vstem (- cx (* rx 0.10)) 0 (p :cap))
          (hbar (p :cap) (- cx (* rx 0.10)) (+ cx rx) :align :under)
          (hbar (* (p :cap) 0.52) (- cx (* rx 0.10)) (+ cx (* rx 0.74)))
          (hbar 0 (- cx (* rx 0.10)) (+ cx rx) :align :on))))

(defglyph "germandbls" 223
  ;; A long s joined to a short one.
  (let* ((rx (* (bowl-rx) 0.84)) (cx (- (centre-x) (u 40)))
         (ry (* (p :x) 0.26)))
    (list (stroke-open
           (append (list (line-seg (- cx rx) 0 (- cx rx) (- (p :asc) ry)))
                   (arc-path (- cx (* rx 0.1)) (- (p :asc) ry) (* rx 0.9) ry 180 0)
                   (list (line-seg (+ cx (* rx 0.8)) (- (p :asc) ry)
                                   (+ cx (* rx 0.8)) (* (p :x) 0.52)))))
          (stroke-open (append (arc-path (+ cx (* rx 0.3)) (* (p :x) 0.52)
                                         (* rx 0.5) (* (p :x) 0.22) 90 -90)
                               (arc-path (+ cx (* rx 0.5)) (* (p :x) 0.18)
                                         (* rx 0.7) (* (p :x) 0.18) 90 -150))))))

(defun slashed (base cap-top)
  (append (glyph-contours (find-glyph base))
          (list (diag (- (centre-x) (* (bowl-rx) 1.02)) (u -30)
                      (+ (centre-x) (* (bowl-rx) 1.02)) (+ cap-top (u 30))
                      :trim nil))))

(defglyph "oslash" 248 (slashed "o" (p :x)))
(defglyph "Oslash" 216 (slashed "O" (p :cap)))

(defglyph "eth" 240
  (append (glyph-contours (find-glyph "o"))
          (list (vstem (+ (centre-x) (bowl-rx)) (* (p :x) 0.6) (p :asc))
                (diag (- (centre-x) (u 30)) (* (p :asc) 0.82)
                      (+ (centre-x) (bowl-rx) (u 40)) (* (p :asc) 0.96) :trim nil))))

(defglyph "Eth" 208
  (append (glyph-contours (find-glyph "D"))
          (list (hbar (* (p :cap) 0.52) (- (centre-x) (bowl-rx) (u 60))
                      (- (centre-x) (* (bowl-rx) 0.2))))))

(defglyph "thorn" 254
  (let* ((x0 (left-stem-x)) (ry (* (p :x) 0.34)))
    (list (vstem x0 (p :desc) (p :asc))
          (stroke-open (append (list (line-seg x0 (* (p :x) 0.86)
                                               (centre-x) (* (p :x) 0.86)))
                               (arc-path (centre-x) (- (* (p :x) 0.86) ry)
                                         (bowl-rx) ry 90 -90)
                               (list (line-seg (centre-x) (- (* (p :x) 0.86) (* 2 ry))
                                               x0 (- (* (p :x) 0.86) (* 2 ry)))))))))

(defglyph "Thorn" 222
  (let* ((x0 (left-stem-x)) (ry (* (p :cap) 0.25)) (top (* (p :cap) 0.86)))
    (list (vstem x0 0 (p :cap))
          (stroke-open (append (list (line-seg x0 top (centre-x) top))
                               (arc-path (centre-x) (- top ry) (bowl-rx) ry 90 -90)
                               (list (line-seg (centre-x) (- top (* 2 ry))
                                               x0 (- top (* 2 ry)))))))))

;;; --- Latin-1 symbols ---------------------------------------------------------

(defglyph "degree" 176
  (let ((r (u 140)))
    (stroke-closed (arc-path (centre-x) (- (p :cap) r (u 20))
                             (- r (/ (p :stem) 2)) (- r (/ (p :stem) 2)) 0 360))))

(defglyph "plusminus" 177
  ;; The bar sits near the baseline with the cross above it, so the mark
  ;; centres where the other operators do.  It used to centre at 426 per
  ;; mille against 285-298 across the field -- it floated above the line
  ;; of maths it belonged to.
  (let ((cy (u 390)) (r (u 215)) (bar (u 58)))
    (list (hbar cy (- (centre-x) r) (+ (centre-x) r))
          (vstem (centre-x) (- cy r) (+ cy r))
          (hbar bar (- (centre-x) r) (+ (centre-x) r)))))

(defglyph "multiply" 215
  (let ((cy (* (p :cap) 0.46)) (r (u 180)))
    (list (diag (- (centre-x) r) (+ cy r) (+ (centre-x) r) (- cy r) :trim nil)
          (diag (+ (centre-x) r) (+ cy r) (- (centre-x) r) (- cy r) :trim nil))))

(defglyph "divide" 247
  (let ((cy (* (p :cap) 0.46)) (r (u 215)) (g (u 170)))
    (list (hbar cy (- (centre-x) r) (+ (centre-x) r))
          (dot-at (centre-x) (+ cy g) (* (p :dot) 0.72))
          (dot-at (centre-x) (- cy g) (* (p :dot) 0.72)))))

(defglyph "logicalnot" 172
  (let ((cy (* (p :cap) 0.46)) (r (u 215)))
    (list (hbar cy (- (centre-x) r) (+ (centre-x) r))
          (vstem (+ (centre-x) r) (- cy (u 180)) cy))))

(defglyph "periodcentered" 183
  (list (dot-at (centre-x) (* (p :x) 0.52) (* (p :dot) 0.8))))

(defglyph "mu" 181
  (let* ((rx (bowl-rx)) (ry (* (p :x) 0.52)) (cy (+ ry (/ (p :thin) 2))))
    (list (stroke-open
           (append (list (line-seg (- (centre-x) rx) (p :desc) (- (centre-x) rx) (p :x)))
                   (arc-path (centre-x) cy rx ry 180 360)
                   (list (line-seg (+ (centre-x) rx) cy (+ (centre-x) rx) (p :x))))))))

(defglyph "guillemotleft" 171
  (let ((cy (* (p :x) 0.52)) (r (u 130)) (d (u 125)))
    (list (diag (- (centre-x) (u 10)) (+ cy r) (- (centre-x) d) cy :trim nil)
          (diag (- (centre-x) d) cy (- (centre-x) (u 10)) (- cy r) :trim nil)
          (diag (+ (centre-x) d) (+ cy r) (+ (centre-x) (u 10)) cy :trim nil)
          (diag (+ (centre-x) (u 10)) cy (+ (centre-x) d) (- cy r) :trim nil))))

(defglyph "guillemotright" 187
  (let ((cy (* (p :x) 0.52)) (r (u 130)) (d (u 125)))
    (list (diag (+ (centre-x) (u 10)) (+ cy r) (+ (centre-x) d) cy :trim nil)
          (diag (+ (centre-x) d) cy (+ (centre-x) (u 10)) (- cy r) :trim nil)
          (diag (- (centre-x) d) (+ cy r) (- (centre-x) (u 10)) cy :trim nil)
          (diag (- (centre-x) (u 10)) cy (- (centre-x) d) (- cy r) :trim nil))))

(defun turned (base)
  "BASE rotated half a turn about the middle of the cap band.

The Spanish opening marks ARE the closing marks turned, so deriving
them keeps the two in step -- and the hand-built inverted question mark
had its hook and stem as separate pieces that merely crossed."
  (mapcar (lambda (c) (rotate-about c (centre-x) (/ (p :cap) 2) 180))
          (glyph-contours (find-glyph base))))

(defglyph "exclamdown" 161 (turned "exclam"))
(defglyph "questiondown" 191 (turned "question"))

(defglyph "section" 167
  (let* ((rx (* (bowl-rx) 0.62))
         (ry (/ (+ (p :cap) (* 2 (p :over)) (- (p :thin))) 6))
         (cy-u (- (p :cap) ry (/ (p :thin) 2)))
         (cy-l (+ ry (/ (p :thin) 2))))
    (list (stroke-open (arc-path (centre-x) cy-u rx ry 10 260))
          (stroke-open (arc-path (centre-x) cy-l rx ry 190 440))
          (stroke-open (list (line-seg (centre-x) cy-l (centre-x) cy-u))))))

(defglyph "paragraph" 182
  (let* ((rx (* (bowl-rx) 0.6)) (ry (* (p :cap) 0.26))
         (cy (- (p :cap) ry (/ (p :thin) 2))))
    (list (stroke-open (append (arc-path (centre-x) cy rx ry 90 270)
                               (list (line-seg (centre-x) (- cy ry)
                                               (+ (centre-x) (* rx 0.4)) (- cy ry)))))
          (vstem (+ (centre-x) (* rx 0.4)) 0 (p :cap))
          (vstem (+ (centre-x) (* rx 0.9)) 0 (p :cap)))))

(defun ringed (base)
  "A letter shrunk into a ring -- the construction of (c) and (r)."
  (let* ((cy (* (p :cap) 0.5)) (r (* (bowl-rx) 0.92))
         (inner (scale-contours (glyph-contours (find-glyph base)) 0.54
                                :about-x (centre-x) :about-y (cap-cy))))
    (append (stroke-closed (arc-path (centre-x) cy (- r (/ (p :stem) 2))
                                     (- r (/ (p :stem) 2)) 0 360))
            (move-contours inner 0 (round (- cy (cap-cy)))))))

(defun superscripted (base)
  "A small raised letter with a rule under it: the ordinals."
  (let* ((small (scale-contours (glyph-contours (find-glyph base)) 0.58
                                :about-x (centre-x) :about-y 0))
         (lift (round (* (p :cap) 0.42))))
    (append (move-contours small 0 lift)
            (list (hbar (* (p :cap) 0.36) (- (centre-x) (u 150))
                        (+ (centre-x) (u 150)))))))

(defglyph "copyright" 169 (ringed "C"))
(defglyph "registered" 174 (ringed "R"))

(defglyph "ordfeminine" 170 (superscripted "a"))
(defglyph "ordmasculine" 186 (superscripted "o"))

;;; --- currency ------------------------------------------------------------------

(defglyph "cent" 162
  (let ((half (/ (p :aperture) 2)))
    (list (stroke-open (arc-path (centre-x) (bowl-cy) (* (bowl-rx) 0.92)
                                 (bowl-ry) half (- 360 half)))
          (vstem (centre-x) (u -90) (+ (p :x) (u 90))))))

(defglyph "sterling" 163
  (let* ((cx (centre-x)) (rx (* (bowl-rx) 0.86)) (ry (* (p :cap) 0.24)))
    (list (stroke-open
           (append (arc-path (- cx (* rx 0.1)) (- (p :cap) ry) (* rx 0.8) ry 20 200)
                   (list (line-seg (- cx (* rx 0.9)) (- (p :cap) ry)
                                   (- cx (* rx 0.9)) (/ (p :thin) 2)))))
          (hbar 0 (- cx rx) (+ cx rx) :align :on)
          (hbar (* (p :cap) 0.42) (- cx rx) (+ cx (* rx 0.3))))))

(defglyph "yen" 165
  (let ((rx (bowl-rx)) (cx (centre-x)) (mid (* (p :cap) 0.52)))
    (list (diag (- cx rx) (p :cap) cx mid :trim :start)
          (diag (+ cx rx) (p :cap) cx mid :trim :start)
          (vstem cx 0 mid)
          (hbar (* (p :cap) 0.40) (- cx (* rx 0.8)) (+ cx (* rx 0.8)))
          (hbar (* (p :cap) 0.22) (- cx (* rx 0.8)) (+ cx (* rx 0.8))))))

(defglyph "currency" 164
  (let* ((cx (centre-x)) (cy (* (p :cap) 0.5)) (r (u 190)) (d (u 125)))
    (append (stroke-closed (arc-path cx cy (- r (/ (p :stem) 2))
                                     (- r (/ (p :stem) 2)) 0 360))
            (list (diag (- cx r d) (+ cy r d) (- cx (* r 0.7)) (+ cy (* r 0.7)) :trim nil)
                  (diag (+ cx r d) (+ cy r d) (+ cx (* r 0.7)) (+ cy (* r 0.7)) :trim nil)
                  (diag (- cx r d) (- cy r d) (- cx (* r 0.7)) (- cy (* r 0.7)) :trim nil)
                  (diag (+ cx r d) (- cy r d) (+ cx (* r 0.7)) (- cy (* r 0.7)) :trim nil)))))

(defglyph "Euro" 8364
  (let* ((half (/ (p :aperture) 2)) (rx (* (bowl-rx) 0.98)))
    (list (stroke-open (arc-path (centre-x) (cap-cy) rx (cap-ry) half (- 360 half)))
          (hbar (* (p :cap) 0.58) (- (centre-x) rx (u 60)) (+ (centre-x) (* rx 0.3)))
          (hbar (* (p :cap) 0.38) (- (centre-x) rx (u 60)) (+ (centre-x) (* rx 0.3))))))

(defglyph "florin" 402
  (let* ((cx (- (centre-x) (u 20))) (r (* (p :tail) 0.9)))
    (list (stroke-open
           (append (arc-path (+ cx r) (- (p :asc) r (/ (p :thin) 2)) r r 180 70)
                   (list (line-seg cx (- (p :asc) r (/ (p :thin) 2))
                                   (- cx (u 60)) (p :desc)))))
          (hbar (* (p :x) 0.52) (- cx (u 190)) (+ cx (u 180))))))
