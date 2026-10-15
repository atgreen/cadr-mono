;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: OFL-1.1

;;;; cyrillic.lisp -- the modern Slavic Cyrillic set.
;;;;
;;;; Russian, Ukrainian, Belarusian, Bulgarian, Serbian and Macedonian.
;;;; Not the whole U+0400 block: that reaches into historic and
;;;; minority orthographies whose conventions this face has no business
;;;; guessing at.
;;;;
;;;; TWO THINGS MAKE CYRILLIC DIFFERENT TO ADD THAN GREEK WAS.
;;;;
;;;; Its lowercase is mostly small capitals.  Latin lowercase is a
;;;; separate alphabet -- a is not a small A -- but Cyrillic en, em,
;;;; ka and te really are their capitals at x-height.  So each shape is
;;;; written ONCE as a function of its height and called twice.  Scaling
;;;; the capital would have been easier and wrong: it scales the stroke
;;;; with it, and the lowercase would come out lighter than the text
;;;; around it.
;;;;
;;;; And a third of it is already here.  A V E K M N O P C T X and
;;;; a e o p c x are shared with Latin, not merely similar, so they are
;;;; aliased rather than redrawn -- one shape, one set of parameters,
;;;; no drift when a stem width changes.

(in-package #:cadr-mono)

(defun alias (name code base)
  "A letter that IS another letter.  Not a copy -- the contours are
rebuilt from the base every time the design changes."
  (setf (gethash name *glyph-builders*)
        (list code t (lambda () (glyph-contours (find-glyph base))) 0)))

;;; --- shapes, each a function of its height --------------------------------

(defun cyr-stem-l (h) (left-stem-x))
(defun cyr-stem-r (h) (declare (ignore h)) (+ (centre-x) (bowl-rx)))

(defun cyr-be (h)
  "B-e.  A stem with a flat top arm and a bowl on the lower half --
the top of a Gamma over the bottom of a B."
  (let* ((x0 (left-stem-x)) (mid (* h 0.54))
         (ry (/ mid 2)))
    (list (vstem x0 0 h)
          (hbar h x0 (+ (centre-x) (* (bowl-rx) 0.84)) :align :under)
          (stroke-open (append (list (line-seg x0 mid (centre-x) mid))
                               (arc-path (centre-x) (/ mid 2) (bowl-rx) ry 90 -90)
                               (list (line-seg (centre-x) (/ (p :thin) 2)
                                               x0 (/ (p :thin) 2))))))))

(defun cyr-ghe (h)
  "Ghe.  A stem and a top arm, and nothing else."
  (list (vstem (left-stem-x) 0 h)
        (hbar h (left-stem-x) (+ (centre-x) (* (bowl-rx) 0.86)) :align :under)))

(defun cyr-de (h)
  "De.  A roof over a platform, with legs that drop below the line."
  (let* ((x0 (- (centre-x) (bowl-rx))) (x1 (+ (centre-x) (bowl-rx)))
         (foot (* h -0.20))
         (inx0 (+ x0 (u 54))) (inx1 (- x1 (u 54))))
    (list (hbar (/ (p :thin) 2) x0 x1 :align :on)
          (vstem (+ x0 (u 30)) foot (/ (p :thin) 2))
          (vstem (- x1 (u 30)) foot (/ (p :thin) 2))
          (stroke-open (list (line-seg (+ inx0 (u 70)) (p :thin)
                                       (+ inx0 (u 118)) h)))
          (hbar h (+ inx0 (u 100)) (- inx1 (u 10)) :align :under)
          (stroke-open (list (line-seg (- inx1 (u 30)) h
                                       (- inx1 (u 30)) (p :thin)))))))

(defun cyr-zhe (h)
  "Zhe.  A central stem with two pairs of diagonals -- the widest
letter in the alphabet and the one most likely to crowd its cell."
  (let* ((cx (centre-x)) (rx (bowl-rx)))
    (list (vstem cx 0 h)
          (diag (- cx rx) 0 (- cx (u 20)) (* h 0.52))
          (diag (- cx rx) h (- cx (u 20)) (* h 0.52))
          (diag (+ cx rx) 0 (+ cx (u 20)) (* h 0.52))
          (diag (+ cx rx) h (+ cx (u 20)) (* h 0.52)))))

(defun cyr-ze (h)
  "Ze.  Two bowls open to the left, like a figure three."
  (let* ((ry (/ h 4)) (rx (* (bowl-rx) 0.92)) (a (/ (p :aperture) 2)))
    (list (stroke-open (arc-path (centre-x) (- h ry) rx ry (- 180 a) -90))
          (stroke-open (arc-path (centre-x) ry rx ry 90 (+ -180 a))))))

(defun cyr-i (h &optional lower)
  "I.  N reversed -- the diagonal climbs to the right.  The italic
lowercase is not that at all: Cyrillic italic is cursive, and this
letter becomes a Latin u.  Literally a u -- so it is the u, rebuilt
from the same parameters, not a second drawing of one."
  (if (and lower (p :italic))
      (glyph-contours (find-glyph "u"))
      (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
        (list (vstem x0 0 h) (vstem x1 0 h) (diag x0 0 x1 h :trim :both)))))

(defun cyr-el (h)
  "El.  A left leg that kicks out, and a right stem."
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
    (list (vstem x1 0 h)
          (stroke-open (list (line-seg x1 h (+ x0 (u 86)) h)))
          (diag (+ x0 (u 86)) h x0 0 :trim :end))))

(defun cyr-pe (h &optional lower)
  "Pe.  Two stems and a roof -- Greek Pi, built the same way.  Italic
lowercase is a Latin n."
  (if (and lower (p :italic))
      (glyph-contours (find-glyph "n"))
      (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
        (list (vstem x0 0 h) (vstem x1 0 h) (hbar h x0 x1 :align :under)))))

(defun cyr-ef (h)
  "Ef.  A bowl either side of a stem that runs past both."
  (let* ((cy (/ h 2)) (ry (* h 0.34)) (rx (* (bowl-rx) 0.96)))
    (list (vstem (centre-x) 0 h)
          (stroke-open (arc-path (centre-x) cy rx ry 90 270))
          (stroke-open (arc-path (centre-x) cy rx ry 90 -90)))))

(defun cyr-tse (h &optional lower)
  "Tse.  Pe with a tail hanging off the right foot; in the italic
lowercase, I with a tail, which is to say a u with one."
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
    (if (and lower (p :italic))
        (append (glyph-contours (find-glyph "u"))
                (list (vstem (- x1 (u 10)) (* h -0.20) (/ (p :thin) 2))))
        (list (vstem x0 0 h)
              (vstem x1 (* h -0.20) h)
              (hbar (/ (p :thin) 2) x0 x1 :align :on)))))

(defun cyr-che (h)
  "Che.  A bowl that stops at mid-height, then a stem to the floor."
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))) (mid (* h 0.46)))
    (list (vstem x1 0 h)
          (vstem x0 mid h)
          (hbar mid x0 x1 :align :on))))

(defun cyr-sha (h)
  "Sha.  Three stems on a floor."
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
    (list (vstem x0 0 h) (vstem (centre-x) 0 h) (vstem x1 0 h)
          (hbar (/ (p :thin) 2) x0 x1 :align :on))))

(defun cyr-shcha (h)
  "Shcha.  Sha with a tail."
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
    (append (cyr-sha h)
            (list (vstem (- x1 (u 10)) (* h -0.20) (/ (p :thin) 2))))))

(defun cyr-soft (h)
  "Soft sign.  A stem with a bowl on its lower half."
  (let* ((x0 (left-stem-x)) (mid (* h 0.52)))
    (list (vstem x0 0 h)
          (stroke-open (append (list (line-seg x0 mid (centre-x) mid))
                               (arc-path (centre-x) (/ mid 2) (* (bowl-rx) 0.92)
                                         (/ mid 2) 90 -90)
                               (list (line-seg (centre-x) (/ (p :thin) 2)
                                               x0 (/ (p :thin) 2))))))))

(defun cyr-hard (h)
  "Hard sign.  The soft sign with an arm reaching back over the top."
  (append (cyr-soft h)
          (list (hbar h (- (left-stem-x) (u 150)) (left-stem-x) :align :under))))

(defun cyr-yeru (h)
  "Yeru.  The soft sign with a second stem standing clear to its right."
  (append (cyr-soft h)
          (list (vstem (+ (centre-x) (bowl-rx)) 0 h))))

(defun cyr-e (h)
  "E.  A bowl open to the LEFT with a bar across the middle -- the
mirror of Ze's sibling, and not to be confused with Ye."
  (let ((a (/ (p :aperture) 2)))
    (list (stroke-open (arc-path (centre-x) (/ h 2) (* (bowl-rx) 0.98)
                                 (- (/ h 2) (/ (p :thin) 2)) (+ 90 a) (- -90 a)))
          (hbar (/ h 2) (* (centre-x) 0.92) (+ (centre-x) (* (bowl-rx) 0.62))))))

(defun cyr-yu (h)
  "Yu.  A stem, a bar, and a ring that does not touch it."
  (let* ((x0 (left-stem-x)) (cx (+ x0 (u 150) (* (bowl-rx) 0.62)))
         (rx (* (bowl-rx) 0.66)) (ry (- (/ h 2) (/ (p :thin) 2))))
    ;; STROKE-CLOSED yields outer and counter, so it appends rather
    ;; than nesting -- wrapping it in LIST buries two contours inside
    ;; one and the glyph collapses.
    (append (list (vstem x0 0 h)
                  (hbar (/ h 2) x0 (- cx rx)))
            (stroke-closed (arc-path cx (/ h 2) rx ry 0 360)))))

(defun cyr-ya (h)
  "Ya.  R reversed: the bowl is on the left and the leg kicks the
other way."
  (mirror-contours
   (let* ((x0 (left-stem-x)) (ry (* h 0.27)) (waist (- h (* 2 ry)))
          (y1 (- h (/ (p :thin) 2))))
     (list (vstem x0 0 h)
           (stroke-open (append (list (line-seg x0 y1 (centre-x) y1))
                                (arc-path (centre-x) (- y1 ry) (bowl-rx) ry 90 -90)
                                (list (line-seg (centre-x) waist x0 waist))))
           (diag (+ x0 (* (bowl-rx) 0.55)) waist (+ (centre-x) (bowl-rx)) 0
                 :trim :end)))))

;;; --- the small-capital shapes ---------------------------------------------
;;;
;;; These five ARE their Latin counterparts in the capital, and the
;;; lowercase is the same shape at x-height rather than a different
;;; letter.  Written once at a given height and used for both.

(defun cyr-be-lower ()
  "Lowercase be.  The one letter in the alphabet that is NOT its
capital shrunk: it has an ascender, a closed bowl on the baseline and
an arm reaching right off the top -- nearer a figure 6 with a flag
than anything in the Latin lowercase.  Built as a small capital it
comes out looking like a stray cap in the middle of a word, which is
exactly how it looked."
  (let* ((cx (centre-x)) (rx (bowl-rx)) (ry (bowl-ry)) (cy (bowl-cy)))
    (append (stroke-closed (arc-path cx cy rx ry 0 360))
            (list (stroke-open (list (line-seg (- cx rx) cy (- cx rx) (p :asc))))
                  (hbar (p :asc) (- cx rx) (+ cx (* rx 0.80)) :align :under)))))

(defun cyr-ve (h)
  (let* ((x0 (left-stem-x)) (mid (* h 0.52)))
    (list (vstem x0 0 h)
          (stroke-open (append (list (line-seg x0 (- h (/ (p :thin) 2))
                                               (centre-x) (- h (/ (p :thin) 2))))
                               (arc-path (centre-x) (/ (+ (- h (/ (p :thin) 2)) mid) 2)
                                         (* (bowl-rx) 0.9)
                                         (/ (- (- h (/ (p :thin) 2)) mid) 2) 90 -90)
                               (list (line-seg (centre-x) mid x0 mid))))
          (stroke-open (append (list (line-seg x0 mid (centre-x) mid))
                               (arc-path (centre-x) (/ (+ mid (/ (p :thin) 2)) 2)
                                         (bowl-rx)
                                         (/ (- mid (/ (p :thin) 2)) 2) 90 -90)
                               (list (line-seg (centre-x) (/ (p :thin) 2)
                                               x0 (/ (p :thin) 2))))))))

(defun cyr-ka (h)
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
    (list (vstem x0 0 h)
          (diag x1 h (+ x0 (u 30)) (* h 0.46) :trim :both)
          (diag (+ x0 (u 96)) (* h 0.56) x1 0 :trim :both))))

(defun cyr-em (h)
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
    (list (vstem x0 0 h) (vstem x1 0 h)
          (diag x0 h (centre-x) (* h 0.18) :trim :both)
          (diag x1 h (centre-x) (* h 0.18) :trim :both))))

(defun cyr-en (h)
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
    (list (vstem x0 0 h) (vstem x1 0 h) (hbar (/ h 2) x0 x1))))

(defun cyr-te (h &optional lower)
  "Te.  Italic lowercase is a Latin m -- the most startling difference
between upright and italic Cyrillic, and the one a reader notices
immediately when it is missing."
  (if (and lower (p :italic))
      (glyph-contours (find-glyph "m"))
      (list (vstem (centre-x) 0 h)
            (hbar h (- (centre-x) (bowl-rx)) (+ (centre-x) (bowl-rx)) :align :under))))

;;; --- the letters the other orthographies add ------------------------------

(defun cyr-ghe-upturn (h)
  "Ukrainian ghe with upturn.  The arm turns up at its end -- the one
letter that distinguishes Ukrainian's alphabet from Russian's at a
glance."
  ;; The tick rises from the arm to full height, so the ARM drops to
  ;; make room -- which is how the letter is actually drawn, and why
  ;; Ghe-upturn always sits a little lower than plain Ghe beside it.
  ;; Hanging the tick below the arm instead just makes a bent Ghe.
  (let* ((tick (u 130))
         (arm (- h tick))
         (x0 (left-stem-x))
         (x1 (+ (centre-x) (* (bowl-rx) 0.86))))
    (list (vstem x0 0 arm)
          (hbar arm x0 x1 :align :under)
          (vstem (- x1 (/ (p :stem) 2)) (- arm (/ (p :thin) 2)) h))))

(defun cyr-ye (h)
  "Ukrainian ye.  E turned round: the bowl opens to the right."
  (mirror-contours (cyr-e h)))

(defun cyr-tshe (h)
  "Serbian tshe.  A stem crossed near the top, with a shoulder."
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
    (list (vstem x0 0 h)
          (hbar (* h 0.80) (- x0 (u 96)) (+ x0 (u 150)))
          (vstem x1 0 (* h 0.56))
          (stroke-open (arc-path (/ (+ x0 x1) 2) (* h 0.56)
                                 (/ (- x1 x0) 2)
                                 (* h 0.16) 0 180)))))

(defun cyr-dje (h)
  "Serbian dje.  Tshe with the right stem carried below the line and
hooked back."
  (let* ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx)))
         (r (* (p :tail) 1.3)) (cy (+ (p :desc) r)))
    (append (list (vstem x0 0 h)
                  (hbar (* h 0.80) (- x0 (u 96)) (+ x0 (u 150)))
                  (stroke-open (arc-path (/ (+ x0 x1) 2) (* h 0.56)
                                         (/ (- x1 x0) 2) (* h 0.16) 0 180)))
            (list (stroke-open
                   (append (list (line-seg x1 (* h 0.56) x1 cy))
                           (arc-path (- x1 r) cy r r 0 -90)))))))

(defun cyr-dzhe (h)
  "Macedonian and Serbian dzhe.  Tse with the tail moved to the middle."
  (let ((x0 (left-stem-x)) (x1 (+ (centre-x) (bowl-rx))))
    (list (vstem x0 0 h) (vstem x1 0 h)
          (hbar (/ (p :thin) 2) x0 x1 :align :on)
          (vstem (centre-x) (* h -0.20) (/ (p :thin) 2)))))

(defun cyr-lje (h)
  "El and the soft sign, sharing a stem."
  (let* ((x0 (left-stem-x)) (xm (+ (centre-x) (u 10)))
         (mid (* h 0.52)))
    (list (vstem xm 0 h)
          (stroke-open (list (line-seg xm h (+ x0 (u 60)) h)))
          (diag (+ x0 (u 60)) h x0 0 :trim :end)
          (stroke-open (append (list (line-seg xm mid (+ xm (u 90)) mid))
                               (arc-path (+ xm (u 90)) (/ mid 2)
                                         (- (+ (centre-x) (bowl-rx)) (+ xm (u 90)))
                                         (/ mid 2) 90 -90)
                               (list (line-seg (+ xm (u 90)) (/ (p :thin) 2)
                                               xm (/ (p :thin) 2))))))))

(defun cyr-nje (h)
  "En and the soft sign, sharing a stem."
  (let* ((x0 (left-stem-x)) (xm (centre-x)) (mid (* h 0.52)))
    (list (vstem x0 0 h) (vstem xm 0 h)
          (hbar (/ h 2) x0 xm)
          (stroke-open (append (list (line-seg xm mid (+ xm (u 90)) mid))
                               (arc-path (+ xm (u 90)) (/ mid 2)
                                         (- (+ (centre-x) (bowl-rx)) (+ xm (u 90)))
                                         (/ mid 2) 90 -90)
                               (list (line-seg (+ xm (u 90)) (/ (p :thin) 2)
                                               xm (/ (p :thin) 2))))))))

;;; --- the alphabet ---------------------------------------------------------
;;;
;;; Capitals at cap height, lowercase at x-height, from the same
;;; functions.  Letters marked ALIAS are the Latin glyph itself.

;; The lowercase is told that it IS the lowercase, because four of
;; these letters take a cursive form in the italic and the capitals do
;; not: Cyrillic capitals slope, Cyrillic lowercase is redrawn.
(macrolet ((pair (name code-up code-lo fn &optional lower-aware)
             `(progn
                (defglyph ,(concatenate 'string name "-cy") ,code-up (,fn (p :cap)))
                (defglyph ,(concatenate 'string (string-downcase name) "-cy")
                    ,code-lo ,(if lower-aware
                                  `(,fn (p :x) t)
                                  `(,fn (p :x)))))))
  (defglyph "Be-cy" #x0411 (cyr-be (p :cap)))
  (pair "Ve"    #x0412 #x0432 cyr-ve)
  (pair "Ghe"   #x0413 #x0433 cyr-ghe)
  (pair "De"    #x0414 #x0434 cyr-de)
  (pair "Zhe"   #x0416 #x0436 cyr-zhe)
  (pair "Ze"    #x0417 #x0437 cyr-ze)
  (pair "Ii"    #x0418 #x0438 cyr-i t)
  (pair "Ka"    #x041A #x043A cyr-ka)
  (pair "El"    #x041B #x043B cyr-el)
  (pair "Em"    #x041C #x043C cyr-em)
  (pair "En"    #x041D #x043D cyr-en)
  (pair "Pe"    #x041F #x043F cyr-pe t)
  (pair "Te"    #x0422 #x0442 cyr-te t)
  (pair "Ef"    #x0424 #x0444 cyr-ef)
  (pair "Tse"   #x0426 #x0446 cyr-tse t)
  (pair "Che"   #x0427 #x0447 cyr-che)
  (pair "Sha"   #x0428 #x0448 cyr-sha)
  (pair "Shcha" #x0429 #x0449 cyr-shcha)
  (pair "Hard"  #x042A #x044A cyr-hard)
  (pair "Yeru"  #x042B #x044B cyr-yeru)
  (pair "Soft"  #x042C #x044C cyr-soft)
  (pair "Erev"  #x042D #x044D cyr-e)
  (pair "Yu"    #x042E #x044E cyr-yu)
  (pair "Ya"    #x042F #x044F cyr-ya)
  ;; the other orthographies
  (pair "Ghestroke" #x0490 #x0491 cyr-ghe-upturn)
  (pair "Ye"    #x0404 #x0454 cyr-ye)
  (pair "Tshe"  #x040B #x045B cyr-tshe)
  (pair "Dje"   #x0402 #x0452 cyr-dje)
  (pair "Dzhe"  #x040F #x045F cyr-dzhe)
  (pair "Lje"   #x0409 #x0459 cyr-lje)
  (pair "Nje"   #x040A #x045A cyr-nje))

;; Shared with Latin outright -- same shape, same parameters, no drift.
(defglyph "be-cy" #x0431 (cyr-be-lower))

(alias "A-cy" #x0410 "A")   (alias "a-cy" #x0430 "a")
(alias "Ie-cy" #x0415 "E")  (alias "ie-cy" #x0435 "e")
(alias "O-cy" #x041E "O")   (alias "o-cy" #x043E "o")
(alias "Er-cy" #x0420 "P")  (alias "er-cy" #x0440 "p")
(alias "Es-cy" #x0421 "C")  (alias "es-cy" #x0441 "c")
(alias "U-cy" #x0423 "Y")   (alias "u-cy" #x0443 "y")
(alias "Ha-cy" #x0425 "X")  (alias "ha-cy" #x0445 "x")
(alias "Dze-cy" #x0405 "S") (alias "dze-cy" #x0455 "s")
(alias "Byelo-cy" #x0406 "I") (alias "byelo-cy" #x0456 "i")
(alias "Je-cy" #x0408 "J")  (alias "je-cy" #x0458 "j")

;; Marked letters, composed like any other.
(defaccented "Io-cy"    #x0401 "E" mark-dieresis)
(defaccented "io-cy"    #x0451 "e" mark-dieresis)
(defaccented "Yi-cy"    #x0407 "I" mark-dieresis)
(defaccented "yi-cy"    #x0457 "dotlessi" mark-dieresis)
(defaccented "Gje-cy"   #x0403 "Ghe-cy" mark-acute)
(defaccented "gje-cy"   #x0453 "ghe-cy" mark-acute)
(defaccented "Kje-cy"   #x040C "Ka-cy" mark-acute)
(defaccented "kje-cy"   #x045C "ka-cy" mark-acute)
(defaccented "Ushort-cy" #x040E "Y" mark-breve)
(defaccented "ushort-cy" #x045E "y" mark-breve)
(defaccented "Iishort-cy" #x0419 "Ii-cy" mark-breve)
(defaccented "iishort-cy" #x0439 "ii-cy" mark-breve)
