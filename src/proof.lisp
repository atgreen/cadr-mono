;;;; proof.lisp -- rasterise the design to PNG so it can be looked at.
;;;;
;;;; There is no human drawing these letters, so the only way to judge
;;;; them is to render and look.  Everything here exists to make that
;;;; loop fast: no font install, no editor, no compile step.

(in-package #:muskako)

(defun draw-contour (c scale ox oy)
  (let* ((c (normalize-start c))
         (start (first c)))
    (flet ((sx (p) (+ ox (* scale (px p))))
           (sy (p) (+ oy (* scale (py p)))))
      (vecto:move-to (sx start) (sy start))
      (let ((pending '()))
        (dolist (pnt (append (rest c) (list start)))
          (if (eq (kind pnt) :offcurve)
              (push pnt pending)
              (let ((ctrl (nreverse pending)))
                (setf pending '())
                (case (length ctrl)
                  (2 (vecto:curve-to (sx (first ctrl)) (sy (first ctrl))
                                     (sx (second ctrl)) (sy (second ctrl))
                                     (sx pnt) (sy pnt)))
                  (1 (vecto:curve-to (sx (first ctrl)) (sy (first ctrl))
                                     (sx (first ctrl)) (sy (first ctrl))
                                     (sx pnt) (sy pnt)))
                  (t (vecto:line-to (sx pnt) (sy pnt))))))))
      (vecto:close-subpath))))

(defun draw-string (text size x baseline)
  "Draw TEXT at SIZE pixels per em, left edge at X.  Returns the pen x
after the run.  All contours of a glyph go into one path so that
counters cut holes under the nonzero fill rule -- the same result the
compiler's overlap removal produces."
  (let ((scale (/ size (p :upm)))
        (pen x))
    (loop for ch across text
          for name = (char-glyph-name ch)
          for g = (and name (find-glyph name))
          do (when (and g (glyph-contours g))
               (dolist (c (glyph-contours g))
                 (draw-contour c scale pen baseline))
               (vecto:fill-path))
             (incf pen (* scale (p :advance))))
    pen))

(defvar *code-index* nil)

(defun code-index ()
  "Map codepoint to glyph name, so the specimen can use any glyph that
exists rather than a hand-maintained case form that silently drops the
ones I forget to add."
  (or *code-index*
      (setf *code-index*
            (let ((h (make-hash-table)))
              (maphash (lambda (name e)
                         (when (first e) (setf (gethash (first e) h) name)))
                       *glyph-builders*)
              h))))

(defun char-glyph-name (ch)
  (gethash (char-code ch) (code-index)))

(defmacro with-sheet ((path w h &key (dark nil)) &body body)
  `(vecto:with-canvas (:width ,w :height ,h)
     (if ,dark (vecto:set-rgb-fill 0 0 0) (vecto:set-rgb-fill 1 1 1))
     (vecto:rectangle 0 0 ,w ,h) (vecto:fill-path)
     (if ,dark (vecto:set-rgb-fill 1 1 1) (vecto:set-rgb-fill 0 0 0))
     ,@body
     (vecto:save-png ,path)))

(defparameter *specimen-lines*
  '("(defun grade (glyph &key (units 4))"
    "  (dolist (c (contours glyph))     ; 4u/side"
    "    (offset c (- units)))          ; 180 -> 164"
    "  (assert (= (width glyph) 1229)))"
    ""
    "Illegible? 0O1lI {}[]()<> =>|& #!$%^~"
    "The quick brown fox jumps over 42 dogs.")
  "Real code and the confusable pairs, at the sizes a terminal uses.")

(defparameter *specimen* (first *specimen-lines*))

(defun guide (y w &key (heavy nil))
  "A horizontal reference line.  Judging overshoot without one is
guessing."
  (if heavy (vecto:set-rgb-stroke 0.72 0.24 0.48) (vecto:set-rgb-stroke 0.80 0.82 0.86))
  (vecto:set-line-width (if heavy 1.0 1.0))
  (vecto:move-to 0 y) (vecto:line-to w y) (vecto:stroke)
  (vecto:set-rgb-fill 0 0 0))

(defun draw-guides (w scale baseline)
  (dolist (spec (list (list (p :line-desc) nil) (list (p :desc) nil) (list 0 t)
                      (list (p :x) t) (list (p :cap) nil) (list (p :asc) nil)
                      (list (p :line-asc) nil)))
    (guide (+ baseline (* scale (first spec))) w :heavy (second spec))))

(defun proof-letters (path &key (size 140) (cols 10) (only nil))
  "Every letter at display size against the metric guides.

Laid out as a grid rather than one row: a single row of 90 glyphs gets
scaled down to fit a screen, which hides the very detail the display
proof exists to show."
  (let* ((glyphs (remove-if-not (lambda (g)
                                  (and (glyph-contours g)
                                       (if only
                                           (member (glyph-code g) only)
                                           (< (or (glyph-code g) 0) #x2500))))
                                (all-glyphs)))
         (scale (/ size (p :upm)))
         (cell (* scale (p :advance)))
         (row-h (* size 1.45))
         (rows (ceiling (length glyphs) cols))
         (w (ceiling (+ 30 (* cell cols))))
         (h (ceiling (+ 20 (* row-h rows)))))
    (with-sheet (path w h)
      (loop for row from 0 below rows
            for baseline = (+ 20 (* (- rows row 1) row-h) (* size 0.30))
            do (draw-guides w scale baseline)
               (loop for i from (* row cols) below (min (length glyphs) (* (1+ row) cols))
                     for g = (nth i glyphs)
                     do (dolist (c (glyph-contours g))
                          (draw-contour c scale (+ 15 (* (- i (* row cols)) cell)) baseline))
                        (vecto:fill-path))))))

(defun proof-sizes (path &key dark (sizes '(11 12 13 14 16 20 28)))
  "Every specimen line at the sizes people actually set a terminal to."
  (let* ((w 760)
         (line (lambda (s) (round (* s 1.55))))
         (block-h (lambda (s) (+ 10 (* (length *specimen-lines*) (funcall line s)))))
         (h (+ 20 (reduce #'+ (mapcar block-h sizes)))))
    (with-sheet (path w h :dark dark)
      (let ((y (- h 14)))
        (dolist (s sizes)
          (dolist (ln *specimen-lines*)
            (decf y (funcall line s))
            (draw-string ln s 14 y))
          (decf y 10))))))

(defun proof-grade (path &key (sizes '(13 16 24 56)))
  "The reference cut beside the graded cut, both on black -- the whole
point of the project, and something these proofs have never shown."
  (let* ((w 820) (half (/ w 2))
         (line (lambda (s) (round (* s 1.7))))
         (h (+ 30 (reduce #'+ (mapcar line sizes)))))
    (with-sheet (path w h :dark t)
      ;; a hairline between the two cuts so the eye has an edge to judge on
      (vecto:set-rgb-stroke 0.25 0.25 0.25) (vecto:set-line-width 1.0)
      (vecto:move-to half 0) (vecto:line-to half h) (vecto:stroke)
      (vecto:set-rgb-fill 1 1 1)
      (let ((y (- h 18))
            (ref (light-cut))
            (grd (dark-cut :grade (d-grade (dark-cut)))))
        (dolist (s sizes)
          (decf y (funcall line s))
          ;; trim to what the column holds, or the two cuts collide
          (let* ((cell (* (/ s (p :upm)) (p :advance)))
                 (fits (max 1 (floor (- half 28) cell)))
                 (txt (subseq *specimen* 0 (min (length *specimen*) fits))))
            (with-design (ref) (draw-string txt s 14 y))
            (with-design (grd) (draw-string txt s (+ half 14) y))))))))

(defun proof-braille (path &key (size 44) (cols 32))
  "A block of braille patterns: the dot grid has to look even."
  (let* ((scale (/ size (p :upm)))
         (cw (* scale (p :advance)))
         (ch (* size 1.25))
         (rows (ceiling 256 cols))
         (w (ceiling (+ 20 (* cw cols))))
         (h (ceiling (+ 20 (* ch rows)))))
    (with-sheet (path w h :dark t)
      (dotimes (i 256)
        (let* ((g (find-glyph (format nil "uni~4,'0X" (+ #x2800 i))))
               (col (mod i cols)) (row (floor i cols)))
          (when (and g (glyph-contours g))
            (dolist (c (glyph-contours g))
              (draw-contour c scale (+ 10 (* col cw))
                            (+ 10 (* (- rows row 1) ch) (* size 0.25))))
            (vecto:fill-path)))))))

(defparameter *box-art*
  '("┌─────┬─────┐  ╔═════╦═════╗  ╭─────╮"
    "│ foo │ bar │  ║ foo ║ bar ║  │ box │"
    "├─────┼─────┤  ╠═════╬═════╣  ╰─────╯"
    "│ baz │ qux │  ║ baz ║ qux ║   ╱╲╳  "
    "└─────┴─────┘  ╚═════╩═════╝  ┏━┳━┓"
    "┠─┰─┨ ┞─┟ ╞═╡ ╟─╢ ┌┬┐ ╒╤╕ ╓╥╖ ┗━┻━┛"
    "▁▂▃▄▅▆▇█ ░▒▓ ▌▐▀▄ ▖▗▘▙▚▛▜▝▞▟ ┄┈╌ ┆┊╎"))

(defun proof-boxes (path &key (sizes (list 13 17 26)))
  "Box art drawn as real adjacent cells.  The only thing that matters
here is whether rules meet across the cell boundary -- a gap at a
corner or a T-joint is the defect, and it only shows when characters
sit next to each other at their true advance."
  (let* ((line (lambda (s) (round (* s (/ (- (p :line-asc) (p :line-desc)) (float (p :upm)))))))
         (h (+ 30 (reduce #'+ (mapcar (lambda (s)
                                        (+ 14 (* (length *box-art*) (funcall line s))))
                                      sizes))))
         (w 780))
    (with-sheet (path w h :dark t)
      (let ((y (- h 20)))
        (dolist (s sizes)
          (dolist (ln *box-art*)
            (decf y (funcall line s))
            (draw-string ln s 12 y))
          (decf y 14))))))

(defun proof-box-grid (path &key (first #x2500) (last #x257F) (size 56) (cols 16))
  "Every box glyph large enough to inspect its joints individually."
  (let* ((n (1+ (- last first)))
         (scale (/ size (p :upm)))
         (cw (* scale (p :advance)))
         (ch (* scale (- (p :line-asc) (p :line-desc))))
         (rows (ceiling n cols))
         (w (ceiling (+ 20 (* cw cols)))) (h (ceiling (+ 20 (* ch rows)))))
    (with-sheet (path w h :dark t)
      (dotimes (i n)
        (let* ((g (find-glyph (format nil "uni~4,'0X" (+ first i))))
               (col (mod i cols)) (row (floor i cols)))
          (when (and g (glyph-contours g))
            (dolist (c (glyph-contours g))
              (draw-contour c scale (+ 10 (* col cw))
                            (+ 10 (* (- rows row 1) ch) (* scale (- (p :line-desc))))))
            (vecto:fill-path)))))))

(defun write-proof (dir)
  (setf *code-index* nil)
  (let ((dir (pathname (concatenate 'string (string-right-trim "/" (namestring dir)) "/"))))
    (ensure-directories-exist dir)
    (proof-letters (merge-pathnames "letters.png" dir))
    (proof-sizes   (merge-pathnames "sizes-light.png" dir))
    (proof-sizes   (merge-pathnames "sizes-dark.png" dir) :dark t)
    (proof-braille (merge-pathnames "braille.png" dir))
    (proof-grade   (merge-pathnames "grade.png" dir))
    (proof-boxes   (merge-pathnames "boxes.png" dir))
    (proof-box-grid (merge-pathnames "box-grid.png" dir))
    dir))
