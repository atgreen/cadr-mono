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

(defun char-glyph-name (ch)
  (case ch
    (#\Space "space") (#\o "o") (#\c "c") (#\e "e") (#\n "n")
    (#\i "i") (#\l "l") (#\0 "zero") (#\1 "one")
    (#\. "period") (#\- "hyphen")
    (t nil)))

(defmacro with-sheet ((path w h &key (dark nil)) &body body)
  `(vecto:with-canvas (:width ,w :height ,h)
     (if ,dark (vecto:set-rgb-fill 0 0 0) (vecto:set-rgb-fill 1 1 1))
     (vecto:rectangle 0 0 ,w ,h) (vecto:fill-path)
     (if ,dark (vecto:set-rgb-fill 1 1 1) (vecto:set-rgb-fill 0 0 0))
     ,@body
     (vecto:save-png ,path)))

(defparameter *specimen* "once coil 01-."
  "A specimen made only of glyphs that exist so far.")

(defun proof-letters (path &key (size 190))
  "Every letter at display size, where construction errors are visible."
  (let* ((glyphs (remove-if-not (lambda (g) (and (glyph-contours g)
                                                 (< (or (glyph-code g) 0) #x2800)))
                                (all-glyphs)))
         (scale (/ size (p :upm)))
         (cell (* scale (p :advance)))
         (w (ceiling (+ 40 (* cell (length glyphs)))))
         (h (ceiling (* size 1.3))))
    (with-sheet (path w h)
      (let ((baseline (* size 0.28)))
        (loop for g in glyphs
              for i from 0
              do (dolist (c (glyph-contours g))
                   (draw-contour c scale (+ 20 (* i cell)) baseline))
                 (vecto:fill-path))))))

(defun proof-sizes (path &key dark (sizes '(11 12 13 14 16 20 28)))
  "The specimen at the sizes people actually set a terminal to."
  (let* ((w 520)
         (line (lambda (s) (round (* s 1.6))))
         (h (+ 24 (reduce #'+ (mapcar line sizes)))))
    (with-sheet (path w h :dark dark)
      (let ((y (- h 12)))
        (dolist (s sizes)
          (decf y (funcall line s))
          (draw-string *specimen* s 14 y))))))

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

(defun write-proof (dir)
  (let ((dir (pathname (concatenate 'string (string-right-trim "/" (namestring dir)) "/"))))
    (ensure-directories-exist dir)
    (proof-letters (merge-pathnames "letters.png" dir))
    (proof-sizes   (merge-pathnames "sizes-light.png" dir))
    (proof-sizes   (merge-pathnames "sizes-dark.png" dir) :dark t)
    (proof-braille (merge-pathnames "braille.png" dir))
    dir))
