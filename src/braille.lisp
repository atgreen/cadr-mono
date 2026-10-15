;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: OFL-1.1

;;;; braille.lisp -- all 256 patterns of U+2800, generated.
;;;;
;;;; These are what btop, gotop and every sparkline tool draw graphs
;;;; with.  Hand-drawing them would be madness, and font-specific dot
;;;; placement is exactly why those graphs look uneven today: the dots
;;;; here come straight off the cell metrics, so the grid is exact.

(in-package #:cadr-mono)

(defun braille-dot-centre (index)
  "Centre of braille dot INDEX (0-7) in the standard 2x4 arrangement:
0-2 are the left column top to bottom, 3-5 the right column, 6 and 7
the lower-left and lower-right of the eight-dot forms."
  (let* ((left  (p :br-inset))
         (right (- (p :advance) (p :br-inset)))
         (row (lambda (n) (- (p :br-top) (* n (p :br-pitch))))))
    (ecase index
      (0 (cons left  (funcall row 0)))
      (1 (cons left  (funcall row 1)))
      (2 (cons left  (funcall row 2)))
      (3 (cons right (funcall row 0)))
      (4 (cons right (funcall row 1)))
      (5 (cons right (funcall row 2)))
      (6 (cons left  (funcall row 3)))
      (7 (cons right (funcall row 3))))))

(defun braille-contours (bits)
  (loop for i from 0 below 8
        when (logbitp i bits)
          collect (let ((c (braille-dot-centre i)))
                    (dot-at (car c) (cdr c) (p :br-dot)))))

(defun register-braille ()
  (dotimes (bits 256)
    (let ((code (+ #x2800 bits))
          (b bits))
      ;; :no-centre -- the dot grid is absolute, and centring each
      ;; pattern on its own ink would destroy the tiling.
      (setf (gethash (format nil "uni~4,'0X" (+ #x2800 bits)) *glyph-builders*)
            (list code nil (lambda () (braille-contours b)))))))

(register-braille)
