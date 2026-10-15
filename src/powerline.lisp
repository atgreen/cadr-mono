;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: OFL-1.1

;;;; powerline.lisp -- U+E0B0..U+E0BF, the powerline separators.
;;;;
;;;; These are the only Nerd Font glyphs this face generates, and they
;;;; are here because they are the only ones that are GEOMETRY.  The
;;;; other ten thousand are drawn pictograms from nine upstream icon
;;;; projects; they belong to the font-patcher, not to a font whose
;;;; claim is that every outline is a function of src/params.lisp.
;;;;
;;;; Like box drawing, a separator has to meet its neighbour: a shell
;;;; prompt paints a background colour change under the boundary, and
;;;; any hairline between the separator and the cell beside it shows as
;;;; a bright seam.  So they are built from the cell -- advance wide,
;;;; line-box tall -- they opt out of centring, and they opt out of the
;;;; italic shear, for the same reason box drawing does.
;;;;
;;;; They also BLEED: the body runs a sixteenth of an advance past the
;;;; cell edge on the side away from the point.  That overflow is
;;;; deliberate and is what guarantees no seam; it is the one place
;;;; outside box drawing where ink is supposed to leave the cell.
;;;;
;;;; Proportions are the ecosystem's, taken off the reference set and
;;;; expressed as fractions of this font's own advance and line box.
;;;; A prompt is a shape users recognise before they read it, so an
;;;; arrow of our own invention would just look broken.

(in-package #:cadr-mono)

(defun pl-bleed ()   (round (* (p :advance) 40/600)))   ; past the far edge
(defun pl-chevron () (round (* (p :advance) 118/600)))  ; thin arrow, horizontal
(defun pl-hair ()    (round (* (p :advance) 28/600)))   ; thin diagonal

(defun poly (&rest xy)
  "A closed polygon from a flat coordinate list, wound counterclockwise."
  (ensure-ccw (loop for (x y) on xy by #'cddr collect (pt :line (round x) (round y)))))

(defmacro defpl (code &body body)
  `(put-box ,code (lambda () ,@body)))

;;; --- solid and hollow arrows ---------------------------------------------

(defpl #xE0B0                           ; solid, pointing right
  (let ((b (pl-bleed)))
    (list (poly (- b) (cell-bottom) (cell-right) (cell-mid-y) (- b) (cell-top)))))

(defpl #xE0B2                           ; solid, pointing left
  (let ((b (+ (cell-right) (pl-bleed))))
    (list (poly b (cell-top) (cell-left) (cell-mid-y) b (cell-bottom)))))

(defpl #xE0B1                           ; hollow, pointing right
  (let ((t* (pl-chevron)))
    (list (poly t* (cell-bottom)
                (cell-left) (cell-bottom)
                (- (cell-right) t*) (cell-mid-y)
                (cell-left) (cell-top)
                t* (cell-top)
                (cell-right) (cell-mid-y)))))

(defpl #xE0B3                           ; hollow, pointing left
  (let ((t* (pl-chevron)))
    (list (poly (- (cell-right) t*) (cell-bottom)
                (cell-right) (cell-bottom)
                (cell-left) (cell-mid-y)
                (cell-right) (cell-top)
                (- (cell-right) t*) (cell-top)
                (cell-left) (cell-mid-y)))))

;;; --- solid and hollow half-rounds ----------------------------------------
;;;
;;; The hollow pair is a crescent, not a rule bent round: its inner arc
;;; keeps the OUTER arc's vertical radius, so the stroke is full width
;;; where the curve runs vertically and vanishes where it runs
;;; horizontally.  That is how the reference draws it and it is what
;;; makes the shape read as a bowl rather than a tube.

(defun pl-ry () (round (/ (- (cell-top) (cell-bottom)) 2)))

(defpl #xE0B4                           ; solid, bulging right
  (let ((b (pl-bleed)))
    (list (append (arc-points (cell-left) (cell-mid-y) (cell-right) (pl-ry) -90 90)
                  (list (pt :line (- b) (cell-top))
                        (pt :line (- b) (cell-bottom)))))))

(defpl #xE0B6                           ; solid, bulging left
  (let ((b (+ (cell-right) (pl-bleed))))
    (list (append (arc-points (cell-right) (cell-mid-y) (cell-right) (pl-ry) 90 270)
                  (list (pt :line b (cell-bottom))
                        (pt :line b (cell-top)))))))

(defpl #xE0B5                           ; hollow, bulging right
  (list (arc-annulus (cell-left) (cell-mid-y) (cell-right) (pl-ry)
                     (pl-chevron) (round (* (pl-ry) 0.004)) -90 90 :stress 0)))

(defpl #xE0B7                           ; hollow, bulging left
  (list (arc-annulus (cell-right) (cell-mid-y) (cell-right) (pl-ry)
                     (pl-chevron) (round (* (pl-ry) 0.004)) 90 270 :stress 0)))

;;; --- solid and hollow diagonals ------------------------------------------

(defpl #xE0B8                           ; solid, lower left
  (let ((b (pl-bleed)))
    (list (poly (- b) (cell-bottom) (cell-right) (cell-bottom) (- b) (cell-top)))))

(defpl #xE0BA                           ; solid, lower right
  (let ((b (+ (cell-right) (pl-bleed))))
    (list (poly b (cell-bottom) b (cell-top) (cell-left) (cell-bottom)))))

(defpl #xE0BC                           ; solid, upper left
  (let ((b (pl-bleed)))
    (list (poly (- b) (cell-bottom) (cell-right) (cell-top) (- b) (cell-top)))))

(defpl #xE0BE                           ; solid, upper right
  (let ((b (+ (cell-right) (pl-bleed))))
    (list (poly b (cell-bottom) b (cell-top) (cell-left) (cell-top)))))

;;; The hollow diagonals come in pairs that draw the same line; the set
;;; carries both so a prompt can pair either hard triangle with a
;;; matching hairline without reaching across the block.
(macrolet ((backslash (code)            ; high on the left, low on the right
             `(defpl ,code
                (let ((h (pl-hair)))
                  (list (poly (cell-left) (cell-top)
                              h (cell-top)
                              (cell-right) (cell-bottom)
                              (- (cell-right) h) (cell-bottom))))))
           (forwardslash (code)         ; low on the left, high on the right
             `(defpl ,code
                (let ((h (pl-hair)))
                  (list (poly (cell-left) (cell-bottom)
                              h (cell-bottom)
                              (cell-right) (cell-top)
                              (- (cell-right) h) (cell-top)))))))
  (backslash    #xE0B9)
  (forwardslash #xE0BB)
  (forwardslash #xE0BD)
  (backslash    #xE0BF))
