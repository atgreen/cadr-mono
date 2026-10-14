;;;; package.lisp

(defpackage #:muskako
  (:use #:cl)
  (:export #:*design* #:design #:make-design #:light-cut #:dark-cut
           #:with-design #:p
           #:glyph #:glyph-name #:glyph-code #:glyph-advance #:glyph-contours
           #:all-glyphs #:find-glyph
           #:write-ufo))
