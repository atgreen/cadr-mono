;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: OFL-1.1

;;;; package.lisp

(defpackage #:cadr-mono
  (:use #:cl)
  (:export #:*design* #:design #:make-design #:light-grade #:dark-grade
           #:with-design #:p
           #:glyph #:glyph-name #:glyph-code #:glyph-advance #:glyph-contours
           #:all-glyphs #:find-glyph
           #:write-ufo))
