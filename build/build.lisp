;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: OFL-1.1

;;;; build.lisp -- generate UFO sources and PNG proofs for both grades.
(require :asdf)
;; Resolve the repository from this script's own location, so a clone
;; builds wherever it is put rather than only where it was written.
(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(asdf:initialize-source-registry
  `(:source-registry (:tree ,*root*) :inherit-configuration))
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :cadr-mono))
(in-package :cadr-mono)

;; The DEFPARAMETER above ran in CL-USER, so it interned CL-USER::*ROOT*.
;; Reading a bare *ROOT* from here would be a different symbol entirely.
(defparameter *root* cl-user::*root*)

(defun emit (design name)
  (with-design (design)
    (format t "~&~a: ~a glyphs, stem ~a, slant ~a~%"
            name (length (all-glyphs)) (p :stem) (p :slant))
    (write-ufo (merge-pathnames (format nil "build/~a.ufo" name) *root*))))

(defun build-all ()
  (emit (light-grade) "CadrMono-Regular")
  (emit (italicise (light-grade)) "CadrMono-Italic")
  (emit (embolden (light-grade)) "CadrMono-Bold")
  (emit (embolden (italicise (light-grade))) "CadrMono-BoldItalic")
  (emit (dark-grade) "CadrMonoDark-Regular")
  (emit (italicise (dark-grade)) "CadrMonoDark-Italic")
  (emit (embolden (dark-grade)) "CadrMonoDark-Bold")
  (emit (embolden (italicise (dark-grade))) "CadrMonoDark-BoldItalic")
  (format t "done~%"))

(handler-case (build-all) (error (e) (format t "~&ERROR: ~a~%" e)))
