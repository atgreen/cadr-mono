;;;; build.lisp -- generate UFO sources and PNG proofs for both cuts.
(require :asdf)
(asdf:initialize-source-registry
  '(:source-registry (:tree "/home/green/git/muskako/") :inherit-configuration))
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :muskako))
(in-package :muskako)

(defun emit (design name)
  (with-design (design)
    (format t "~&~a: ~a glyphs, stem ~a, slant ~a~%"
            name (length (all-glyphs)) (p :stem) (p :slant))
    (write-ufo (format nil "/home/green/git/muskako/build/~a.ufo" name))))

(defun build-all ()
  (emit (light-cut) "Muskako-Regular")
  (emit (italicise (light-cut)) "Muskako-Italic")
  (emit (embolden (light-cut)) "Muskako-Bold")
  (emit (embolden (italicise (light-cut))) "Muskako-BoldItalic")
  (emit (dark-cut) "MuskakoDark-Regular")
  (emit (italicise (dark-cut)) "MuskakoDark-Italic")
  (emit (embolden (dark-cut)) "MuskakoDark-Bold")
  (emit (embolden (italicise (dark-cut))) "MuskakoDark-BoldItalic")
  (format t "done~%"))

(handler-case (build-all) (error (e) (format t "~&ERROR: ~a~%" e)))
