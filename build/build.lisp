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
  (emit (italicise (light-cut) :slant -10) "Muskako-Italic")
  (emit (dark-cut) "MuskakoDark-Regular")
  (emit (italicise (dark-cut) :slant -10) "MuskakoDark-Italic")
  (format t "done~%"))

(handler-case (build-all) (error (e) (format t "~&ERROR: ~a~%" e)))
