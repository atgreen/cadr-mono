;;;; build.lisp -- generate UFO sources and PNG proofs for both cuts.
(require :asdf)
(asdf:initialize-source-registry
  '(:source-registry (:tree "/home/green/git/muskako/") :inherit-configuration))
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :muskako))
(in-package :muskako)

(defun build-all ()
  (with-design ((light-cut))
    (format t "~&light cut: ~a glyphs~%" (length (all-glyphs)))
    (write-ufo "/home/green/git/muskako/build/Muskako-Regular.ufo"))
  (with-design ((dark-cut :grade 4))
    (format t "dark cut:  ~a glyphs, stem ~a~%" (length (all-glyphs)) (p :stem))
    (write-ufo "/home/green/git/muskako/build/MuskakoDark-Regular.ufo"))
  (format t "done~%"))

(handler-case (build-all) (error (e) (format t "~&ERROR: ~a~%" e)))
