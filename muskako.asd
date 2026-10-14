;;;; muskako.asd -- a parametric monospace for terminals

(asdf:defsystem #:muskako
  :description "Parametric terminal typeface: generated glyphs, UFO output, PNG proofs."
  :author "Anthony Green <green@moxielogic.com>"
  :license "OFL-1.1"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "params")
                             (:file "geom")
                             (:file "pen")
                             (:file "primitives")
                             (:file "glyphs")
                             (:file "accents")
                             (:file "latin1")
                             (:file "greek")
                             (:file "symbols")
                             (:file "braille")
                             (:file "boxes")
                             (:file "ufo")
                             (:file "diagnose")))))
