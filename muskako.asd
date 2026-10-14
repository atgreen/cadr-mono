;;;; muskako.asd -- a parametric monospace for terminals

(asdf:defsystem #:muskako
  :description "Parametric terminal typeface: generated glyphs, UFO output, PNG proofs."
  :author "Anthony Green <green@moxielogic.com>"
  :license "OFL-1.1"
  :depends-on (#:vecto)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "params")
                             (:file "geom")
                             (:file "pen")
                             (:file "primitives")
                             (:file "glyphs")
                             (:file "braille")
                             (:file "boxes")
                             (:file "ufo")
                             (:file "proof")
                             (:file "diagnose")))))
