;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: OFL-1.1

;;;; cadr-mono.asd -- a parametric monospace for terminals

(asdf:defsystem #:cadr-mono
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
                             (:file "cyrillic")
                             (:file "boxes")
                             (:file "powerline")
                             (:file "ufo")
                             (:file "diagnose")))))
