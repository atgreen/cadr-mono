;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: OFL-1.1

;;;; ufo.lisp -- emit a UFO 3 source tree.
;;;;
;;;; A UFO is a directory of XML: writing one is transcription, not
;;;; translation, which is why this is the seam between our Lisp and the
;;;; rest of the world's font tooling.  fontmake/fontc take it from here.

(in-package #:cadr-mono)

;;; --- release identity -------------------------------------------------

;;; The release version, as semver.  CI passes the git tag through
;;; CADR_MONO_VERSION so a tagged build stamps itself; a local build
;;; falls back to the constant below, which is the version in
;;; CHANGELOG.md and must be bumped with it.
(defparameter +version+ "1.0.1")

(defun env (name)
  (let ((v #+sbcl (sb-ext:posix-getenv name) #-sbcl nil))
    (and v (plusp (length v)) v)))

(defun release-version ()
  (or (env "CADR_MONO_VERSION") +version+))

(defun version-parts ()
  "major, minor, patch from the semver string."
  (let* ((v (string-left-trim "v" (release-version)))
         (dot1 (position #\. v))
         (dot2 (and dot1 (position #\. v :start (1+ dot1)))))
    (values (parse-integer v :end dot1 :junk-allowed t)
            (if dot1 (parse-integer v :start (1+ dot1) :end dot2 :junk-allowed t) 0)
            (if dot2 (parse-integer v :start (1+ dot2) :junk-allowed t) 0))))

;;; OpenType carries a 16.16 fontRevision, not a semver, so the patch
;;; number folds into the fractional part: 1.2.3 becomes 1.203.  Three
;;; digits is all the format gives us and it is enough for a font.
(defun font-revision-minor ()
  (multiple-value-bind (major minor patch) (version-parts)
    (declare (ignore major))
    (+ (* minor 100) (min 99 patch))))

(defun build-stamp ()
  "A UTC stamp identifying this particular build.

Two builds of the same source install to the same path under the same
family name and the same version, which leaves no way to tell which one
an application is actually holding.  That matters because replacing the
file does not dislodge it: a running VTE or Pango keeps the FreeType
face it already opened.  The stamp does not force a reload -- nothing
in the file can -- but it makes it possible to ask what is live.

  fc-query -f '%{fontversion}\n' ~/.local/share/fonts/CadrMono-Regular.ttf"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore sec))
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d" year month day hour min)))

(defun xml-header (s)
  (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%"))

(defun plist-open (s)
  (xml-header s)
  (format s "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">~%")
  (format s "<plist version=\"1.0\">~%<dict>~%"))

(defun plist-close (s) (format s "</dict>~%</plist>~%"))

(defun kv (s key type value)
  (format s "  <key>~a</key>~%  <~a>~a</~a>~%" key type value type))

(defun kv-int (s key value) (kv s key "integer" value))

(defun kv-ints (s key values)
  "An array of integers.  PANOSE is ten of them, not a string."
  (format s "  <key>~a</key>~%  <array>~%" key)
  (dolist (v values) (format s "    <integer>~a</integer>~%" v))
  (format s "  </array>~%"))
(defun kv-str (s key value) (kv s key "string" value))

(defun kv-bool (s key value)
  "A plist boolean is a self-closing tag, not content, so KV cannot
write it."
  (format s "  <key>~a</key>~%  <~:[false~;true~]/>~%" key value))

(defun write-glif (glyph path)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :external-format :utf-8)
    (xml-header s)
    (format s "<glyph name=\"~a\" format=\"2\">~%" (glyph-name glyph))
    (format s "  <advance width=\"~a\"/>~%" (glyph-advance glyph))
    (when (glyph-code glyph)
      (format s "  <unicode hex=\"~4,'0X\"/>~%" (glyph-code glyph)))
    (when (glyph-contours glyph)
      (format s "  <outline>~%")
      (dolist (c (glyph-contours glyph))
        (format s "    <contour>~%")
        (dolist (pnt c)
          (format s "      <point x=\"~a\" y=\"~a\"~@[ type=\"~a\"~]/>~%"
                  (px pnt) (py pnt)
                  (case (kind pnt)
                    (:line "line") (:curve "curve") (t nil))))
        (format s "    </contour>~%"))
      (format s "  </outline>~%"))
    (format s "</glyph>~%")))

(defun write-ufo (dir)
  "Write the current design as a UFO 3 source tree at DIR."
  (let* ((dir (pathname (concatenate (quote string) (string-right-trim "/" (namestring dir)) "/")))
         (glyphs-dir (merge-pathnames "glyphs/" dir))
         (all (all-glyphs)))
    (ensure-directories-exist glyphs-dir)

    ;; Clear the layer first.  contents.plist is rewritten from scratch
    ;; below, so a stale .glif would not reach the compiled font -- but
    ;; build/nerdfont.py writes ten thousand of them into this same
    ;; directory, and leaving those behind makes the tree confusing to
    ;; read and the next merge non-idempotent.
    (dolist (f (directory (merge-pathnames "*.glif" glyphs-dir)))
      (delete-file f))

    (with-open-file (s (merge-pathnames "metainfo.plist" dir) :direction :output
                       :if-exists :supersede :external-format :utf-8)
      (plist-open s)
      (kv-str s "creator" "org.moxielogic.cadr-mono")
      (kv-int s "formatVersion" 3)
      (plist-close s))

    (with-open-file (s (merge-pathnames "layercontents.plist" dir) :direction :output
                       :if-exists :supersede :external-format :utf-8)
      (xml-header s)
      (format s "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">~%")
      (format s "<plist version=\"1.0\">~%<array>~%  <array>~%    <string>public.default</string>~%    <string>glyphs</string>~%  </array>~%</array>~%</plist>~%"))

    (with-open-file (s (merge-pathnames "fontinfo.plist" dir) :direction :output
                       :if-exists :supersede :external-format :utf-8)
      (plist-open s)
      ;; Family carries the CUT (the grade); style carries the slope.
      ;; So Cadr Mono / Cadr Mono Dark each get a Regular and an Italic, and
      ;; a terminal's bold-and-italic switches land where they should.
      (kv-str s "familyName" (p :family))
      (kv-str s "styleName" (p :style))
      (kv-int s "italicAngle" (p :slant))
      (kv-int s "openTypeOS2WeightClass" (p :weight))
      (kv-int s "unitsPerEm" (p :upm))
      (kv-int s "ascender" (p :line-asc))
      (kv-int s "descender" (p :line-desc))
      (kv-int s "capHeight" (p :cap))
      (kv-int s "xHeight" (p :x))
      (kv-int s "openTypeHheaAscender" (p :line-asc))
      (kv-int s "openTypeHheaDescender" (p :line-desc))
      (kv-int s "openTypeOS2TypoAscender" (p :line-asc))
      (kv-int s "openTypeOS2TypoDescender" (p :line-desc))
      (kv-int s "openTypeOS2WinAscent" (p :line-asc))
      (kv-int s "openTypeOS2WinDescent" (abs (p :line-desc)))
      ;; 2 = Latin text, 11 = normal sans, 9 = monospaced proportion
      (kv-ints s "openTypeOS2Panose" '(2 11 5 9 0 0 0 0 0 0))
      ;; Every glyph in this font has the same advance, but that is not
      ;; self-evident to a font picker filtering for monospaced faces --
      ;; it reads the flag.
      (kv-bool s "postscriptIsFixedPitch" t)
      ;; Release metadata.  The OFL asks that the licence travel with the
      ;; binary, not just with the repository, so the notice goes in the
      ;; name table (ID 13) rather than only in LICENSE.
      (kv-int s "versionMajor" (nth-value 0 (version-parts)))
      (kv-int s "versionMinor" (font-revision-minor))
      ;; ttfautohint appends its own note to this string, so it reads
      ;; "Version 1.000; build ...; ttfautohint (...)" in the shipped font.
      (kv-str s "openTypeNameVersion"
              (format nil "Version ~d.~3,'0d; build ~a"
                      (nth-value 0 (version-parts)) (font-revision-minor)
                      (build-stamp)))
      (kv-str s "copyright" "Copyright 2026 Anthony Green. SIL Open Font License 1.1.")
      (kv-str s "openTypeNameDesigner" "Anthony Green")
      (kv-str s "openTypeNameLicense"
              "This Font Software is licensed under the SIL Open Font License, Version 1.1. This license is available with a FAQ at https://openfontlicense.org")
      (kv-str s "openTypeNameLicenseURL" "https://openfontlicense.org")
      (plist-close s))

    (with-open-file (s (merge-pathnames "contents.plist" glyphs-dir) :direction :output
                       :if-exists :supersede :external-format :utf-8)
      (plist-open s)
      (dolist (g all)
        (kv-str s (glyph-name g) (format nil "~a.glif" (glyph-name g))))
      (plist-close s))

    (dolist (g all)
      (write-glif g (merge-pathnames (format nil "~a.glif" (glyph-name g)) glyphs-dir)))

    (length all)))
