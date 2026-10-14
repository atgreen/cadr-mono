;;;; ufo.lisp -- emit a UFO 3 source tree.
;;;;
;;;; A UFO is a directory of XML: writing one is transcription, not
;;;; translation, which is why this is the seam between our Lisp and the
;;;; rest of the world's font tooling.  fontmake/fontc take it from here.

(in-package #:muskako)

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
(defun kv-str (s key value) (kv s key "string" value))

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

    (with-open-file (s (merge-pathnames "metainfo.plist" dir) :direction :output
                       :if-exists :supersede :external-format :utf-8)
      (plist-open s)
      (kv-str s "creator" "org.moxielogic.muskako")
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
      (kv-str s "familyName" (format nil "~a~@[ ~a~]" (p :family)
                                     (unless (string= (p :style) "Regular") (p :style))))
      (kv-str s "styleName" "Regular")
      (kv-int s "unitsPerEm" (p :upm))
      (kv-int s "ascender" (p :asc))
      (kv-int s "descender" (p :desc))
      (kv-int s "capHeight" (p :cap))
      (kv-int s "xHeight" (p :x))
      (kv-int s "openTypeHheaAscender" (p :asc))
      (kv-int s "openTypeHheaDescender" (p :desc))
      (kv-int s "openTypeOS2TypoAscender" (p :asc))
      (kv-int s "openTypeOS2TypoDescender" (p :desc))
      (kv-int s "openTypeOS2WinAscent" (+ (p :asc) 40))
      (kv-int s "openTypeOS2WinDescent" (+ (abs (p :desc)) 40))
      (kv-str s "openTypeOS2Panose" "2 11 5 9 0 0 0 0 0 0")
      (kv-str s "copyright" "Copyright 2026 Anthony Green. SIL Open Font License 1.1.")
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
