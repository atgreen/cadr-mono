;;;; params.lisp -- the design parameter set.
;;;;
;;;; This is the surface you adjust.  Every glyph in the face is a pure
;;;; function of these numbers, so changing one here changes it everywhere
;;;; and consistently.  All values are in font units at 1000 per em.

(in-package #:cadr-mono)

(defstruct (design (:conc-name d-))
  ;; --- the em and the cell -------------------------------------------
  ;;
  ;; EVERY metric below is written PER MILLE -- as if the em were 1000
  ;; units -- and P scales it to the actual upm on the way out.  That
  ;; keeps the design resolution-independent and makes upm a real
  ;; parameter instead of a migration.
  (upm        2048)   ; units per em.  2048 is the TrueType convention and
                      ; a power of two, which the rasteriser prefers; 1000
                      ; is the PostScript/CFF convention.  We ship hinted
                      ; static TTFs, so the grid is TrueType's.
  (advance     600)   ; every glyph is exactly this wide.  Never varies.

  ;; --- vertical proportions ------------------------------------------
  ;; x/cap lands at 0.714, deliberately below the tall-x coding faces:
  ;; ascenders need room or word silhouettes stop reading in prose.
  (cap-height  700)
  (x-height    530)   ; x/advance 0.883.  At 500 it was 0.833, the lowest
                      ; in the field (0.867-0.917) and the reason the
                      ; font read light: the letters were short in a
                      ; normal-width cell, not narrow.  The stem is
                      ; unchanged, so this makes the text bigger without
                      ; making the strokes heavier.
  ;; LETTER extents -- where ink actually tops out and bottoms out.
  (ascender    760)   ; b d f h k l
  (descender  -230)   ; g j p q y

  ;; LINE extents -- the cell the terminal reserves.  These are NOT the
  ;; letter extents, and conflating them is what leaves a font with no
  ;; headroom: ink then touches, or overflows, the row above.  Measured
  ;; against the field, the norm is 8-24% of the line box clear above
  ;; the tallest ink, most commonly around 11%; we had MINUS 3%.
  ;; These also set the advance-to-line-box ratio, 600/1210 = 0.496,
  ;; which puts us with Hack (0.517) and Adwaita (0.508) rather than at
  ;; the Courier-ish extreme of 0.606 we sat at before.
  ;;
  ;; The ascent has to contain an accented CAPITAL, not just an
  ;; ascender.  Hack and Liberation both set theirs to exactly their
  ;; tallest accented cap; ours clears it by a little.
  (line-ascent   920)
  (line-descent -290)
  (overshoot    12)   ; round letters exceed flat ones by this

  ;; --- stroke --------------------------------------------------------
  (sidebearing  58)   ; space outside the round letters.  Bold needs it
                      ; tighter: the advance cannot grow, so the only
                      ; room a heavier stroke can take is the margin.
  (stem         88)   ; vertical stroke weight.  The grade moves this.
  (thin         72)   ; horizontal stroke weight (88 * 0.82)
  (stress-angle  -5)  ; degrees.  Humanist: the thin spots tilt off vertical.
                      ; At -16 with thin/stem 0.70 the tilt was legible as
                      ; wobble: every round letter's counter leaned and no
                      ; two terminals pointed the same way.  Measured, the
                      ; field runs 0.70 (Liberation) to 0.89 (JetBrains);
                      ; the outlier here was never the modulation on its
                      ; own, it was the modulation AND the tilt together.
  (tension    0.60d0) ; bowl flatness.  0.5523 is a true ellipse; higher
                      ; squares the shoulders the way drawn bowls do.

  ;; --- humanist detailing --------------------------------------------
  (aperture     96)   ; DEGREES OF OPENING on c e s -- the gap itself.
                      ; Smaller is more closed, and a more closed c
                      ; carries more ink.  (This read the other way round
                      ; until measurement caught it: the arc spanned
                      ; 180+2a, so raising the "aperture" closed the
                      ; letter.)
  (terminal-cut 22)   ; degrees.  Angled stroke ends, not flat ones.
  (shoulder-ease 0.54); arch radius of n m h u r, as a fraction of x-height
  (tail         110)  ; centreline radius of the tail on l and t
  (dot-radius    54)  ; i j, punctuation, and the mark inside zero
  (serif-weight 1.05d0) ; serifs on the narrow glyphs carry weight, not
                      ; just length: reaching further with a hairline
                      ; still leaves illi lighter than nnnn.  But 1.55
                      ; overshot badly.  It made i's foot 1.26x the
                      ; STEM, where every serifed reference draws that
                      ; slab THINNER than its stem (0.76-0.91), and it
                      ; pushed illi/nnnn to 0.93 against a field of
                      ; 0.74-0.91 -- denser than any of them, solving a
                      ; lightness problem that had stopped existing.
                      ; 1.05 puts the slab at 0.86 of the stem, mid
                      ; field, and illi at 0.77.
  (serif        210)  ; half-length of the serif arms on i j l, and of
                      ; both bars on I.  A bare stem leaves four times
                      ; the sidebearing of a round letter and floats in
                      ; its cell; narrow glyphs have to earn their width
                      ; in a monospace.  245 earned too much: i's foot
                      ; spanned 82% of the cell and I's bars the same,
                      ; against fields of 66-80% and 46-68%, making I
                      ; the second most unconventional glyph in the
                      ; font.  210 lands i at 70% and I inside its
                      ; field.

  ;; --- disambiguation choices ----------------------------------------
  ;; :SLASH, not :dot.  Measured on blurred rasters at 11/13/16px
  ;; (build/confusion.py), O against 0 scores 0.0001 with no mark,
  ;; 0.0017 with the dot and 0.0620 with the slash -- so the dot
  ;; recovers almost none of the distance and left O/0 the second most
  ;; confusable pair in the font.  The dot is 0.7px at 13px: a
  ;; sub-pixel mark inside a counter cannot survive rasterising.
  ;; Enlarging it fails the other way, since a dot big enough at 13px
  ;; is a theta at 40px.  A slash is the same proportional mark at
  ;; every size, which is the property that was actually needed.
  (zero-mark  :slash) ; :dot :slash or :none
  (one-serif      t)  ; base serif on the numeral 1
  (l-tail         t)  ; tail on lowercase l

  ;; --- braille and box drawing ---------------------------------------
  (braille-dot   58)  ; radius of a braille dot
  (braille-inset 150) ; dot columns sit at advance/4 and 3*advance/4 so the
                      ; gap inside a cell equals the gap across cells --
                      ; this is what makes btop graphs tile evenly
  (braille-top   680) ; y of the top dot row
  (braille-pitch 190) ; vertical distance between dot rows
  (rule-weight    70) ; stroke weight of box-drawing rules

  ;; --- slope ------------------------------------------------------------
  ;; Measured across the field: JetBrains and Adwaita slope 9 degrees,
  ;; Hack 11, Liberation 12.  Hack and Liberation are pure obliques --
  ;; nothing redrawn; JetBrains reshapes f and y, Adwaita only f.  So a
  ;; genuinely drawn italic is as rare as the research said, and this
  ;; one reshapes more than any of them.
  (crowd       1.0d0) ; extra thinning for the glyphs that pack several
                      ; strokes into one cell.  A bold can afford more of
                      ; it than a regular: at regular weight m's stem is
                      ; already near a whole pixel at 13px and cannot be
                      ; thinned further, where a bold stem has room.
  (weight      400)   ; OS/2 weight class
  (slant         0)   ; degrees of slope; negative leans right
  (italic      nil)   ; whether to use the italic letterforms

  ;; --- provenance ----------------------------------------------------
  (family "Cadr Mono")
  (style  "Regular")
  (grade     0))      ; units of ink removed per side; 0 is the reference cut

(defvar *design* (make-design)
  "The design currently being generated.")

(defmacro with-design ((design) &body body)
  `(let ((*design* ,design)) ,@body))

(defparameter +metric-slots+
  '(:advance :cap :x :asc :desc :line-asc :line-desc :over :stem :thin :tail :dot :serif :sidebearing
    :br-dot :br-inset :br-top :br-pitch :rule :grade)
  "Slots carrying a length.  These are written per mille and scaled to
the current upm; angles, ratios and flags are not.")

(defun metric-scale () (/ (d-upm *design*) 1000))

(defun u (per-mille)
  "A length written per mille, in units of the current em.  Use this for
any literal length in a glyph so it tracks the em.  Rounded, because an
advance width has to be a whole number of units."
  (round (* per-mille (metric-scale))))

(defun raw-p (slot)
  (ecase slot
    (:upm (d-upm *design*))            (:advance (d-advance *design*))
    (:cap (d-cap-height *design*))     (:x (d-x-height *design*))
    (:asc (d-ascender *design*))       (:desc (d-descender *design*))
    (:line-asc (d-line-ascent *design*)) (:line-desc (d-line-descent *design*))
    (:over (d-overshoot *design*))     (:stem (d-stem *design*))
    (:sidebearing (d-sidebearing *design*)) (:weight (d-weight *design*))
    (:crowd (d-crowd *design*))
    (:thin (d-thin *design*))          (:stress (d-stress-angle *design*))
    (:aperture (d-aperture *design*))
    (:ease (d-shoulder-ease *design*)) (:tail (d-tail *design*))
    (:tension (d-tension *design*))
    (:dot (d-dot-radius *design*))     (:zero (d-zero-mark *design*))
    (:serif (d-serif *design*))      (:serif-weight (d-serif-weight *design*))
    (:one-serif (d-one-serif *design*)) (:l-tail (d-l-tail *design*))
    (:br-dot (d-braille-dot *design*)) (:br-inset (d-braille-inset *design*))
    (:br-top (d-braille-top *design*)) (:br-pitch (d-braille-pitch *design*))
    (:rule (d-rule-weight *design*))
    (:family (d-family *design*))      (:style (d-style *design*))
    (:slant (d-slant *design*))       (:italic (d-italic *design*))
    (:grade (d-grade *design*))))

(defun p (slot)
  "Read a design parameter, e.g. (p :stem).  Lengths come back in units
of the current em; everything else comes back as written."
  (let ((v (raw-p slot)))
    (if (member slot +metric-slots+) (u v) v)))

(defun light-cut (&rest overrides)
  "The reference cut, drawn for dark-on-light."
  (apply #'make-design :family "Cadr Mono" :style "Regular" :grade 0 overrides))

(defun embolden (d)
  "Make a cut bold.

Measured across the field, a bold stem runs 1.32 to 1.58 times the
regular -- JetBrains 1.40, Hack 1.32, Adwaita 1.44, Liberation 1.58 --
so this takes the middle at 1.44.  Contrast drops a little, as it does
in any bold: thick strokes crowd a counter, and holding the regular's
modulation would close them."
  (setf (d-stem d)        (round (* (d-stem d) 1.44))
        (d-thin d)        (round (* (d-stem d) 0.76))
        (d-sidebearing d) 42
        (d-dot-radius d)  (round (* (d-dot-radius d) 1.18))
        (d-rule-weight d) (round (* (d-rule-weight d) 1.36))
        (d-serif d)       (round (* (d-serif d) 0.94))
        (d-crowd d)       0.83d0
        (d-weight d)      700
        (d-style d)       (if (d-italic d) "Bold Italic" "Bold"))
  d)

(defun italicise (d &key (slant -10))
  "Turn a cut into its italic.  The slope is applied to outlines; the
flag switches the letters that are genuinely redrawn."
  (setf (d-slant d) slant (d-italic d) t
        (d-style d) (if (= (d-weight d) 700) "Bold Italic" "Italic"))
  d)

(defun dark-cut (&key (grade 4) &allow-other-keys)
  "The graded cut for light-on-dark.  GRADE units of ink come off each
side of every stroke.  Advance widths and every vertical metric are
untouched, so the two cuts are swappable without reflowing a column."
  (let ((d (make-design :family "Cadr Mono Dark" :style "Regular" :grade grade)))
    (setf (d-stem d)        (- (d-stem d) (* 2 grade))
          (d-thin d)        (- (d-thin d) (* 2 grade))
          (d-dot-radius d)  (- (d-dot-radius d) grade)
          (d-braille-dot d) (- (d-braille-dot d) grade)
          (d-rule-weight d) (- (d-rule-weight d) (* 2 grade)))
    d))
