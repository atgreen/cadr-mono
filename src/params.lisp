;;;; params.lisp -- the design parameter set.
;;;;
;;;; This is the surface you adjust.  Every glyph in the face is a pure
;;;; function of these numbers, so changing one here changes it everywhere
;;;; and consistently.  All values are in font units at 1000 per em.

(in-package #:muskako)

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
  (x-height    500)
  (ascender    760)   ; b d f h k l
  (descender  -230)   ; g j p q y
  (overshoot    12)   ; round letters exceed flat ones by this

  ;; --- stroke --------------------------------------------------------
  (stem         88)   ; vertical stroke weight.  The grade moves this.
  (thin         62)   ; horizontal stroke weight (88 * 0.70)
  (stress-angle -16)  ; degrees.  Humanist: the thin spots tilt off vertical.
  (tension    0.60d0) ; bowl flatness.  0.5523 is a true ellipse; higher
                      ; squares the shoulders the way drawn bowls do.

  ;; --- humanist detailing --------------------------------------------
  (aperture     34)   ; degrees of arc cut away on c e s.  Bigger = more open.
  (terminal-cut 22)   ; degrees.  Angled stroke ends, not flat ones.
  (shoulder-ease 0.54); arch radius of n m h u r, as a fraction of x-height
  (tail         110)  ; centreline radius of the tail on l and t
  (dot-radius    54)  ; i j, punctuation, and the mark inside zero
  (serif        190)  ; half-length of the serif arms on i j l.  A bare
                      ; stem leaves four times the sidebearing of a round
                      ; letter and floats in its cell; narrow glyphs have
                      ; to earn their width in a monospace.

  ;; --- disambiguation choices ----------------------------------------
  (zero-mark    :dot) ; :dot :slash or :none
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

  ;; --- provenance ----------------------------------------------------
  (family "Muskako")
  (style  "Regular")
  (grade     0))      ; units of ink removed per side; 0 is the reference cut

(defvar *design* (make-design)
  "The design currently being generated.")

(defmacro with-design ((design) &body body)
  `(let ((*design* ,design)) ,@body))

(defparameter +metric-slots+
  '(:advance :cap :x :asc :desc :over :stem :thin :tail :dot :serif
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
    (:over (d-overshoot *design*))     (:stem (d-stem *design*))
    (:thin (d-thin *design*))          (:stress (d-stress-angle *design*))
    (:aperture (d-aperture *design*))
    (:ease (d-shoulder-ease *design*)) (:tail (d-tail *design*))
    (:tension (d-tension *design*))
    (:dot (d-dot-radius *design*))     (:zero (d-zero-mark *design*))
    (:serif (d-serif *design*))
    (:one-serif (d-one-serif *design*)) (:l-tail (d-l-tail *design*))
    (:br-dot (d-braille-dot *design*)) (:br-inset (d-braille-inset *design*))
    (:br-top (d-braille-top *design*)) (:br-pitch (d-braille-pitch *design*))
    (:rule (d-rule-weight *design*))
    (:family (d-family *design*))      (:style (d-style *design*))
    (:grade (d-grade *design*))))

(defun p (slot)
  "Read a design parameter, e.g. (p :stem).  Lengths come back in units
of the current em; everything else comes back as written."
  (let ((v (raw-p slot)))
    (if (member slot +metric-slots+) (u v) v)))

(defun light-cut (&rest overrides)
  "The reference cut, drawn for dark-on-light."
  (apply #'make-design :style "Regular" :grade 0 overrides))

(defun dark-cut (&key (grade 4) &allow-other-keys)
  "The graded cut for light-on-dark.  GRADE units of ink come off each
side of every stroke.  Advance widths and every vertical metric are
untouched, so the two cuts are swappable without reflowing a column."
  (let ((d (make-design :style "Dark" :grade grade)))
    (setf (d-stem d)        (- (d-stem d) (* 2 grade))
          (d-thin d)        (- (d-thin d) (* 2 grade))
          (d-dot-radius d)  (- (d-dot-radius d) grade)
          (d-braille-dot d) (- (d-braille-dot d) grade)
          (d-rule-weight d) (- (d-rule-weight d) (* 2 grade)))
    d))
