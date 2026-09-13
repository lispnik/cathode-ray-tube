;;;; src/text/font.lisp -- CoreText fonts, from files, without installing them.
;;;;
;;;; THE SPLIT THIS FILE IS BUILT AROUND: load with foreign-funcall, MEASURE
;;;; with objc:invoke.
;;;;
;;;; CoreText's loading calls take and return pointers and integers, so CFFI
;;;; handles them.  Its METRICS calls do not: CTFontGetBoundingRectsForGlyphs
;;;; returns a CGRect by value and CTFontGetAdvancesForGlyphs takes and returns
;;;; a CGSize, and CFFI without libffi can express neither.  But a CTFontRef IS
;;;; an NSFont -- toll-free bridged -- and -[NSFont boundingRectForGlyph:] and
;;;; -advancementForGlyph: return NSRect and NSSize, which are two of the four
;;;; structures the objc bridge converts natively.  So the same font object is
;;;; used through two different doors, one for each kind of call.

(in-package #:cathode-ray-tube.text)

(defstruct (font (:constructor %make-font (handle pixel-size)))
  (handle nil)
  (pixel-size 0 :type real)
  (ascent 0d0 :type double-float)
  (descent 0d0 :type double-float)
  (leading 0d0 :type double-float)
  (cell-width 0d0 :type double-float)
  (cell-height 0d0 :type double-float))

(defun font-from-file (pathname size)
  "A retained NSFont/CTFont for the first face in PATHNAME at SIZE, or NIL.

CoreText makes a font straight from a file, so none of the twenty-six faces this
program ships has to be installed, registered, or looked up by name -- which
also means a user's own font with the same family name cannot shadow ours."
  (metal:with-metal
    (let* ((url (objc:invoke "NSURL" "fileURLWithPath:"
                             (uiop:native-namestring pathname)))
           (descriptors (cffi:foreign-funcall "CTFontManagerCreateFontDescriptorsFromURL"
                                              :pointer (objc:objc-object-pointer url)
                                              :pointer)))
      (unless (cffi:null-pointer-p descriptors)
        (unwind-protect
             (when (plusp (cffi:foreign-funcall "CFArrayGetCount"
                                                :pointer descriptors :long))
               (let ((descriptor (cffi:foreign-funcall "CFArrayGetValueAtIndex"
                                                       :pointer descriptors :long 0
                                                       :pointer)))
                 ;; Create means it arrives retained.
                 (cffi:foreign-funcall "CTFontCreateWithFontDescriptor"
                                       :pointer descriptor :double (float size 1d0)
                                       :pointer (cffi:null-pointer) :pointer)))
          (cffi:foreign-funcall "CFRelease" :pointer descriptors :void))))))

(defun font-from-family (family size)
  "A retained CTFont for an INSTALLED family, or NIL where there is no such one.

For the system fallback and nothing else.  -[NSFont fontWithName:size:] answers
nil for a family that is not installed, which is the honest answer and the one
FONT-FALLBACK-CHAIN wants -- a machine without Menlo should lose its last-resort
fallback, not its window.

Retained here so that RELEASE-FONT can treat every font the same way, however it
arrived: -fontWithName: returns an autoreleased object and the rest of this file
returns Create-rule ones."
  (metal:with-metal
    (let ((font (objc:invoke "NSFont" "fontWithName:size:" family (float size 1d0))))
      (unless (or (null font) (metal:null-object-p font))
        (objc:retain font)
        (objc:objc-object-pointer font)))))

(defun system-monospace-families ()
  "Every installed monospace family, sorted.

fontSource 1 in a profile means `not one of ours, one of the machine's', and
this is the list it names into.  Upstream asks QFontDatabase for every family
and keeps the fixed-pitch ones (fontmanager.cpp:27-42); -[NSFont isFixedPitch]
is the same question asked of AppKit, which is better than the alternative of
remembering that NSFixedPitchFontMask is 1024.

Slow enough to be worth not calling per frame -- it makes one NSFont per
installed family -- and fast enough that caching it would mean deciding when to
notice a font being installed.  The settings window asks once when it opens."
  (metal:with-metal
    (let ((manager (objc:invoke "NSFontManager" "sharedFontManager"))
          (families '()))
      (let* ((array (objc:invoke manager "availableFontFamilies"))
             (count (objc:invoke-into 'integer array "count")))
        (dotimes (i count)
          (let* ((family (objc:invoke-into 'string array "objectAtIndex:" i))
                 (font (and family
                            (objc:invoke "NSFont" "fontWithName:size:" family 12d0))))
            (when (and font (not (metal:null-object-p font))
                       (objc:invoke-bool font "isFixedPitch"))
              (push family families)))))
      (sort families #'string-lessp))))

(defun load-family-font (family &optional (pixel-size 16))
  "An installed family as a FONT, measured, or NIL."
  (let ((handle (font-from-family family pixel-size)))
    (when handle (measure-font (%make-font handle pixel-size)))))

(defun glyph-for-character (font character)
  "The glyph id for CHARACTER, or 0 when the face has no glyph for it.

UTF-16 only: CTFontGetGlyphsForCharacters takes UniChars, so anything outside
the basic multilingual plane needs a surrogate pair and two glyph slots.  Those
characters are returned as 0 here and fall through to the substitution chain,
which is the right answer for a terminal whose marquee fonts are 8x8 bitmaps."
  (let ((code (char-code character)))
    (if (> code #xFFFF)
        0
        (cffi:with-foreign-objects ((characters :uint16) (glyphs :uint16))
          (setf (cffi:mem-ref characters :uint16) code)
          (if (cffi:foreign-funcall "CTFontGetGlyphsForCharacters"
                                    :pointer (font-handle font)
                                    :pointer characters :pointer glyphs
                                    :long 1 :bool)
              (cffi:mem-ref glyphs :uint16)
              0)))))

(defun font-advance (font glyph)
  "GLYPH's horizontal advance, through NSFont rather than CoreText.

-advancementForGlyph: returns an NSSize, which the bridge converts.
CTFontGetAdvancesForGlyphs returns a CGSize by value, which it cannot."
  (metal:with-metal
    (let ((size (objc:invoke-into 'vector (font-handle font)
                                  "advancementForGlyph:" glyph)))
      (aref size 0))))

(defun font-glyph-bounds (font glyph)
  "(values X Y WIDTH HEIGHT) of GLYPH's bounding box, in font units at this size.

Y is the offset of the box's BOTTOM from the baseline, so it is negative for a
glyph with a descender."
  (metal:with-metal
    (let ((rect (objc:invoke-into 'vector (font-handle font)
                                  "boundingRectForGlyph:" glyph)))
      (values (aref rect 0) (aref rect 1) (aref rect 2) (aref rect 3)))))

(defun measure-font (font)
  "Fill in FONT's ascent, descent and cell size.

The cell is sized from \"M\", which is what cool-retro-term's computeBaseWidth
measures, and the height from the ascent and descent rather than from any
glyph -- a monospace terminal cell is a property of the face, not of whatever
happens to be in it."
  (metal:with-metal
    (let ((handle (font-handle font)))
      (setf (font-ascent font) (abs (objc:invoke-into 'double-float handle "ascender"))
            (font-descent font) (abs (objc:invoke-into 'double-float handle "descender"))
            (font-leading font) (abs (objc:invoke-into 'double-float handle "leading")))
      (let* ((m (glyph-for-character font #\M))
             (advance (if (plusp m) (font-advance font m) 0d0)))
        (setf (font-cell-width font)
              (if (plusp advance)
                  (float advance 1d0)
                  ;; A face with no M at all: fall back to something sane rather
                  ;; than a zero-width cell, which would divide by zero when the
                  ;; grid is computed.
                  (* 0.6d0 (font-pixel-size font)))
              (font-cell-height font)
              (max 1d0 (+ (font-ascent font) (font-descent font)))))))
  font)

(defun load-font (pathname &key (pixel-size 16))
  (let ((handle (font-from-file pathname pixel-size)))
    (when (and handle (not (cffi:null-pointer-p handle)))
      (measure-font (%make-font handle pixel-size)))))

(defun release-font (font)
  (when (and font (font-handle font))
    (metal:with-metal
      (cffi:foreign-funcall "CFRelease" :pointer (font-handle font) :void))
    (setf (font-handle font) nil)))

;;; The bundled faces -----------------------------------------------------------
;;;
;;; Transcribed from cool-retro-term's app/fontmanager.cpp.  The full table with
;;; every face's base width, fallback chain and low-resolution flag arrives with
;;; the font manager; what is here is the file layout, which is enough to load
;;; any of them by name.

(defparameter +bundled-fonts+
  '((:terminess          "terminus/TerminessNerdFontMono-Regular.ttf"       12 t)
    (:bigblue-terminal   "bigblue-terminal/BigBlueTerm437NerdFontMono-Regular.ttf" 12 t)
    (:fixedsys-excelsior "fixedsys-excelsior/FSEX301-L2.ttf"                16 t :unscii-16)
    (:greybeard          "greybeard/Greybeard-16px.ttf"                     16 t :unscii-16)
    (:commodore-pet      "pet-me/PetMe.ttf"                                  8 t :unscii-8)
    (:commodore-64       "pet-me/PetMe64.ttf"                                8 t :unscii-8)
    (:gohu               "gohu/GohuFont11NerdFontMono-Regular.ttf"          11 t)
    (:cozette            "cozette/CozetteVector.ttf"                        13 t)
    (:unscii-8           "unscii/unscii-8.ttf"                               8 t)
    (:unscii-8-thin      "unscii/unscii-8-thin.ttf"                          8 t :unscii-8)
    (:unscii-16          "unscii/unscii-16-full.ttf"                        16 t)
    (:apple-ii           "apple2/PrintChar21.ttf"                            8 t :unscii-8)
    (:atari-400          "atari-400-800/AtariClassic-Regular.ttf"            8 t :unscii-8)
    (:ibm-ega-8x8        "oldschool-pc-fonts/PxPlus_IBM_EGA_8x8.ttf"         8 t :unscii-8)
    (:ibm-vga-8x16       "oldschool-pc-fonts/PxPlus_IBM_VGA_8x16.ttf"       16 t :unscii-16)
    (:departure-mono     "departure-mono/DepartureMonoNerdFontMono-Regular.otf" 11 t)
    ;; The "modern" faces: rendered at the target pixel height with no
    ;; magnification, and the only ones offered when rasterisation is Modern.
    (:terminess-hi       "terminus/TerminessNerdFontMono-Regular.ttf"       32 nil)
    (:hack               "hack/HackNerdFontMono-Regular.ttf"                32 nil)
    (:fira-code          "fira-code/FiraCodeNerdFontMono-Regular.ttf"       32 nil)
    (:iosevka            "iosevka/IosevkaTermNerdFontMono-Regular.ttf"      32 nil)
    (:jetbrains-mono     "jetbrains-mono/JetBrainsMonoNerdFontMono-Regular.ttf" 32 nil)
    (:ibm-3278           "ibm-3278/3270NerdFontMono-Regular.ttf"            32 nil)
    (:source-code-pro    "source-code-pro/SauceCodeProNerdFontMono-Regular.ttf" 32 nil)
    (:opendyslexic       "opendyslexic/OpenDyslexicMNerdFontMono-Regular.otf" 32 nil))
  "(KEYWORD RELATIVE-PATH NATIVE-PIXEL-SIZE LOW-RESOLUTION-P &optional FALLBACK).

NATIVE-PIXEL-SIZE is the size a low-resolution face is designed for and must be
rendered at: magnifying its native pixels by a whole number is what keeps them
square, and rasterising it at some other size is what makes it look like a
blurry version of itself.

FALLBACK is the face to ask when this one has no glyph, and the column is
upstream's, transcribed from the addBundledFont calls at fontmanager.cpp:201-379.
It matters more than it looks: these are 8x16 bitmap designs from machines that
had 128 characters, and a terminal shows whatever bytes arrive.  Without it a
Commodore PET running anything that draws a box gets a screen of blanks, which
reads as the program being broken rather than as the font being from 1977.

Upstream names UNSCII_8 and UNSCII_16 as their own fallbacks and then skips the
substitution when it equals the face; the column is simply left empty here, which
is the same thing said once instead of twice.")

(defparameter *system-fallback-family* "Menlo"
  "The last resort, after a face's own fallback.

fontmanager.cpp:509 appends it on macOS and `Monospace' everywhere else, so this
is the whole of that conditional.  A variable rather than a constant because a
machine without Menlo is a machine where the right answer is to say so and carry
on, and because the tests need to be able to ask for a face that does not exist.")

(defparameter +profile-font-names+
  '(("TERMINESS_SCALED"        . :terminess)
    ("BIGBLUE_TERMINAL_SCALED" . :bigblue-terminal)
    ("EXCELSIOR_SCALED"        . :fixedsys-excelsior)
    ("GREYBEARD_SCALED"        . :greybeard)
    ("COMMODORE_PET_SCALED"    . :commodore-pet)
    ("GOHU_11_SCALED"          . :gohu)
    ("COZETTE_SCALED"          . :cozette)
    ("UNSCII_8_SCALED"         . :unscii-8)
    ("UNSCII_8_THIN_SCALED"    . :unscii-8-thin)
    ("UNSCII_16_SCALED"        . :unscii-16)
    ("APPLE_II_SCALED"         . :apple-ii)
    ("ATARI_400_SCALED"        . :atari-400)
    ("COMMODORE_64_SCALED"     . :commodore-64)
    ("IBM_EGA_8x8"             . :ibm-ega-8x8)
    ("IBM_VGA_8x16"            . :ibm-vga-8x16)
    ("DEPARTURE_MONO_SCALED"   . :departure-mono)
    ;; The eight "modern" faces, offered only when rasterisation is Modern.
    ("TERMINESS"               . :terminess-hi)
    ("HACK"                    . :hack)
    ("FIRA_CODE"               . :fira-code)
    ("IOSEVKA"                 . :iosevka)
    ("JETBRAINS_MONO"          . :jetbrains-mono)
    ("IBM_3278"                . :ibm-3278)
    ("SOURCE_CODE_PRO"         . :source-code-pro)
    ("OPENDYSLEXIC"            . :opendyslexic))
  "cool-retro-term's fontName values, as they appear in a profile.

The names are upstream's, unchanged, because a profile file has to be readable
by both programs -- see the note on +PROFILE-KEYS+.  The _SCALED suffix marks
the LOW-RESOLUTION faces, which is a fact about how they are rendered rather
than about the file: they are bitmap designs drawn for one pixel size and
magnified by a whole number, never rasterised larger.")

(defun font-for-profile-name (name)
  "The bundled face a profile's fontName means, or NIL.

NIL rather than an error: a profile from a newer cool-retro-term may name a face
this build does not ship, and falling back to the default is a better answer
than refusing to open the window."
  (cdr (assoc name +profile-font-names+ :test #'string-equal)))

(defparameter +font-display-names+
  '((:terminess          . "Terminess")
    (:bigblue-terminal   . "BigBlue Terminal")
    (:fixedsys-excelsior . "Fixedsys Excelsior")
    (:greybeard          . "Greybeard")
    (:commodore-pet      . "Commodore PET")
    (:gohu               . "Gohu 11")
    (:cozette            . "Cozette")
    (:unscii-8           . "Unscii 8")
    (:unscii-8-thin      . "Unscii 8 Thin")
    (:unscii-16          . "Unscii 16")
    (:apple-ii           . "Apple ][")
    (:atari-400          . "Atari 400-800")
    (:commodore-64       . "Commodore 64")
    (:ibm-ega-8x8        . "IBM EGA 8x8")
    (:ibm-vga-8x16       . "IBM VGA 8x16")
    (:departure-mono     . "Departure Mono")
    (:terminess-hi       . "Terminess")
    (:hack               . "Hack")
    (:fira-code          . "Fira Code")
    (:iosevka            . "Iosevka")
    (:jetbrains-mono     . "JetBrains Mono")
    (:ibm-3278           . "IBM 3278")
    (:source-code-pro    . "Source Code Pro")
    (:opendyslexic       . "OpenDyslexic"))
  "What a face is CALLED, as against what a profile file calls it.

addBundledFont's second argument, transcribed: the settings window shows these
and the file records the FONTNAME beside them.  Two of them are `Terminess',
which is upstream's own doing -- the scaled and unscaled entries are the same
design offered twice, and the two are never in the same list because
rasterisation decides which one is.")

(defun font-display-name-for (face)
  "FACE's human name, or its keyword printed if the table has no row."
  (or (cdr (assoc face +font-display-names+))
      (string-capitalize (symbol-name face))))

(defun profile-name-for-display (display)
  "The fontName a profile should record for the face DISPLAY names.

The inverse of FONT-DISPLAY-NAME-FOR through the keyword, and the reason the
settings window can show `Commodore PET' while the file says
COMMODORE_PET_SCALED -- which is what makes the file readable by both programs."
  (let ((face (car (rassoc display +font-display-names+ :test #'string=))))
    (or (car (rassoc face +profile-font-names+))
        display)))

(defun bundled-font-display-names (&key modern)
  "The faces to offer, as names a person would recognise.

MODERN picks the outline faces over the bitmap ones, which is upstream's
rasterisation filter: `modernMode == !font.lowResolutionFont'
(fontmanager.cpp:441).  Offering all twenty-four at once would let you choose a
32-pixel outline face in a profile that magnifies bitmaps, which is a
combination that has no sensible rendering."
  (let ((wanted '()))
    (dolist (entry +bundled-fonts+ (nreverse wanted))
      (let ((face (first entry))
            (low-resolution (fourth entry)))
        (when (eq (not modern) (and low-resolution t))
          (push (font-display-name-for face) wanted))))))

(defun bundled-font-path (name)
  (let ((entry (assoc name +bundled-fonts+)))
    (unless entry (error "No bundled font named ~S." name))
    (util:resource (concatenate 'string "fonts/" (second entry)))))

(defun bundled-font-native-size (name)
  (third (assoc name +bundled-fonts+)))

(defun bundled-font-low-resolution-p (name)
  (fourth (assoc name +bundled-fonts+)))

(defun bundled-font-fallback (name)
  "The face to ask when NAME has no glyph, or NIL."
  (fifth (assoc name +bundled-fonts+)))

(defun font-fallback-chain (name &key (pixel-size (and name (bundled-font-native-size name))))
  "The faces to try after NAME, in order, already loaded at PIXEL-SIZE.

The face's own fallback first and the system monospace last, which is
fontmanager.cpp:499-513 exactly.  Loaded at the PRIMARY face's pixel size rather
than at their own: a glyph borrowed from another face has to sit in this face's
cell, and one rasterised at a different size would be visibly a different size.

Anything that will not load is dropped rather than signalled.  A missing fallback
means a blank glyph, which is where this started; it is not a reason to refuse to
open a window."
  (let ((chain '()))
    ;; NAME may be NIL -- a system family, which has no table entry and so no
    ;; declared fallback.  It still gets the system one, which is the whole of
    ;; what upstream gives a system font too.
    (let ((own (and name (bundled-font-fallback name))))
      (when (and own (not (eq own name)))
        (let ((font (ignore-errors (load-bundled-font own :pixel-size pixel-size))))
          (when font (push font chain)))))
    (when *system-fallback-family*
      (let ((font (ignore-errors (load-family-font *system-fallback-family*
                                                   pixel-size))))
        (when font (push font chain))))
    (nreverse chain)))

(defun load-bundled-font (name &key pixel-size)
  "Load a bundled face by keyword, at its native size unless told otherwise."
  (load-font (bundled-font-path name)
             :pixel-size (or pixel-size (bundled-font-native-size name) 16)))
