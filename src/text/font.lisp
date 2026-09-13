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
    (:fixedsys-excelsior "fixedsys-excelsior/FSEX301-L2.ttf"                16 t)
    (:greybeard          "greybeard/Greybeard-16px.ttf"                     16 t)
    (:commodore-pet      "pet-me/PetMe.ttf"                                  8 t)
    (:commodore-64       "pet-me/PetMe64.ttf"                                8 t)
    (:gohu               "gohu/GohuFont11NerdFontMono-Regular.ttf"          11 t)
    (:cozette            "cozette/CozetteVector.ttf"                        13 t)
    (:unscii-8           "unscii/unscii-8.ttf"                               8 t)
    (:unscii-8-thin      "unscii/unscii-8-thin.ttf"                          8 t)
    (:unscii-16          "unscii/unscii-16-full.ttf"                        16 t)
    (:apple-ii           "apple2/PrintChar21.ttf"                            8 t)
    (:atari-400          "atari-400-800/AtariClassic-Regular.ttf"            8 t)
    (:ibm-ega-8x8        "oldschool-pc-fonts/PxPlus_IBM_EGA_8x8.ttf"         8 t)
    (:ibm-vga-8x16       "oldschool-pc-fonts/PxPlus_IBM_VGA_8x16.ttf"       16 t)
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
  "(KEYWORD RELATIVE-PATH NATIVE-PIXEL-SIZE LOW-RESOLUTION-P).

NATIVE-PIXEL-SIZE is the size a low-resolution face is designed for and must be
rendered at: magnifying its native pixels by a whole number is what keeps them
square, and rasterising it at some other size is what makes it look like a
blurry version of itself.")

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

(defun bundled-font-path (name)
  (let ((entry (assoc name +bundled-fonts+)))
    (unless entry (error "No bundled font named ~S." name))
    (util:resource (concatenate 'string "fonts/" (second entry)))))

(defun bundled-font-native-size (name)
  (third (assoc name +bundled-fonts+)))

(defun bundled-font-low-resolution-p (name)
  (fourth (assoc name +bundled-fonts+)))

(defun load-bundled-font (name &key pixel-size)
  "Load a bundled face by keyword, at its native size unless told otherwise."
  (load-font (bundled-font-path name)
             :pixel-size (or pixel-size (bundled-font-native-size name) 16)))
