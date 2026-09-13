;;;; src/text/atlas.lisp -- glyphs, rasterised once and kept on the GPU.
;;;;
;;;; One R8Unorm texture of coverage values, shelf-packed.  A terminal's working
;;;; set is tiny -- a few hundred glyphs covers almost any session -- so there is
;;;; no eviction and no LRU: the atlas fills, and if it ever filled completely
;;;; the right answer would be a second page rather than throwing work away.
;;;;
;;;; SLOT (0,0) IS A SOLID BLOCK, deliberately.  Underlines, strikethroughs, the
;;;; block cursor and every cell background are drawn as quads that sample it, so
;;;; the whole text pass is ONE pipeline and one texture binding.  A separate
;;;; untextured pipeline for rectangles would be a second pipeline, a second
;;;; encoder state change, and a second thing to get wrong.

(in-package #:cathode-ray-tube.text)

(defstruct glyph
  "Where a rasterised glyph is in the atlas, and how to place it.

U and V are normalised texture coordinates.  BEARING-X and BEARING-Y are the
offset from the pen position to the top-left of the bitmap, in pixels, with Y
measured DOWN from the baseline -- which is the direction the text shader works
in, and the opposite of the direction CoreText reports."
  (u0 0.0 :type single-float) (v0 0.0 :type single-float)
  (u1 0.0 :type single-float) (v1 0.0 :type single-float)
  (width 0 :type fixnum) (height 0 :type fixnum)
  (bearing-x 0.0 :type single-float) (bearing-y 0.0 :type single-float)
  (advance 0.0 :type single-float))

(defstruct (atlas (:constructor %make-atlas (width height texture pixels)))
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  texture
  (pixels nil :type (or null (simple-array (unsigned-byte 8) (*))))
  ;; The shelf allocator: a current row at PEN-Y of height SHELF-HEIGHT, filling
  ;; left to right from PEN-X.  When a glyph will not fit the row, a new shelf
  ;; starts below.  Crude, and exactly right for glyphs, which are all roughly
  ;; the same height.
  (pen-x 0 :type fixnum)
  (pen-y 0 :type fixnum)
  (shelf-height 0 :type fixnum)
  (glyphs (make-hash-table :test 'equal) :type hash-table)
  (dirty nil)
  (solid nil))

(defconstant +atlas-padding+ 1
  "A transparent pixel between glyphs, so that linear filtering at the edge of
one cannot pick up its neighbour.")

(defun make-atlas (&key (width 2048) (height 2048))
  (let* ((pixels (make-array (* width height) :element-type '(unsigned-byte 8)
                                              :initial-element 0))
         (texture (metal:make-texture :width width :height height
                                      :pixel-format metal:+pixel-format-r8unorm+
                                      :usage metal:+usage-shader-read+
                                      :storage metal:+storage-mode-shared+
                                      :label "glyph atlas"))
         (atlas (%make-atlas width height texture pixels)))
    (setf (atlas-solid atlas) (allocate-solid atlas))
    atlas))

(defun release-atlas (atlas)
  (when (atlas-texture atlas)
    (metal:release-texture (atlas-texture atlas))
    (setf (atlas-texture atlas) nil)))

(defun allocate-solid (atlas)
  "A 2x2 fully-opaque block at the origin.

Two pixels rather than one so that sampling its CENTRE is unambiguous however
the rasteriser rounds."
  (multiple-value-bind (x y) (atlas-allocate atlas 2 2)
    (let ((pixels (atlas-pixels atlas)) (width (atlas-width atlas)))
      (dotimes (row 2)
        (dotimes (col 2)
          (setf (aref pixels (+ (* (+ y row) width) x col)) 255))))
    (setf (atlas-dirty atlas) t)
    (make-glyph :u0 (/ (+ x 0.5) (float (atlas-width atlas)))
                :v0 (/ (+ y 0.5) (float (atlas-height atlas)))
                :u1 (/ (+ x 1.5) (float (atlas-width atlas)))
                :v1 (/ (+ y 1.5) (float (atlas-height atlas)))
                :width 2 :height 2)))

(defun atlas-allocate (atlas width height)
  "Reserve a WIDTH by HEIGHT rectangle.  (values X Y), or signals when full."
  (let ((pad +atlas-padding+))
    (when (> (+ (atlas-pen-x atlas) width pad) (atlas-width atlas))
      ;; Next shelf.
      (setf (atlas-pen-x atlas) 0
            (atlas-pen-y atlas) (+ (atlas-pen-y atlas) (atlas-shelf-height atlas) pad)
            (atlas-shelf-height atlas) 0))
    (when (> (+ (atlas-pen-y atlas) height pad) (atlas-height atlas))
      (error "The glyph atlas is full (~Dx~D)." (atlas-width atlas)
             (atlas-height atlas)))
    (let ((x (atlas-pen-x atlas)) (y (atlas-pen-y atlas)))
      (incf (atlas-pen-x atlas) (+ width pad))
      (setf (atlas-shelf-height atlas) (max (atlas-shelf-height atlas) height))
      (values x y))))

;;; Rasterising --------------------------------------------------------------------

(defconstant +alpha-only+ 7 "kCGImageAlphaOnly.")

(defun rasterise-glyph (atlas font character &key bold italic)
  "Draw CHARACTER into the atlas and return its GLYPH.  Main thread only.

BOLD and ITALIC are SYNTHESISED rather than taken from another face, because
these faces do not have one: PxPlus_IBM_VGA_8x16 and PetMe ship a single weight
and no oblique, which is true of every bitmap font here and of most of the
outline ones as we bundle them.  Emboldening by drawing twice a pixel apart and
slanting by shearing the pen are what a terminal has always done when the face
had nothing else to offer, and it is what qmltermwidget does too."
  (metal:with-metal
    (let ((glyph-id (glyph-for-character font character)))
      (if (zerop glyph-id)
          ;; No glyph in this face.  A blank entry rather than an error: a
          ;; terminal is shown whatever bytes arrive, and one unmapped character
          ;; must not take down the frame.
          (make-glyph :advance (float (font-cell-width font) 1.0))
          (multiple-value-bind (bx by bw bh) (font-glyph-bounds font glyph-id)
            (let* ((pad 1)
                   ;; Bold widens the ink by one pixel and italic leans it over
                   ;; by up to a third of its height, so the box grows to match
                   ;; or the emboldened right edge and the slanted top are
                   ;; clipped -- which looks like a broken font rather than a
                   ;; box that is one pixel too small.
                   (extra-x (+ (if bold 1 0)
                               (if italic (ceiling (* 0.25 (ceiling bh))) 0)))
                   (width (+ (ceiling bw) (* 2 pad) extra-x))
                   (height (+ (ceiling bh) (* 2 pad)))
                   (advance (font-advance font glyph-id)))
              (if (or (<= width (* 2 pad)) (<= height (* 2 pad)))
                  ;; A space, or anything else with no ink.
                  (make-glyph :advance (float advance 1.0))
                  (multiple-value-bind (x y) (atlas-allocate atlas width height)
                    (draw-glyph-into atlas font glyph-id x y width height
                                     (- (floor bx) pad)
                                     (- (+ (floor by) (ceiling bh)) (- pad))
                                     :bold bold :italic italic)
                    (setf (atlas-dirty atlas) t)
                    (let ((w (float (atlas-width atlas)))
                          (h (float (atlas-height atlas))))
                      (make-glyph :u0 (/ x w) :v0 (/ y h)
                                  :u1 (/ (+ x width) w) :v1 (/ (+ y height) h)
                                  :width width :height height
                                  ;; CoreText measures Y UP from the baseline;
                                  ;; the shader places glyphs measuring DOWN from
                                  ;; it.  Hence the negation, and it is the one
                                  ;; place in the atlas where the sign matters.
                                  :bearing-x (float (- (floor bx) pad) 1.0)
                                  :bearing-y (float (- (+ (floor by) (ceiling bh))
                                                       (- pad))
                                                    1.0)
                                  :advance (float advance 1.0)))))))))))

(defun draw-glyph-into (atlas font glyph-id x y width height origin-x origin-y
                        &key bold italic)
  "Rasterise GLYPH-ID into ATLAS's pixel array at X, Y."
  (let ((bytes (* width height)))
    (cffi:with-foreign-object (bitmap :uint8 bytes)
      (dotimes (i bytes) (setf (cffi:mem-aref bitmap :uint8 i) 0))
      ;; kCGImageAlphaOnly: 8 bits of coverage, no colour space.  Exactly what an
      ;; R8Unorm atlas wants, and a quarter of the memory of drawing into RGBA.
      (let ((context (cffi:foreign-funcall
                      "CGBitmapContextCreate"
                      :pointer bitmap :unsigned-long width :unsigned-long height
                      :unsigned-long 8 :unsigned-long width
                      :pointer (cffi:null-pointer) :unsigned-int +alpha-only+
                      :pointer)))
        (unless (cffi:null-pointer-p context)
          (unwind-protect
               (cffi:with-foreign-objects ((glyphs :uint16) (positions :double 2))
                 (setf (cffi:mem-ref glyphs :uint16) glyph-id
                       ;; The pen goes at minus the bounding box's origin, so
                       ;; that the glyph's ink lands inside the bitmap.
                       (cffi:mem-aref positions :double 0) (float (- origin-x) 1d0)
                       (cffi:mem-aref positions :double 1)
                       (float (- height origin-y) 1d0))
                 (cffi:foreign-funcall "CTFontDrawGlyphs"
                                       :pointer (font-handle font)
                                       :pointer glyphs :pointer positions
                                       :unsigned-long 1 :pointer context :void)
                 (when bold
                   ;; Drawn again one pixel to the right.  Not a heavier face --
                   ;; there is not one -- but the same trick every terminal has
                   ;; used for the same reason.
                   (setf (cffi:mem-aref positions :double 0)
                         (float (- 1 origin-x) 1d0))
                   (cffi:foreign-funcall "CTFontDrawGlyphs"
                                         :pointer (font-handle font)
                                         :pointer glyphs :pointer positions
                                         :unsigned-long 1 :pointer context :void)))
            (cffi:foreign-funcall "CGContextRelease" :pointer context :void))
          ;; MEASURED, not reasoned about.  CoreGraphics DRAWS with the origin
          ;; at the bottom left, which invites the conclusion that the rows come
          ;; out upside down and have to be reversed here.  They do not: a
          ;; bitmap context's MEMORY is laid out top-down, so row 0 of the buffer
          ;; is row 0 of the atlas.  Reversing produced a terminal in which every
          ;; line was in the right place, every glyph was in the right cell, and
          ;; every letterform was mirrored -- which looks far more like a
          ;; texture-coordinate bug than like what it was.
          ;;
          ;; ITALIC IS SHEARED HERE, in the row copy, rather than by the text
          ;; matrix.  CGContextSetTextMatrix takes a CGAffineTransform BY VALUE:
          ;; six doubles, which is more than the four an AAPCS64 homogeneous
          ;; float aggregate may have, so it is passed INDIRECTLY -- and a
          ;; cffi:foreign-funcall handing over six loose doubles puts them in
          ;; the wrong registers entirely.  The call did nothing, silently, and
          ;; the test caught it only because a sheared M has the same ink as an
          ;; upright one and I had to look at why.
          ;;
          ;; Shearing the bitmap needs no ABI at all: row 0 is the top and
          ;; leans furthest right, the bottom row not at all.
          (let ((pixels (atlas-pixels atlas))
                (aw (atlas-width atlas))
                (slant (if italic 0.25 0.0)))
            (dotimes (row height)
              (let ((source (* row width))
                    (destination (+ (* (+ y row) aw) x))
                    (shift (round (* slant (- height 1 row)))))
                (dotimes (col width)
                  (let ((target (+ col shift)))
                    (when (< target width)
                      (setf (aref pixels (+ destination target))
                            (cffi:mem-aref bitmap :uint8 (+ source col))))))))))))))

(defun atlas-glyph (atlas font character &key bold italic)
  "CHARACTER's glyph, rasterising it on first sight.

Keyed on the font's handle AND the style as well as the character: two faces, or
the same face at two sizes, are different fonts, and a bold A is a different
picture from a plain one."
  (let ((key (list (cffi:pointer-address (font-handle font))
                   (char-code character)
                   (and bold t) (and italic t))))
    (or (gethash key (atlas-glyphs atlas))
        (setf (gethash key (atlas-glyphs atlas))
              (rasterise-glyph atlas font character :bold bold :italic italic)))))

(defun atlas-flush (atlas)
  "Upload anything newly rasterised.

Called once a frame, BEFORE the pass that samples the atlas -- so that no draw
can ever read a glyph slot that has been allocated but not yet uploaded.  The
whole texture goes up rather than the changed rectangles: it happens only on
frames where a new character appeared, which after a second of use is almost
never."
  (when (atlas-dirty atlas)
    (metal:with-metal
      (let* ((texture (atlas-texture atlas))
             (width (atlas-width atlas))
             (height (atlas-height atlas))
             (pixels (atlas-pixels atlas)))
        (cffi:with-foreign-object (buffer :uint8 (* width height))
          (dotimes (i (* width height))
            (setf (cffi:mem-aref buffer :uint8 i) (aref pixels i)))
          (metal:with-mtl-region (region 0 0 width height)
            (objc:invoke (metal:texture-handle texture)
                         "replaceRegion:mipmapLevel:withBytes:bytesPerRow:"
                         region 0 buffer width)))))
    (setf (atlas-dirty atlas) nil)
    t))
