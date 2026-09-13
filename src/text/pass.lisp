;;;; src/text/pass.lisp -- cells into pixels.
;;;;
;;;; ONE instanced draw for the whole terminal: a quad per cell background,
;;;; then a quad per glyph, blended straight-alpha over them.  Both sample the
;;;; same atlas -- backgrounds read the solid block reserved at its origin -- so
;;;; there is one pipeline, one texture binding and one draw call.
;;;;
;;;; Metal guarantees primitive order within a draw call for blending purposes,
;;;; which is what lets backgrounds and glyphs share one, rather than needing a
;;;; pass each.

(in-package #:cathode-ray-tube.text)

(defconstant +instance-size+ 48
  "sizeof(CellInstance): float4 colour, then four float2s.  The float4 first, so
the structure's 16-byte alignment is satisfied without padding.")

(defstruct (text-renderer (:constructor %make-text-renderer))
  font
  atlas
  target
  sampler
  (cols 0 :type fixnum)
  (rows 0 :type fixnum)
  ;; The on-screen cell, in device pixels: the font's own cell times SCALE.
  (cell-width 0.0 :type single-float)
  (cell-height 0.0 :type single-float)
  ;; INTEGER magnification, and integer is the whole point.
  ;;
  ;; The sixteen low-resolution faces are bitmap designs rasterised at their
  ;; NATIVE pixel size -- PxPlus_IBM_VGA_8x16 is drawn for an 8x16 cell and
  ;; nothing else.  Asking CoreText for one at 32 pixels gets you a blurry
  ;; interpolation of a bitmap, which is the one thing these fonts must never
  ;; look like.  So they are rasterised small and magnified by a WHOLE number
  ;; with a nearest-neighbour sampler, which keeps every pixel square.
  ;;
  ;; It is also what makes the program legible on a Retina display at all: the
  ;; glyphs are in device pixels, so without magnification an 8x16 cell is 4x8
  ;; POINTS and the terminal comes out half-size.  This is cool-retro-term's
  ;; `scaleTexture', arrived at from the same direction.
  (scale 1 :type (integer 1 16))
  ;; Extra rows between lines, in FONT pixels before magnification.
  ;;
  ;; Every one of the fourteen profiles sets lineSpacing to 0.1, and without it
  ;; the rows touch -- which on the bitmap faces reads as a rendering fault
  ;; rather than as tight leading.  fontmanager.cpp:483:
  ;;
  ;;     lineSpacing = qRound(targetPixelHeight * m_lineSpacing)
  ;;
  ;; qRound, not CL:ROUND: see CRT.UTIL:QROUND.  A 16-pixel cell at 0.1 gives 2
  ;; rows, and a 15-pixel one gives 2 rather than 1, which is the whole
  ;; difference the two rounding rules make here.
  (line-spacing 0 :type fixnum)
  ;; fontWidth: how much wider than its natural advance a cell is.
  ;;
  ;; Upstream does this by rendering the terminal into a NARROWER texture --
  ;; PreprocessedTerminal.qml:101, `totalWidth = floor(width / (screenScaling *
  ;; fontWidth))' -- and then stretching that texture across the full window.
  ;; So the glyphs are stretched with it, and a wider cell with a horizontally
  ;; stretched glyph is the same picture arrived at from the other side.
  ;;
  ;; Two profiles want it: Commodore 64 and Commodore PET, both at 1.25, and
  ;; both of them look wrong without it -- the PET's characters are meant to be
  ;; noticeably wide.  There is no height equivalent; upstream has none either.
  (font-width 1.0d0 :type double-float)
  ;; The substitution chain: the face's own fallback, then the system monospace.
  ;; Held by the renderer rather than by the font because it is loaded at the
  ;; PRIMARY face's pixel size, so it belongs to the pairing rather than to
  ;; either face.
  (fallbacks '() :type list)
  (margin 0.0 :type single-float)
  ;; The instance buffer: an MTLBuffer in SHARED storage, written in place.
  ;; Two instances per cell is the worst case -- a background and a glyph --
  ;; plus one for the cursor.
  ;;
  ;; An MTLBuffer rather than malloc'd memory because -setVertexBuffer: takes
  ;; an MTLBuffer and nothing else; and written in place rather than staged and
  ;; copied because on Apple silicon there is one pool of memory and the copy
  ;; would be from it to itself.
  (instances nil)
  (instance-capacity 0 :type fixnum)
  ;; Rows, reused between frames, so a steady terminal allocates nothing.
  (scratch nil))

(defun cell-advance (font font-width)
  "FONT's horizontal advance for one cell, widened by FONT-WIDTH, in font pixels."
  (* (max 1d0 (font-cell-width font)) font-width))

(defun line-height (font line-spacing)
  "FONT's cell height plus its leading, in font pixels."
  (let ((height (max 1d0 (font-cell-height font))))
    (+ height (util:qround (* height line-spacing)))))

(defun text-renderer-virtual-size (renderer)
  "The terminal's grid in NATIVE font pixels: (values WIDTH HEIGHT).

NOT the magnified size, and the distinction is the whole reason two resolution
spaces exist.  This is what sets the scanline FREQUENCY -- one scanline per
terminal pixel row -- while the drawable's size only feeds the anti-moire ramp.

Passing the magnified size instead makes the two equal, so the oversampling
ratio comes out as exactly the magnification, and smoothstep(2, 4, 2) is zero:
rasterisation silently never engages, on every profile, at every window size.
That is what was happening, and a scanline profile that renders no scanlines
looks like a shader bug rather than an arithmetic one."
  (let ((scale (text-renderer-scale renderer))
        ;; FONT-WIDTH divides back out here and nowhere else.  It is a STRETCH
        ;; applied on the way to the screen, so the terminal pixel grid the
        ;; scanlines count is the un-stretched one -- upstream's `totalWidth' is
        ;; the width BEFORE the division, which is to say before the stretch.
        ;; Leaving it in would make a 1.25 profile's scanlines 25% too sparse.
        (font-width (text-renderer-font-width renderer)))
    (values (/ (* (text-renderer-cols renderer) (text-renderer-cell-width renderer))
               (* scale font-width))
            (/ (* (text-renderer-rows renderer) (text-renderer-cell-height renderer))
               scale))))

(defun text-grid-size (font width height &key (margin 0.0) (scale 1)
                                              (line-spacing 0d0)
                                              (font-width 1.0d0))
  "How many columns and rows of FONT fit in WIDTH by HEIGHT device pixels."
  (let ((cw (* scale (cell-advance font font-width)))
        (ch (* scale (line-height font line-spacing))))
    (values (max 1 (floor (- width (* 2 margin)) cw))
            (max 1 (floor (- height (* 2 margin)) ch)))))

(defun grid-pixel-size (font cols rows &key (margin 0.0) (scale 1)
                                            (line-spacing 0d0)
                                            (font-width 1.0d0))
  "The device pixels COLS by ROWS of FONT need.  The inverse of TEXT-GRID-SIZE.

For sizing a window to a grid rather than fitting a grid to a window, which is
what a terminal defaulting to 80x25 wants."
  (values (+ (* cols scale (cell-advance font font-width)) (* 2 margin))
          (+ (* rows scale (line-height font line-spacing)) (* 2 margin))))

(defun make-text-renderer (&key font width height (margin 0.0) (scale 1)
                                (line-spacing 0d0) (font-width 1.0d0)
                                (fallbacks '()))
  (let* ((atlas (make-atlas))
         (renderer (%make-text-renderer
                    :font font
                    :atlas atlas
                    :margin (float margin 1.0)
                    :scale (max 1 (round scale))
                    :line-spacing (util:qround
                                   (* (max 1d0 (font-cell-height font))
                                      line-spacing))
                    :font-width (float font-width 1d0)
                    :fallbacks fallbacks
                    :cell-width (float (* scale (cell-advance font font-width)) 1.0)
                    :cell-height (float (* scale (line-height font line-spacing))
                                        1.0)
                    ;; Nearest, because the low-resolution faces are the point:
                    ;; their pixels are magnified by a whole number and must stay
                    ;; square.  Linear would turn Commodore PET into a smudge.
                    :sampler (metal:make-sampler :min metal:+filter-nearest+
                                                 :mag metal:+filter-nearest+
                                                 :address metal:+address-clamp-to-edge+
                                                 :label "glyph atlas"))))
    (resize-text-renderer renderer width height)
    renderer))

(defun release-text-renderer (renderer)
  (when (text-renderer-target renderer)
    (metal:release-texture (text-renderer-target renderer))
    (setf (text-renderer-target renderer) nil))
  (when (text-renderer-atlas renderer)
    (release-atlas (text-renderer-atlas renderer))
    (setf (text-renderer-atlas renderer) nil))
  (when (text-renderer-instances renderer)
    (metal:release-buffer (text-renderer-instances renderer))
    (setf (text-renderer-instances renderer) nil
          (text-renderer-instance-capacity renderer) 0))
  ;; The chain is loaded for this renderer and owned by it.  The PRIMARY font is
  ;; not -- the session loaded that and releases it itself.
  (dolist (fallback (text-renderer-fallbacks renderer))
    (release-font fallback))
  (setf (text-renderer-fallbacks renderer) '()))

(defun resize-text-renderer (renderer width height)
  "Fit the grid to WIDTH by HEIGHT device pixels and remake the target."
  (multiple-value-bind (cols rows)
      (text-grid-size (text-renderer-font renderer) width height
                      :margin (text-renderer-margin renderer)
                      :scale (text-renderer-scale renderer)
                      ;; Recomputed from the stored PIXELS rather than the
                      ;; original fraction, so a resize cannot round differently
                      ;; from the construction and shift every glyph by one.
                      :line-spacing (/ (text-renderer-line-spacing renderer)
                                       (max 1d0 (font-cell-height
                                                 (text-renderer-font renderer))))
                      :font-width (text-renderer-font-width renderer))
    (setf (text-renderer-cols renderer) cols
          (text-renderer-rows renderer) rows)
    (when (text-renderer-target renderer)
      (metal:release-texture (text-renderer-target renderer)))
    (setf (text-renderer-target renderer)
          (metal:make-texture :width (max 1 (floor width)) :height (max 1 (floor height))
                              :pixel-format metal:+pixel-format-rgba8unorm+
                              :label "terminal text"))
    ;; Rows of CELLs, reused.  The renderer walks every dirty row every frame,
    ;; and a fresh structure per character would allocate tens of thousands of
    ;; short-lived objects a second.
    (setf (text-renderer-scratch renderer)
          (let ((grid (make-array rows)))
            (dotimes (r rows grid)
              (setf (aref grid r)
                    (let ((row (make-array cols)))
                      (dotimes (c cols row)
                        (setf (aref row c) (vt:make-cell))))))))
    (ensure-instance-capacity renderer (+ 1 (* 2 cols rows)))
    (values cols rows)))

(defun ensure-instance-capacity (renderer count)
  (when (> count (text-renderer-instance-capacity renderer))
    (when (text-renderer-instances renderer)
      (metal:release-buffer (text-renderer-instances renderer)))
    (setf (text-renderer-instances renderer)
          (metal:make-buffer (* count +instance-size+) :label "terminal cells")
          (text-renderer-instance-capacity renderer) count)))

;;; Writing instances ----------------------------------------------------------

(declaim (inline write-instance))
(defun write-instance (buffer index r g b a x y w h u0 v0 u1 v1)
  (let ((base (* index +instance-size+)))
    (macrolet ((f (offset value)
                 `(setf (cffi:mem-ref buffer :float (+ base ,offset))
                        (float ,value 1.0))))
      (f 0 r) (f 4 g) (f 8 b) (f 12 a)
      (f 16 x) (f 20 y)
      (f 24 w) (f 28 h)
      (f 32 u0) (f 36 v0)
      (f 40 u1) (f 44 v1))))

(defun cell-colors (cell default-fg default-bg &optional selected)
  "(values FG-R FG-G FG-B BG-R BG-G BG-B) in 0..1, reverse video applied.

SELECTED inverts as well, and inverting twice cancels -- so selecting
already-reversed text shows it the right way round, which is what makes a
selection readable over a highlighted region."
  (multiple-value-bind (fr fg fb) (vt:resolve-color (vt:cell-fg cell) :default default-fg)
    (multiple-value-bind (br bg bb) (vt:resolve-color (vt:cell-bg cell) :default default-bg)
      (if (alexandria:xor (vt:attr-set-p (vt:cell-attrs cell) vt:+attr-reverse+)
                          selected)
          (values (/ br 255.0) (/ bg 255.0) (/ bb 255.0)
                  (/ fr 255.0) (/ fg 255.0) (/ fb 255.0))
          (values (/ fr 255.0) (/ fg 255.0) (/ fb 255.0)
                  (/ br 255.0) (/ bg 255.0) (/ bb 255.0))))))

(defun rule (buffer n solid r g b x y w h)
  "A filled rectangle, sampling the atlas's solid block.

Underlines, strikethroughs and the cursor are all this.  Reusing the solid slot
rather than a second untextured pipeline is what keeps the whole text pass to
one pipeline, one texture binding and one draw."
  (write-instance buffer n r g b 1.0 x y w h
                  (glyph-u0 solid) (glyph-v0 solid)
                  (glyph-u1 solid) (glyph-v1 solid))
  (1+ n))

(defun build-instances (renderer snapshot &key (default-fg '(229 229 229))
                                               (default-bg '(0 0 0))
                                               (blink-on t) (cursor-on t)
                                               selected-p)
  "Fill the instance buffer from SNAPSHOT.  Returns the instance count.

Backgrounds first, then glyphs: within one draw call Metal respects primitive
order for blending, so the glyphs composite over the backgrounds without a
second pass."
  (let* ((buffer (metal:buffer-contents (text-renderer-instances renderer)))
         (atlas (text-renderer-atlas renderer))
         (font (text-renderer-font renderer))
         (solid (atlas-solid atlas))
         (cw (text-renderer-cell-width renderer))
         (ch (text-renderer-cell-height renderer))
         (scale (float (text-renderer-scale renderer) 1.0))
         (margin (text-renderer-margin renderer))
         ;; CW and CH are already scaled; the GLYPH metrics are not -- they come
         ;; from CoreText in the font's own pixels -- so every one of them is
         ;; multiplied here.  Missing one puts the glyphs in the right cells at
         ;; the wrong size, which looks like a font problem.
         (ascent (* scale (float (font-ascent font) 1.0)))
         ;; The horizontal scale a GLYPH is drawn at.  Upstream stretches the
         ;; whole terminal texture in x by fontWidth, so the glyphs stretch with
         ;; the cells rather than sitting narrow inside wide ones.
         (xscale (* scale (float (text-renderer-font-width renderer) 1.0)))
         (rows (min (text-renderer-rows renderer) (term:snapshot-rows snapshot)))
         (cols (min (text-renderer-cols renderer) (term:snapshot-cols snapshot)))
         (cells (term:snapshot-cells snapshot))
         (n 0))
    ;; Backgrounds.
    (dotimes (row rows)
      (let ((line (aref cells row)))
        (dotimes (col cols)
          (let ((cell (aref line col)))
            (when (plusp (vt:cell-width cell))
              (multiple-value-bind (fr fg fb br bg bb)
                  (cell-colors cell default-fg default-bg
                               (and selected-p (funcall selected-p row col)))
                (declare (ignore fr fg fb))
                ;; Alpha 1 inside the content rectangle.  The static pass uses
                ;; the blurred alpha as its bloom mask, so a background that
                ;; wrote 0 would silently remove the cell from the bloom.
                (write-instance buffer n br bg bb 1.0
                                (+ margin (* col cw)) (+ margin (* row ch))
                                (* cw (max 1 (vt:cell-width cell))) ch
                                (glyph-u0 solid) (glyph-v0 solid)
                                (glyph-u1 solid) (glyph-v1 solid))
                (incf n)))))))
    ;; Glyphs.
    (dotimes (row rows)
      (let ((line (aref cells row)))
        (dotimes (col cols)
          (let ((cell (aref line col)))
            ;; Width 0 is the SECOND half of a double-width glyph: the first
            ;; half already drew it, and drawing here would paint over its right
            ;; side.
            (when (and (plusp (vt:cell-width cell))
                       (not (vt:cell-blank-p cell))
                       (not (vt:attr-set-p (vt:cell-attrs cell) vt:+attr-conceal+))
                       ;; Blink hides the INK, never the background: a blinking
                       ;; cell that dropped its background would flash a hole in
                       ;; a coloured region rather than blinking its text.
                       (or blink-on
                           (not (vt:attr-set-p (vt:cell-attrs cell)
                                               vt:+attr-blink+))))
              (let* ((attrs (vt:cell-attrs cell))
                     (glyph (atlas-glyph atlas font (vt:cell-char cell)
                                         :bold (vt:attr-set-p attrs vt:+attr-bold+)
                                         :italic (vt:attr-set-p attrs
                                                                vt:+attr-italic+)
                                         :fallbacks (text-renderer-fallbacks
                                                     renderer))))
                (when (plusp (glyph-width glyph))
                  (multiple-value-bind (fr fg fb)
                      (cell-colors cell default-fg default-bg
                                   (and selected-p (funcall selected-p row col)))
                    (write-instance
                     buffer n fr fg fb 1.0
                     (+ margin (* col cw) (* xscale (glyph-bearing-x glyph)))
                     (+ margin (* row ch) ascent
                        (- (* scale (glyph-bearing-y glyph))))
                     (* xscale (glyph-width glyph)) (* scale (glyph-height glyph))
                     (glyph-u0 glyph) (glyph-v0 glyph)
                     (glyph-u1 glyph) (glyph-v1 glyph))
                    (incf n)))))))))
    ;; Underlines and strikethroughs, as rules over the cells that asked for
    ;; them.  A separate pass over the grid rather than interleaved with the
    ;; glyphs, so that a rule is drawn OVER its glyph rather than under it --
    ;; which is what a strikethrough means.
    (dotimes (row rows)
      (let ((line (aref cells row)))
        (dotimes (col cols)
          (let* ((cell (aref line col))
                 (attrs (vt:cell-attrs cell)))
            (when (and (plusp (vt:cell-width cell))
                       (or (vt:attr-set-p attrs vt:+attr-underline+)
                           (vt:attr-set-p attrs vt:+attr-strike+))
                       (or blink-on (not (vt:attr-set-p attrs vt:+attr-blink+))))
              (multiple-value-bind (fr fg fb)
                  (cell-colors cell default-fg default-bg
                               (and selected-p (funcall selected-p row col)))
                (let* ((x (+ margin (* col cw)))
                       (w (* cw (max 1 (vt:cell-width cell))))
                       (top (+ margin (* row ch)))
                       ;; One device pixel at 1x, two at 2x: a rule that stays
                       ;; one pixel under magnification disappears next to
                       ;; glyphs whose strokes grew with it.
                       (thickness (max 1.0 scale)))
                  (when (vt:attr-set-p attrs vt:+attr-strike+)
                    (setf n (rule buffer n solid fr fg fb
                                  x (+ top (* 0.55 ascent)) w thickness)))
                  (when (vt:attr-set-p attrs vt:+attr-underline+)
                    (let ((y (+ top ascent thickness)))
                      (ecase (vt:cell-underline-style attrs)
                        ((0 1) (setf n (rule buffer n solid fr fg fb x y w thickness)))
                        (2 ;; Double: two rules with a gap of one thickness.
                           (setf n (rule buffer n solid fr fg fb x y w thickness))
                           (setf n (rule buffer n solid fr fg fb x
                                         (+ y (* 2 thickness)) w thickness)))
                        (3 ;; Curly, as four dashes alternating by one thickness.
                           ;; Not a sine -- at one or two pixels of amplitude a
                           ;; sine and a square wave are the same picture, and
                           ;; this is four instances instead of a shader.
                           (let ((dash (/ w 4.0)))
                             (dotimes (i 4)
                               (setf n (rule buffer n solid fr fg fb
                                             (+ x (* i dash))
                                             (+ y (if (evenp i) 0.0 thickness))
                                             dash thickness)))))))))))))))
    ;; The cursor, as a block over the cell it is on.
    (when (and (term:snapshot-cursor-visible snapshot)
               cursor-on
               (< (term:snapshot-cursor-row snapshot) rows)
               (< (term:snapshot-cursor-col snapshot) cols))
      (write-instance buffer n 0.75 0.75 0.75 0.65
                      (+ margin (* (term:snapshot-cursor-col snapshot) cw))
                      (+ margin (* (term:snapshot-cursor-row snapshot) ch))
                      cw ch
                      (glyph-u0 solid) (glyph-v0 solid)
                      (glyph-u1 solid) (glyph-v1 solid))
      (incf n))
    n))

;;; The pass -------------------------------------------------------------------

(defun render-text (renderer snapshot &key (buffer nil) (default-fg '(229 229 229))
                                           (default-bg '(0 0 0)) (blink-on t)
                                           (cursor-on t) selected-p)
  "Draw SNAPSHOT into the renderer's target.  Returns the target.

SELECTED-P, when given, is called with a row and a column and says whether that
cell is selected.  A predicate rather than a range, so the renderer needs to
know nothing about how a selection is shaped."
  (let* ((target (text-renderer-target renderer))
         (count (build-instances renderer snapshot
                                 :default-fg default-fg :default-bg default-bg
                                 :blink-on blink-on :cursor-on cursor-on
                                 :selected-p selected-p)))
    ;; BEFORE the pass, always: a glyph seen for the first time this frame was
    ;; allocated a slot while instances were being built, and a draw that
    ;; sampled it before the upload would read whatever was in the atlas before.
    (atlas-flush (text-renderer-atlas renderer))
    (metal:with-metal
      (let ((pipeline (metal:pipeline :vertex "text_vertex" :fragment "text_fragment"
                                      :pixel-format metal:+pixel-format-rgba8unorm+
                                      :blending t :label "text")))
        (cffi:with-foreign-object (uniforms :float 2)
          (setf (cffi:mem-aref uniforms :float 0) (float (metal:texture-width target) 1.0)
                (cffi:mem-aref uniforms :float 1) (float (metal:texture-height target) 1.0))
          ;; Cleared to zero alpha, and WITHOUT the profile background -- the
          ;; dynamic pass applies that.  See the note in crt.metal.
          (metal:with-render-pass (encoder target
                                   :buffer (or buffer (metal:command-buffer))
                                   :clear '(0d0 0d0 0d0 0d0)
                                   :label "text")
            (when (plusp count)
              (metal:use-pipeline encoder pipeline)
              (metal:bind-vertex-buffer encoder (text-renderer-instances renderer) 0)
              (metal:bind-vertex-bytes encoder uniforms 8 1)
              (metal:bind-fragment-texture encoder (atlas-texture
                                                    (text-renderer-atlas renderer))
                                           0)
              (metal:bind-fragment-sampler encoder (text-renderer-sampler renderer) 0)
              (metal:draw-instances encoder count))))))
    target))
