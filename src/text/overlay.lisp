;;;; src/text/overlay.lisp -- the terminal-size overlay.
;;;;
;;;; SizeOverlay.qml: a rounded black box at half opacity, centred, showing
;;;; COLUMNSxROWS, which appears when the grid changes and fades out a second
;;;; later.  It is a sibling of the shader chain rather than part of it -- `z: 3'
;;;; over everything -- so it is NOT curved, tinted, blurred or scanlined.  A
;;;; size readout you cannot read during the resize it is reporting would be a
;;;; strange thing to build.
;;;;
;;;; That places it after the dynamic pass, drawn straight onto the drawable,
;;;; which is why the dynamic pass has to be told to leave the PRESENT to
;;;; someone else when there is an overlay to come.
;;;;
;;;; The one deviation is the corner radius: upstream rounds by 5 pixels and
;;;; this is square.  Rounding wants either a fragment shader of its own or a
;;;; nine-slice in the atlas, and on a box this size it is not visible from
;;;; across the desk.

(in-package #:cathode-ray-tube.text)

(defconstant +overlay-hold+ 1.0d0
  "Seconds the overlay stays at full strength.  SizeOverlay.qml's Timer.")

(defconstant +overlay-fade+ 0.2d0
  "Seconds it takes to fade out.  SizeOverlay.qml's NumberAnimation.")

(defconstant +overlay-opacity+ 0.5d0
  "How dark the box gets.  SizeOverlay.qml again.")

(defstruct (overlay (:constructor %make-overlay))
  "The transient COLUMNSxROWS readout.

Holds no font and no atlas of its own: RENDER-OVERLAY borrows the text
renderer's, so the readout is in the terminal's own face and shares its glyph
cache.  Upstream uses the UI font, and a Commodore PET reporting its size in
Helvetica would be the stranger of the two choices."
  (instances nil)
  (capacity 0 :type fixnum)
  (text "" :type string)
  (cols 0 :type fixnum)
  (rows 0 :type fixnum)
  ;; INTERNAL-REAL-TIME, not the effect clock: the effect clock is frame-skipped
  ;; to about 20Hz and stops advancing when the effects are off entirely, and a
  ;; readout that outlived its welcome by three seconds on the Boring profile
  ;; would be a puzzling thing to debug.
  (shown-at 0 :type integer))

(defun make-overlay () (%make-overlay))

(defun release-overlay (overlay)
  (when (overlay-instances overlay)
    (metal:release-buffer (overlay-instances overlay))
    (setf (overlay-instances overlay) nil (overlay-capacity overlay) 0)))

(defun overlay-note-size (overlay cols rows)
  "Record a new grid size, and start the timer if it actually changed.

Only on a CHANGE: a drag that moves the window edge by three pixels without
crossing a cell boundary must not restart the timer, or the overlay never fades
while the mouse is down."
  (unless (and (= cols (overlay-cols overlay)) (= rows (overlay-rows overlay)))
    (setf (overlay-cols overlay) cols
          (overlay-rows overlay) rows
          (overlay-text overlay) (format nil "~Dx~D" cols rows)
          (overlay-shown-at overlay) (get-internal-real-time)))
  overlay)

(defun overlay-alpha (overlay)
  "How opaque the box is now, 0 when it is done."
  (if (zerop (overlay-shown-at overlay))
      0d0
      (let ((age (/ (- (get-internal-real-time) (overlay-shown-at overlay))
                    (float internal-time-units-per-second 1d0))))
        (cond ((< age +overlay-hold+) 1d0)
              ((< age (+ +overlay-hold+ +overlay-fade+))
               (- 1d0 (/ (- age +overlay-hold+) +overlay-fade+)))
              (t 0d0)))))

(defun overlay-visible-p (overlay)
  (plusp (overlay-alpha overlay)))

(defun ensure-overlay-capacity (overlay count)
  (when (> count (overlay-capacity overlay))
    (when (overlay-instances overlay)
      (metal:release-buffer (overlay-instances overlay)))
    (setf (overlay-instances overlay)
          (metal:make-buffer (* count +instance-size+) :label "size overlay")
          (overlay-capacity overlay) count)))

(defun build-overlay-instances (overlay renderer width height alpha)
  "Lay the box and its text out centred in WIDTH by HEIGHT.  Returns the count."
  (let* ((font (text-renderer-font renderer))
         (atlas (text-renderer-atlas renderer))
         (solid (atlas-solid atlas))
         (text (overlay-text overlay))
         (scale (float (text-renderer-scale renderer) 1.0))
         (advance (float (* scale (max 1d0 (font-cell-width font))) 1.0))
         (ascent (* scale (float (font-ascent font) 1.0)))
         (line (float (* scale (max 1d0 (font-cell-height font))) 1.0))
         ;; `width: textSize.width * 2' -- the box is twice the text, both ways.
         (box-w (* 2.0 advance (length text)))
         (box-h (* 2.0 line))
         (box-x (- (* 0.5 width) (* 0.5 box-w)))
         (box-y (- (* 0.5 height) (* 0.5 box-h)))
         (text-x (- (* 0.5 width) (* 0.5 advance (length text))))
         (text-y (- (* 0.5 height) (* 0.5 line))))
    (ensure-overlay-capacity overlay (1+ (length text)))
    (let ((buffer (metal:buffer-contents (overlay-instances overlay)))
          (n 0))
      ;; The box.  Premultiplied, because the drawable is composited that way
      ;; and the pipeline blends ONE, ONE-MINUS-SOURCE-ALPHA -- so a black box at
      ;; half alpha is (0 0 0 0.5) and its colour channels are already zero.
      (write-instance buffer n 0.0 0.0 0.0 (* alpha (float +overlay-opacity+ 1.0))
                      box-x box-y box-w box-h
                      (glyph-u0 solid) (glyph-v0 solid)
                      (glyph-u1 solid) (glyph-v1 solid))
      (incf n)
      (loop for i below (length text)
            for character = (char text i)
            for glyph = (atlas-glyph atlas font character)
            do (when (plusp (glyph-width glyph))
                 (write-instance
                  buffer n alpha alpha alpha alpha
                  (+ text-x (* i advance) (* scale (glyph-bearing-x glyph)))
                  (+ text-y ascent (- (* scale (glyph-bearing-y glyph))))
                  (* scale (glyph-width glyph)) (* scale (glyph-height glyph))
                  (glyph-u0 glyph) (glyph-v0 glyph)
                  (glyph-u1 glyph) (glyph-v1 glyph))
                 (incf n)))
      n)))

(defun render-overlay (overlay renderer target width height
                       &key drawable (pixel-format metal:+pixel-format-bgra8unorm+))
  "Draw OVERLAY onto TARGET, which already holds the finished frame.

LOAD, never CLEAR: the frame is already there and this adds to it.  Presents
DRAWABLE, because it is the last pass to touch it."
  (let ((alpha (overlay-alpha overlay)))
    (when (plusp alpha)
      (let ((count (build-overlay-instances overlay renderer width height
                                            (float alpha 1.0))))
        (atlas-flush (text-renderer-atlas renderer))
        (metal:with-metal
          (let ((pipeline (metal:pipeline :vertex "text_vertex"
                                          :fragment "text_fragment"
                                          :pixel-format pixel-format
                                          :blending t :label "overlay")))
            (cffi:with-foreign-object (uniforms :float 2)
              (setf (cffi:mem-aref uniforms :float 0) (float width 1.0)
                    (cffi:mem-aref uniforms :float 1) (float height 1.0))
              (metal:with-render-pass (encoder target
                                       :load metal:+load-action-load+
                                       :present drawable
                                       :viewport (list 0 0 width height)
                                       :label "size overlay")
                (when (plusp count)
                  (metal:use-pipeline encoder pipeline)
                  (metal:bind-vertex-buffer encoder (overlay-instances overlay) 0)
                  (metal:bind-vertex-bytes encoder uniforms 8 1)
                  (metal:bind-fragment-texture
                   encoder (atlas-texture (text-renderer-atlas renderer)) 0)
                  (metal:bind-fragment-sampler
                   encoder (text-renderer-sampler renderer) 0)
                  (metal:draw-instances encoder count))))))))
    overlay))
