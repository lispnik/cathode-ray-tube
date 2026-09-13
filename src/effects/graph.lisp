;;;; src/effects/graph.lisp -- the render graph.
;;;;
;;;;   T  the terminal text, from CRT.TEXT
;;;;   |
;;;;   +--> burn-in (ping-pong B0/B1) ------+
;;;;   |                                    |
;;;;   +--> bloom (downsample, H, V) -> L --+
;;;;   |                                    |
;;;;   +--> static ------------------> S ---+--> dynamic --> the drawable
;;;;                                        |
;;;;        frame (cached) ----------> F ---+
;;;;
;;;; The chain is T -> static -> S -> dynamic; burn-in and bloom branch off T
;;;; DIRECTLY, not off the static output.  (ShaderTerminal.qml:
;;;; staticShader.source is the terminal source, frameBuffer wraps staticShader,
;;;; dynamicShader.screenBuffer is frameBuffer.)
;;;;
;;;; Two passes do not run every frame, and that is not an optimisation:
;;;;
;;;;   BURN-IN advances only when the terminal actually repainted, which is
;;;;   cool-retro-term's `imagePainted' signal.  Running it every frame would
;;;;   decay the phosphor at the DISPLAY's refresh rate rather than the
;;;;   terminal's, so the same session would fade differently on a 60Hz and a
;;;;   120Hz screen.
;;;;
;;;;   THE BEZEL has no time uniform at all.  It is redrawn only when the
;;;;   profile or the size changes.

(in-package #:cathode-ray-tube.effects)

(defstruct (graph (:constructor %make-graph))
  profile
  ;; Targets.
  (burn-a nil) (burn-b nil)
  (blur-h nil) (bloom nil)
  (frame nil) (static nil)
  (noise nil)
  ;; Samplers: clamped for everything that must not wrap, repeating for the two
  ;; that must.
  (clamp-sampler nil) (repeat-sampler nil)
  ;; Sizes, so a resize can tell whether anything actually changed.
  (width 0 :type fixnum) (height 0 :type fixnum)
  (virtual-width 1.0d0) (virtual-height 1.0d0)
  ;; Quality knobs, upstream's names and defaults.
  (window-scaling 1.0d0)
  (bloom-quality 0.5d0)
  (burn-in-quality 0.5d0)
  ;; Burn-in bookkeeping.
  (burn-in-last-update 0.0d0)
  (burn-in-previous-update 0.0d0)
  (frame-valid nil))

;;; The noise texture ---------------------------------------------------------------

(defun load-noise-texture ()
  "allNoise512.png, the ONLY texture in the whole effect chain.

Four uncorrelated channels doing four jobs: .r is the horizontal-sync threshold,
.g is both the flicker brightness and the tear frequency, .b and .a are the
jitter displacement, and .a is reused for the snow.  Copied byte-identical from
cool-retro-term, because the exact values are what the look is tuned against."
  (metal:with-metal
    (let* ((path (util:resource "images/allNoise512.png"))
           (image (objc:invoke (objc:invoke "NSImage" "alloc")
                               "initWithContentsOfFile:"
                               (uiop:native-namestring path))))
      (when (metal:null-object-p image)
        (error "Could not read the noise texture at ~A." path))
      (let* ((rep (objc:invoke image "TIFFRepresentation"))
             (bitmap (objc:invoke "NSBitmapImageRep" "imageRepWithData:" rep))
             (width (objc:invoke-into 'integer bitmap "pixelsWide"))
             (height (objc:invoke-into 'integer bitmap "pixelsHigh"))
             (stride (objc:invoke-into 'integer bitmap "bytesPerRow"))
             ;; -bitmapData returns `unsigned char *', which ENCODES exactly as
             ;; a C string does -- plain INVOKE hands back a Lisp string, and
             ;; since the buffer usually starts with a zero byte, an empty one.
             ;; INVOKE-INTO with :POINTER is what that disposition is for.
             (bytes (objc:invoke-into :pointer bitmap "bitmapData"))
             (texture (metal:make-texture :width width :height height
                                          :pixel-format metal:+pixel-format-rgba8unorm+
                                          :usage metal:+usage-shader-read+
                                          :label "allNoise512")))
        (metal:with-mtl-region (region 0 0 width height)
          (objc:invoke (metal:texture-handle texture)
                       "replaceRegion:mipmapLevel:withBytes:bytesPerRow:"
                       region 0 bytes stride))
        texture))))

;;; Construction -------------------------------------------------------------------

(defun make-graph (&key profile width height)
  (let ((graph (%make-graph :profile profile)))
    (setf (graph-noise graph) (load-noise-texture)
          ;; REPEAT for the noise: the dynamic pass scrolls its coordinates with
          ;; time without bounding them, so clamping would freeze the grain into
          ;; a smear at the edges.
          (graph-repeat-sampler graph)
          (metal:make-sampler :address metal:+address-repeat+ :label "noise")
          (graph-clamp-sampler graph)
          (metal:make-sampler :address metal:+address-clamp-to-edge+ :label "effects"))
    (resize-graph graph width height)
    graph))

(defun release-graph (graph)
  (dolist (accessor '(graph-burn-a graph-burn-b graph-blur-h graph-bloom
                      graph-frame graph-static graph-noise))
    (let ((texture (funcall accessor graph)))
      (when texture
        (metal:release-texture texture)
        (funcall (fdefinition (list 'setf accessor)) nil graph)))))

(defun resize-graph (graph width height)
  "Reallocate every target for a WIDTH by HEIGHT drawable.

The sizes are not uniform and the differences are load-bearing: the burn-in
accumulator and the bloom run at a fraction of the screen, which is what makes
them affordable and also what gives them their softness."
  (let ((width (max 1 (floor width)))
        (height (max 1 (floor height))))
    (unless (and (= width (graph-width graph)) (= height (graph-height graph)))
      (setf (graph-width graph) width (graph-height graph) height)
      ;; ACCESSOR is a SYMBOL, not #'accessor: (fdefinition (list 'setf ...))
      ;; needs a function NAME, and handing it a function object fails with
      ;; "Invalid function name: (SETF #<FUNCTION ...>)", which reads as though
      ;; the setf function were missing rather than the argument wrong.
      (flet ((replace-texture (accessor w h &key label
                                                 (format metal:+pixel-format-rgba8unorm+))
               (let ((old (funcall accessor graph)))
                 (when old (metal:release-texture old)))
               (funcall (fdefinition (list 'setf accessor))
                        (metal:make-texture :width (max 1 (floor w))
                                            :height (max 1 (floor h))
                                            :pixel-format format :label label)
                        graph)))
        (let ((scaled-w (* width (graph-window-scaling graph)))
              (scaled-h (* height (graph-window-scaling graph)))
              (bloom-w (* width (graph-bloom-quality graph)))
              (bloom-h (* height (graph-bloom-quality graph)))
              (burn-w (* width (graph-burn-in-quality graph)))
              (burn-h (* height (graph-burn-in-quality graph))))
          (replace-texture 'graph-static scaled-w scaled-h :label "static")
          (replace-texture 'graph-frame scaled-w scaled-h :label "frame")
          (replace-texture 'graph-blur-h bloom-w bloom-h :label "bloom h")
          (replace-texture 'graph-bloom bloom-w bloom-h :label "bloom")
          (replace-texture 'graph-burn-a burn-w burn-h :label "burn-in a")
          (replace-texture 'graph-burn-b burn-w burn-h :label "burn-in b")))
      ;; Every target has just been created with undefined contents, and the
      ;; bezel is only drawn when it is invalid -- so say so.
      (setf (graph-frame-valid graph) nil)
      ;; The accumulator starts black rather than at whatever the allocator
      ;; handed us, or the first frames show a ghost of somebody else's memory.
      (clear-texture (graph-burn-a graph))
      (clear-texture (graph-burn-b graph)))
    graph))

(defun clear-texture (texture)
  (metal:with-render-pass (encoder texture :clear '(0d0 0d0 0d0 0d0))))

(defun invalidate-frame (graph)
  (setf (graph-frame-valid graph) nil))

(defun set-graph-profile (graph profile)
  (setf (graph-profile graph) profile)
  (invalidate-frame graph)
  graph)

;;; The passes -----------------------------------------------------------------------

(defun render-frame-pass (graph)
  "The bezel, into F.  Only when invalid: it has no time uniform."
  (let* ((profile (graph-profile graph))
         (target (graph-frame graph))
         (scale (settings:normalized-window-scale (graph-width graph)
                                                  (graph-height graph)))
         (pipeline (metal:pipeline :fragment "frame_fragment" :label "frame")))
    (metal:with-uniforms (u frame-uniforms)
      (let ((colour (settings:derived-frame-color profile)))
        (setf (u :frame-color) (list (util:rgba-r colour) (util:rgba-g colour)
                                     (util:rgba-b colour) 1.0)
              (u :viewport-size) (list (metal:texture-width target)
                                       (metal:texture-height target))
              (u :screen-curvature) (* (settings:profile-screen-curvature profile)
                                       settings:+screen-curvature-size+ scale)
              (u :frame-size) (* (settings:frame-size profile) scale)
              (u :screen-radius) (settings:screen-radius profile)
              ;; RAW, not scaled by 0.2 -- the frame pass is the one place
              ;; ambientLight is used unmodified.
              (u :ambient-light) (settings:profile-ambient-light profile)
              (u :frame-shininess) (settings:frame-shininess profile)
              (u :opacity) 1.0))
      (metal:with-render-pass (encoder target :clear '(0d0 0d0 0d0 0d0) :label "frame")
        (metal:use-pipeline encoder pipeline)
        (metal:bind-uniforms encoder (u) 'frame-uniforms 0)
        (metal:draw-quad encoder)))
    (setf (graph-frame-valid graph) t)
    target))

(defun render-burn-in-pass (graph source time)
  "Advance the phosphor accumulator.  Returns the texture now holding it.

Ping-pong: Qt reads its own target recursively, which Metal forbids.  This is
numerically identical rather than an approximation, because the shader only ever
reads the texel it writes."
  (let* ((profile (graph-profile graph))
         (destination (graph-burn-b graph))
         (previous (graph-burn-a graph))
         (pipeline (metal:pipeline :fragment "burn_in_fragment" :label "burn-in")))
    (setf (graph-burn-in-previous-update graph) (graph-burn-in-last-update graph)
          (graph-burn-in-last-update graph) time)
    (metal:with-uniforms (u burn-in-uniforms)
      (setf (u :last-update) (graph-burn-in-last-update graph)
            (u :previous-update) (graph-burn-in-previous-update graph)
            (u :burn-in-time) (settings:burn-in-fade-time profile)
            (u :opacity) 1.0)
      (metal:with-render-pass (encoder destination :clear '(0d0 0d0 0d0 0d0)
                                                   :label "burn-in")
        (metal:use-pipeline encoder pipeline)
        (metal:bind-uniforms encoder (u) 'burn-in-uniforms 0)
        (metal:bind-fragment-texture encoder source 0)
        (metal:bind-fragment-texture encoder previous 1)
        (metal:bind-fragment-sampler encoder (graph-clamp-sampler graph) 0)
        (metal:draw-quad encoder)))
    ;; Swap, so the texture just written becomes next frame's `previous'.
    (rotatef (graph-burn-a graph) (graph-burn-b graph))
    (graph-burn-a graph)))

(defun render-bloom-pass (graph source)
  "Downsample and blur, horizontally then vertically, into L."
  (let* ((profile (graph-profile graph))
         (horizontal (graph-blur-h graph))
         (target (graph-bloom graph))
         (pipeline (metal:pipeline :fragment "blur_fragment" :label "blur"))
         ;; lint(16, 64, bloomQuality), as upstream sets FastBlur's radius.  It
         ;; is in source pixels, and the blur runs at bloomQuality scale, so the
         ;; effective spread in screen terms is the same at any quality.
         (radius (/ (util:lint 16.0d0 64.0d0 (graph-bloom-quality graph)) 8.0d0)))
    (declare (ignorable profile))
    (flet ((blur (from to dx dy)
             (metal:with-uniforms (u blur-uniforms)
               (setf (u :texel) (list (/ dx (max 1 (metal:texture-width from)))
                                      (/ dy (max 1 (metal:texture-height from))))
                     (u :radius) radius)
               (metal:with-render-pass (encoder to :clear '(0d0 0d0 0d0 0d0)
                                                   :label "blur")
                 (metal:use-pipeline encoder pipeline)
                 (metal:bind-uniforms encoder (u) 'blur-uniforms 0)
                 (metal:bind-fragment-texture encoder from 0)
                 (metal:bind-fragment-sampler encoder (graph-clamp-sampler graph) 0)
                 (metal:draw-quad encoder)))))
      (blur source horizontal 1.0 0.0)
      (blur horizontal target 0.0 1.0))
    target))

(defun render-static-pass (graph source bloom)
  (let* ((profile (graph-profile graph))
         (target (graph-static graph))
         (scale (settings:normalized-window-scale (graph-width graph)
                                                  (graph-height graph)))
         (pipeline (metal:pipeline :fragment "static_fragment"
                                   :constants (static-pipeline-constants profile)
                                   :label "static")))
    (metal:with-uniforms (u static-uniforms)
      (setf (u :screen-curvature) (* (settings:profile-screen-curvature profile)
                                     settings:+screen-curvature-size+ scale)
            ;; In UV, scaled by the width, so the fringe is a constant number of
            ;; pixels rather than a constant fraction of the window.
            (u :rgb-shift) (* (settings:profile-rgb-shift profile)
                              (/ 4.0d0 (max 1 (metal:texture-width target))))
            (u :frame-shininess) (settings:frame-shininess profile)
            (u :frame-size) (* (settings:frame-size profile) scale)
            (u :screen-brightness) (util:lint 0.5d0 1.5d0
                                              (settings:profile-brightness profile))
            (u :bloom) (* (settings:profile-bloom profile) 2.5d0)
            (u :opacity) 1.0)
      (metal:with-render-pass (encoder target :clear '(0d0 0d0 0d0 1d0) :label "static")
        (metal:use-pipeline encoder pipeline)
        (metal:bind-uniforms encoder (u) 'static-uniforms 0)
        (metal:bind-fragment-texture encoder source 0)
        (metal:bind-fragment-texture encoder bloom 1)
        (metal:bind-fragment-sampler encoder (graph-clamp-sampler graph) 0)
        (metal:draw-quad encoder)))
    target))

(defun render-dynamic-pass (graph target static burn-in time &key drawable
                                                                  (opacity 1.0d0))
  (let* ((profile (graph-profile graph))
         (scale (settings:normalized-window-scale (graph-width graph)
                                                  (graph-height graph)))
         (pipeline (metal:pipeline :vertex "dynamic_vertex"
                                   :fragment "dynamic_fragment"
                                   :constants (dynamic-pipeline-constants profile)
                                   :pixel-format (if drawable
                                                     metal:+pixel-format-bgra8unorm+
                                                     metal:+pixel-format-rgba8unorm+)
                                   :label "dynamic"))
         (font (settings:derived-font-color profile))
         (background (settings:derived-background-color profile))
         (jitter (settings:profile-jitter profile)))
    (metal:with-uniforms (u dynamic-uniforms)
      (setf (u :font-color) (list (util:rgba-r font) (util:rgba-g font)
                                  (util:rgba-b font) 1.0)
            (u :background-color) (list (util:rgba-r background)
                                        (util:rgba-g background)
                                        (util:rgba-b background) 1.0)
            (u :virtual-resolution) (list (graph-virtual-width graph)
                                          (graph-virtual-height graph))
            ;; Anisotropic: three and a half times more horizontal than
            ;; vertical, because a real tube's beam wanders along a scanline far
            ;; more easily than across them.
            (u :jitter-displacement) (list (* 0.007d0 jitter) (* 0.002d0 jitter))
            ;; Keeps the grain a constant size on screen however the window is
            ;; scaled, rather than stretching with it.
            (u :scale-noise-size) (list (/ (* (graph-width graph) 0.75d0) 512.0d0)
                                        (/ (* (graph-height graph) 0.75d0) 512.0d0))
            (u :time) time
            (u :opacity) opacity
            (u :rasterization-intensity)
            (settings:rasterization-intensity (graph-virtual-width graph)
                                              (graph-virtual-height graph)
                                              (graph-width graph)
                                              (graph-height graph))
            (u :burn-in-last-update) (graph-burn-in-last-update graph)
            (u :burn-in-time) (settings:burn-in-fade-time profile)
            (u :static-noise) (settings:profile-static-noise profile)
            (u :screen-curvature) (* (settings:profile-screen-curvature profile)
                                     settings:+screen-curvature-size+ scale)
            (u :glowing-line) (* (settings:profile-glowing-line profile) 0.2d0)
            (u :chroma-color) (settings:profile-chroma-color profile)
            (u :jitter) jitter
            (u :horizontal-sync) (settings:profile-horizontal-sync profile)
            (u :horizontal-sync-strength) (settings:horizontal-sync-strength profile)
            (u :flickering) (settings:profile-flickering profile)
            (u :frame-size) (* (settings:frame-size profile) scale)
            (u :bloom) (* (settings:profile-bloom profile) 2.5d0))
      (metal:with-render-pass (encoder target :clear '(0d0 0d0 0d0 1d0)
                                              :present drawable :label "dynamic")
        (metal:use-pipeline encoder pipeline)
        ;; The vertex stage samples the noise texture -- the per-frame RNG runs
        ;; there, four invocations instead of one per pixel.
        (metal:bind-vertex-bytes encoder (u)
                                 (metal:uniform-block-size 'dynamic-uniforms) 0)
        (objc:invoke encoder "setVertexTexture:atIndex:"
                     (metal:texture-handle (graph-noise graph)) 0)
        (objc:invoke encoder "setVertexSamplerState:atIndex:"
                     (graph-repeat-sampler graph) 0)
        (metal:bind-uniforms encoder (u) 'dynamic-uniforms 0)
        (metal:bind-fragment-texture encoder (graph-noise graph) 0)
        (metal:bind-fragment-texture encoder static 1)
        (metal:bind-fragment-texture encoder burn-in 2)
        (metal:bind-fragment-texture encoder (graph-frame graph) 3)
        (metal:bind-fragment-sampler encoder (graph-repeat-sampler graph) 0)
        (metal:bind-fragment-sampler encoder (graph-clamp-sampler graph) 1)
        (metal:draw-quad encoder)))
    target))

;;; The whole thing --------------------------------------------------------------------

(defun render-effects (graph source target &key drawable (time 0.0d0) painted
                                                virtual-width virtual-height)
  "Run the chain from SOURCE (the terminal text) into TARGET.

PAINTED is cool-retro-term's `imagePainted': true when the terminal changed
since the last frame, and the only thing that advances the burn-in accumulator."
  (let ((profile (graph-profile graph)))
    (when virtual-width (setf (graph-virtual-width graph) (float virtual-width 1d0)))
    (when virtual-height (setf (graph-virtual-height graph) (float virtual-height 1d0)))
    (metal:with-metal
      (unless (graph-frame-valid graph)
        (render-frame-pass graph))
      (let* ((burn-in (if (and (plusp (settings:profile-burn-in profile)) painted)
                          (render-burn-in-pass graph source time)
                          (graph-burn-a graph)))
             (bloom (if (or (plusp (settings:profile-bloom profile))
                            (plusp (settings:profile-frame-shininess profile)))
                        (render-bloom-pass graph source)
                        (graph-bloom graph)))
             (static (render-static-pass graph source bloom)))
        (render-dynamic-pass graph target static burn-in time
                             :drawable drawable
                             :opacity (settings:profile-window-opacity profile))))))
