;;;; tests/effects-tests.lisp -- Tier 2: the CRT chain, pixel by pixel.

(in-package #:cathode-ray-tube/tests)
(in-suite effects)

(defun near~ (a b &optional (tol 3)) (<= (abs (- a b)) tol))

(defun centre-pixel (texture)
  "The middle pixel of TEXTURE.

Its OWN middle: the burn-in accumulator runs at burnInQuality scale and the
bloom at bloomQuality, so half the targets in the graph are smaller than the
drawable and assuming otherwise indexes off the end of the readback."
  (let ((pixels (crt.metal:texture-bytes texture)))
    (crt.metal:texture-pixel pixels texture
                         (floor (crt.metal:texture-width texture) 2)
                         (floor (crt.metal:texture-height texture) 2))))

(defun solid-texture (width height r g b &optional (a 255))
  "A texture filled with one colour, as a stand-in for the terminal."
  (let ((texture (crt.metal:make-texture :width width :height height :label "test source")))
    (crt.metal:with-render-pass (encoder texture
                             :clear (list (/ r 255d0) (/ g 255d0) (/ b 255d0)
                                          (/ a 255d0))))
    texture))

(defmacro with-graph ((graph profile &key (width 128) (height 96)) &body body)
  `(let ((,graph (crt.effects:make-graph :profile ,profile :width ,width :height ,height)))
     (unwind-protect (progn ,@body)
       (crt.effects:release-graph ,graph))))

(test uniform-layout-round-trips
  "Write every field from Lisp and read it back.

The shader's `constant' struct and the Lisp that fills it are two descriptions
of the same bytes and NOTHING checks they agree.  An offset one slot out puts a
float in the neighbouring field: no error, a picture that is subtly wrong, and
the thing that looks wrong is never the thing that is.  This is the cheapest
insurance available against that, and it runs without a GPU."
  (crt.metal:with-uniforms (u crt.effects::dynamic-uniforms)
    (setf (u :font-color) '(0.1 0.2 0.3 1.0)
          (u :background-color) '(0.4 0.5 0.6 1.0)
          (u :virtual-resolution) '(80.0 24.0)
          (u :time) 1.25
          (u :bloom) 2.5
          (u :chroma-color) 0.75)
    (is (every (lambda (a b) (< (abs (- a b)) 1d-6))
               '(0.1 0.2 0.3 1.0) (u :font-color)))
    (is (every (lambda (a b) (< (abs (- a b)) 1d-6))
               '(80.0 24.0) (u :virtual-resolution)))
    (is (< (abs (- 1.25 (u :time))) 1d-6))
    (is (< (abs (- 2.5 (u :bloom))) 1d-6)
        "a float after three vectors -- this is where an alignment error lands")
    (is (< (abs (- 0.75 (u :chroma-color))) 1d-6))
    ;; The fields set above must not have disturbed their neighbours.
    (is (zerop (u :jitter)) "an unset field must still be zero")))

(test uniform-alignment-follows-metal-rules
  (multiple-value-bind (layout size)
      (crt.metal:compute-uniform-layout '((:a :float) (:b :float4) (:c :float2)
                                      (:d :float)))
    (is (= 0 (third (assoc :a layout))))
    (is (= 16 (third (assoc :b layout)))
        "a float4 aligns to 16, so it cannot start at offset 4")
    (is (= 32 (third (assoc :c layout))))
    (is (= 40 (third (assoc :d layout))))
    (is (= 48 size) "the struct is rounded up to its widest member's alignment")))

(test vector-fields-are-checked
  "A two-element list into a float4 would silently leave two components stale."
  (crt.metal:with-uniforms (u crt.effects::dynamic-uniforms)
    (signals error (setf (u :font-color) '(1.0 0.0)))))

(test the-chain-runs-for-every-profile
  "Every one of the fourteen must build its pipelines and produce a frame.

The specialisation tuple differs per profile -- raster mode, burn-in, frame,
chroma, rgb shift, bloom, curvature, shininess -- so this compiles a couple of
dozen distinct pipelines and is the only thing that would catch a profile whose
combination does not compile."
  (when (gpu-or-skip)
    (dolist (profile crt.settings:+profiles+)
      (with-graph (graph profile :width 96 :height 64)
        (let ((source (solid-texture 96 64 128 128 128))
              (target (crt.metal:make-texture :width 96 :height 64)))
          (unwind-protect
               (finishes
                 (crt.effects:render-effects graph source target
                                         :time 1.0d0 :painted t
                                         :virtual-width 80 :virtual-height 24))
            (crt.metal:release-texture source)
            (crt.metal:release-texture target))))
      (is (stringp (crt.settings:profile-name profile))
          "~A rendered" (crt.settings:profile-name profile)))))

(test chroma-tints-toward-the-phosphor
  "Grey in, phosphor out.

With every animated effect off, no frame and no curvature, the dynamic pass
reduces to mix(backgroundColor, fontColor, rgb2grey(source)) -- so a mid-grey
source must come out on the line between the profile's two derived colours.
This is the single sharpest parity assertion available: the most complicated
shader in the project, reduced to a formula that can be computed in Lisp."
  (when (gpu-or-skip)
    (let ((profile (crt.settings:make-profile
                    :font-color "#ff8100" :background-color "#000000"
                    :static-noise 0d0 :jitter 0d0 :flickering 0d0
                    :glowing-line 0d0 :burn-in 0d0 :bloom 0d0
                    :horizontal-sync 0d0 :rgb-shift 0d0
                    :screen-curvature 0d0 :frame-size 0d0 :ambient-light 0d0
                    :frame-shininess 0d0 :chroma-color 0d0
                    :rasterization 0 :brightness 0.5d0 :contrast 0.8d0)))
      (with-graph (graph profile :width 64 :height 64)
        (let ((source (solid-texture 64 64 255 255 255))
              (target (crt.metal:make-texture :width 64 :height 64)))
          (unwind-protect
               (progn
                 (crt.effects:render-effects graph source target :time 0d0 :painted nil
                                                             :virtual-width 64
                                                             :virtual-height 64)
                 (let* ((pixels (crt.metal:texture-bytes target))
                        (centre (crt.metal:texture-pixel pixels target 32 32))
                        (font (crt.settings:derived-font-color profile)))
                   ;; White in, so grey is ~0.97 (the shader's weights sum to
                   ;; that, not to 1.0) and the result is almost pure font
                   ;; colour, times brightness 1.0 at the default 0.5 slider.
                   (is (> (first centre) 180)
                       "amber has a strong red channel; got ~S" centre)
                   (is (< (third centre) 60)
                       "and almost no blue; got ~S" centre)
                   (is (> (first centre) (second centre))
                       "red above green, as #ff8100 is")
                   (is (> (* 255 (crt.util:rgba-r font)) 200)
                       "sanity: the derived font colour is bright red")))
            (crt.metal:release-texture source)
            (crt.metal:release-texture target)))))))

(test the-bezel-is-opaque-at-the-corner-and-clear-in-the-middle
  "The frame pass alone, which is fully procedural and fully deterministic."
  (when (gpu-or-skip)
    (let ((profile (crt.settings:make-profile :ambient-light 0d0 :frame-size 0.5d0
                                          :frame-shininess 0d0 :screen-radius 0.2d0
                                          :screen-curvature 0d0)))
      (with-graph (graph profile :width 128 :height 128)
        (crt.effects::render-frame-pass graph)
        (let* ((frame (crt.effects::graph-frame graph))
               (pixels (crt.metal:texture-bytes frame))
               (corner (crt.metal:texture-pixel pixels frame 1 1))
               (centre (crt.metal:texture-pixel pixels frame 64 64)))
          (is (> (fourth corner) 200)
              "the corner is bezel and must be nearly opaque; got ~S" corner)
          (is (< (fourth centre) 40)
              "the centre is screen and must be nearly clear; got ~S" centre))))))

(test burn-in-decays-and-the-lit-mask-grants-one-frame
  "Light a pixel, then feed black, and watch the phosphor fade.

Deterministic arithmetic with no noise anywhere near it.  The part worth
asserting is the ALPHA MASK, which is not obvious from the picture and is easy
to drop in a port:

    blurDecay = max(0, blurDecay - prevMask)

Alpha is 1 wherever the pixel was lit LAST step, and subtracting it cancels a
decay of less than a whole unit -- so a pixel that was lit gets exactly one
frame of grace before it starts to fade.  That is what stops a glyph from
smearing into its own trail while it is still being drawn.  A port that omits
the mask decays one frame early everywhere, which is invisible on a still screen
and wrong on a moving one.

Measured here at burnIn 0.5, so the rate is 1/lint(0.16, 1.6, 0.5) = 1/0.88."
  (when (gpu-or-skip)
    (let ((profile (crt.settings:make-profile :burn-in 0.5d0)))
      (with-graph (graph profile :width 64 :height 64)
        (let ((lit (solid-texture 64 64 255 255 255))
              (dark (solid-texture 64 64 0 0 0)))
          (unwind-protect
               (let ((charged (crt.effects::render-burn-in-pass graph lit 0d0)))
                 (is (> (first (centre-pixel charged)) 200)
                     "the accumulator should be lit")
                 ;; One step later: the mask still says "was lit", so a decay of
                 ;; 0.05 * 1.136 = 0.057 is cancelled entirely.
                 (let ((grace (crt.effects::render-burn-in-pass graph dark 0.05d0)))
                   (is (> (first (centre-pixel grace)) 250)
                       "the lit mask must cancel the first step's decay; got ~D"
                       (first (centre-pixel grace))))
                 ;; The mask is clear now, so this step does fade.
                 (let ((fading (first (centre-pixel
                                       (crt.effects::render-burn-in-pass graph dark 0.10d0)))))
                   (is (< fading 255) "now it must decay; got ~D" fading)
                   (is (> fading 100) "but only a little; got ~D" fading))
                 ;; Long enough and it is gone.
                 (let ((gone (first (centre-pixel
                                     (crt.effects::render-burn-in-pass graph dark 5.0d0)))))
                   (is (< gone 16) "five seconds later it should be dark; got ~D"
                       gone)))
            (crt.metal:release-texture lit)
            (crt.metal:release-texture dark)))))))

(test burn-in-only-advances-when-the-terminal-painted
  "Otherwise the phosphor would decay at the DISPLAY's refresh rate.

The same session would then fade visibly differently on a 60Hz and a 120Hz
screen, which is the sort of bug that gets reported as 'burn-in looks wrong on
my new monitor' and is very hard to act on."
  (when (gpu-or-skip)
    (let ((profile (crt.settings:make-profile :burn-in 0.5d0)))
      (with-graph (graph profile :width 64 :height 64)
        (let ((source (solid-texture 64 64 255 255 255))
              (target (crt.metal:make-texture :width 64 :height 64)))
          (unwind-protect
               (progn
                 (crt.effects:render-effects graph source target :time 1d0 :painted t)
                 (let ((first-update (crt.effects::graph-burn-in-last-update graph)))
                   (crt.effects:render-effects graph source target :time 2d0 :painted nil)
                   (is (= first-update (crt.effects::graph-burn-in-last-update graph))
                       "an unpainted frame must not advance the accumulator")
                   (crt.effects:render-effects graph source target :time 3d0 :painted t)
                   (is (= 3d0 (crt.effects::graph-burn-in-last-update graph))
                       "a painted one must")))
            (crt.metal:release-texture source)
            (crt.metal:release-texture target)))))))

(test the-bezel-is-cached
  "It has no time uniform, so redrawing it every frame is pure waste."
  (when (gpu-or-skip)
    (let ((profile (crt.settings:find-profile "Default Amber")))
      (with-graph (graph profile :width 64 :height 64)
        (let ((source (solid-texture 64 64 64 64 64))
              (target (crt.metal:make-texture :width 64 :height 64)))
          (unwind-protect
               (progn
                 (crt.effects:render-effects graph source target :painted t)
                 (is-true (crt.effects::graph-frame-valid graph))
                 (crt.effects::invalidate-frame graph)
                 (is-false (crt.effects::graph-frame-valid graph))
                 (crt.effects:render-effects graph source target :painted t)
                 (is-true (crt.effects::graph-frame-valid graph)
                          "and it is redrawn once when invalidated"))
            (crt.metal:release-texture source)
            (crt.metal:release-texture target)))))))
