;;;; src/settings/derived.lisp -- everything the shaders are actually handed.
;;;;
;;;; A profile stores the numbers a slider produces.  A shader wants something
;;;; else: a colour that has been mixed three times, a fade rate that is the
;;;; reciprocal of an interpolation, a curvature scaled by the window's size.
;;;; Every one of those conversions lives here and NOWHERE ELSE, because they
;;;; are where a port drifts -- each is individually plausible when wrong, and
;;;; the result is a terminal that looks *almost* like the original.
;;;;
;;;; These are ports of the property bindings in ApplicationSettings.qml lines
;;;; 28-135.  Where upstream's expression is surprising, it is preserved and the
;;;; surprise is noted rather than tidied away.

(in-package #:cathode-ray-tube.settings)

(defconstant +screen-curvature-size+ 0.6d0)
(defconstant +min-burn-in-fade-time+ 0.16d0)
(defconstant +max-burn-in-fade-time+ 1.6d0)
(defconstant +base-font-scaling+ 0.75d0)

;;; Colour -------------------------------------------------------------------------

(defun saturated-color (profile)
  "The font colour pulled toward white by the saturation slider.

    mix(fontColor, #FFFFFF, saturationColor * 0.5)

Note the 0.5: the slider runs 0..1 and reaches only halfway to white."
  (util:mix-color (util:str-to-color (profile-font-color profile))
                  (util:str-to-color "#ffffff")
                  (* (profile-saturation-color profile) 0.5d0)))

(defun contrast-mix (profile)
  "0.7 + contrast * 0.3 -- the weight both derived colours are mixed at.

So contrast 0 still mixes at 0.7 rather than 0: the two colours never collapse
together, which is why the lowest contrast setting is washed out rather than
blank."
  (+ 0.7d0 (* (profile-contrast profile) 0.3d0)))

(defun derived-font-color (profile)
  "What the shader is handed as `fontColor'."
  (util:mix-color (util:str-to-color (profile-background-color profile))
                  (saturated-color profile)
                  (contrast-mix profile)))

(defun derived-background-color (profile)
  "What the shader is handed as `backgroundColor'.

The same two colours as DERIVED-FONT-COLOR, mixed at the same weight, in the
OPPOSITE order -- which is what makes raising contrast move them apart."
  (util:mix-color (saturated-color profile)
                  (util:str-to-color (profile-background-color profile))
                  (contrast-mix profile)))

(defun derived-frame-color (profile)
  (util:str-to-color (profile-frame-color profile)))

;;; Geometry -------------------------------------------------------------------------

(defun frame-shininess (profile)
  (* (profile-frame-shininess profile) 0.5d0))

(defun frame-size (profile)
  (* (profile-frame-size profile) 0.05d0))

(defun screen-radius (profile)
  "The corner radius, in PIXELS -- not a fraction.

lint(4, 120, _screenRadius): even a profile with the slider at zero has four
pixels of rounding, because a perfectly square corner does not look like a
tube."
  (util:lint 4.0d0 120.0d0 (profile-screen-radius profile)))

(defun margin (profile)
  "The gap between the text and the screen edge, in pixels.

The second term is the interesting one: a rounded corner eats into the usable
rectangle, and (1 - 1/sqrt(2)) * radius is how far in the arc cuts at 45
degrees.  Without it, text on a heavily rounded profile runs under the bezel."
  (+ (util:lint 1.0d0 40.0d0 (profile-margin profile))
     (* (- 1.0d0 (/ 1.0d0 (sqrt 2.0d0))) (screen-radius profile))))

(defun frame-enabled-p (profile)
  "ambientLight > 0 || _frameSize > 0 || screenCurvature > 0.

Any one of the three means the bezel pass has something to do; all three at zero
means it can be skipped entirely, which is what the Boring and E-Ink profiles
rely on."
  (or (plusp (profile-ambient-light profile))
      (plusp (profile-frame-size profile))
      (plusp (profile-screen-curvature profile))))

(defun normalized-window-scale (width height)
  "1024 / (0.5 * width + 0.5 * height).

Keeps curvature and bezel geometry looking the same at every window size: the
distortion is defined in a space normalised to a nominal 1024-pixel window, so
without this a maximised window would have a barely-curved screen and a small
one would bulge."
  (/ 1024.0d0 (max 1.0d0 (+ (* 0.5d0 width) (* 0.5d0 height)))))

;;; Time ----------------------------------------------------------------------------

(defun burn-in-fade-time (profile)
  "The RATE, in reciprocal seconds, that the shaders call `burnInTime'.

    1 / lint(0.16, 1.6, burnIn)

A reciprocal, so a LARGER burnIn setting is a SLOWER fade -- and the shader
multiplies it by elapsed time to get how far the phosphor has decayed."
  (let ((seconds (util:lint +min-burn-in-fade-time+ +max-burn-in-fade-time+
                            (profile-burn-in profile))))
    (if (plusp seconds) (/ 1.0d0 seconds) 0.0d0)))

(defun horizontal-sync-strength (profile)
  "lint(0.05, 0.35, horizontalSync) -- both the threshold and the amplitude.

It is used twice in terminal_dynamic.vert: once compared against a noise sample
to decide WHETHER this frame tears, and again to scale how far.  So raising the
slider makes tearing both more frequent and more violent, which is why a small
change in it is so visible."
  (util:lint 0.05d0 0.35d0 (profile-horizontal-sync profile)))

;;; Rasterisation ---------------------------------------------------------------------

(defun rasterization-intensity (virtual-width virtual-height
                                screen-width screen-height)
  "smoothstep(2, 4, oversampling) -- the anti-moire ramp.

Scanlines need at least two device pixels per terminal pixel to be drawn at all
and four to be drawn cleanly; below that they alias into a shimmering mess that
moves when the window does.  So they fade out rather than being drawn badly,
which is why a small window on a non-Retina display shows no scanlines and that
is correct.

The two resolutions are DIFFERENT SPACES and conflating them is the classic
error here: VIRTUAL is the terminal's character-grid pixels, which set the
scanline frequency, and SCREEN is device pixels, which only ever feed this."
  (let ((density (min (/ screen-width (max 1.0d0 virtual-width))
                      (/ screen-height (max 1.0d0 virtual-height)))))
    (util:smoothstep 2.0d0 4.0d0 density)))
