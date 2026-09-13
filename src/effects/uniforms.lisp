;;;; src/effects/uniforms.lisp -- the blocks the effect shaders read.
;;;;
;;;; Each of these mirrors a `struct' in res/shaders/crt.metal, FIELD FOR FIELD
;;;; AND IN ORDER.  Nothing checks that automatically except
;;;; UNIFORM-LAYOUT-ROUND-TRIPS in the suite, which writes every field from here
;;;; and reads it back through a shader -- so if you add a field, add it in both
;;;; places and in the same position.

(in-package #:cathode-ray-tube.effects)

;;; Function constant indices, mirroring crt.metal's [[function_constant(n)]].
(defconstant +k-raster-mode+       0)
(defconstant +k-burn-in+           1)
(defconstant +k-display-frame+     2)
(defconstant +k-chroma+            3)
(defconstant +k-rgb-shift+         4)
(defconstant +k-bloom+             5)
(defconstant +k-curvature+         6)
(defconstant +k-frame-shininess+   7)

(metal:define-uniform-block frame-uniforms
  (:frame-color :float4)
  (:viewport-size :float2)
  (:screen-curvature :float)
  (:frame-size :float)
  (:screen-radius :float)
  (:ambient-light :float)
  (:frame-shininess :float)
  (:opacity :float))

(metal:define-uniform-block burn-in-uniforms
  (:last-update :float)
  (:previous-update :float)
  (:burn-in-time :float)
  (:opacity :float))

(metal:define-uniform-block blur-uniforms
  (:texel :float2)
  (:radius :float))

(metal:define-uniform-block static-uniforms
  (:screen-curvature :float)
  (:rgb-shift :float)
  (:frame-shininess :float)
  (:frame-size :float)
  (:screen-brightness :float)
  (:bloom :float)
  (:opacity :float))

(metal:define-uniform-block dynamic-uniforms
  (:font-color :float4)
  (:background-color :float4)
  (:virtual-resolution :float2)
  (:jitter-displacement :float2)
  (:scale-noise-size :float2)
  (:time :float)
  (:opacity :float)
  (:rasterization-intensity :float)
  (:burn-in-last-update :float)
  (:burn-in-time :float)
  (:static-noise :float)
  (:screen-curvature :float)
  (:glowing-line :float)
  (:chroma-color :float)
  (:jitter :float)
  (:horizontal-sync :float)
  (:horizontal-sync-strength :float)
  (:flickering :float)
  (:frame-size :float)
  (:bloom :float))

;;; Choosing a specialisation ------------------------------------------------------
;;;
;;; Ported from ShaderTerminal.qml's dynamicFragmentPath and staticFragmentPath,
;;; which build a .qsb FILENAME out of exactly these predicates.  Here they build
;;; a plist, and the pipeline cache turns that into a specialised pipeline.

(defun dynamic-pipeline-constants (profile)
  (list +k-raster-mode+ (settings:profile-rasterization profile)
        +k-burn-in+ (plusp (settings:profile-burn-in profile))
        +k-display-frame+ (settings:frame-enabled-p profile)
        +k-chroma+ (plusp (settings:profile-chroma-color profile))))

(defun static-pipeline-constants (profile)
  (list +k-rgb-shift+ (plusp (settings:profile-rgb-shift profile))
        +k-bloom+ (plusp (settings:profile-bloom profile))
        +k-curvature+ (or (plusp (settings:profile-screen-curvature profile))
                          (plusp (settings:profile-frame-size profile)))
        +k-frame-shininess+ (plusp (settings:profile-frame-shininess profile))))
