;;;; src/settings/profile.lisp -- the twenty-seven numbers that make a look.
;;;;
;;;; A profile is exactly cool-retro-term's: the same keys, the same names, the
;;;; same ranges.  Keeping the names identical is not sentiment -- it is what
;;;; lets a profile exported from either program be read by the other, which is
;;;; the only way anyone can check that this port is faithful.

(in-package #:cathode-ray-tube.settings)

;;; Rasterisation modes, from ApplicationSettings.qml.
(defconstant +raster-none+      0)
(defconstant +raster-scanlines+ 1)
(defconstant +raster-pixels+    2)
(defconstant +raster-subpixels+ 3)
(defconstant +raster-modern+    4)

(defstruct profile
  "The twenty-seven settings of a look, with cool-retro-term's own defaults.

The underscore-prefixed names upstream (_frameSize, _screenRadius, _margin,
_frameShininess) are the RAW 0..1 slider values; the derived quantities live in
derived.lisp and are computed rather than stored, exactly as QML's property
bindings compute them."
  (name "Custom" :type string)
  (ambient-light 0.2d0)
  (background-color "#000000")
  (bloom 0.55d0)
  (brightness 0.5d0)
  (burn-in 0.25d0)
  (chroma-color 0.25d0)
  (contrast 0.8d0)
  (flickering 0.1d0)
  (font-color "#ff8100")
  (font-name "TERMINESS_SCALED")
  (font-source 0)
  (font-width 1.0d0)
  (line-spacing 0.1d0)
  (glowing-line 0.2d0)
  (horizontal-sync 0.08d0)
  (jitter 0.2d0)
  (rasterization 0)
  (rgb-shift 0.0d0)
  (saturation-color 0.25d0)
  (screen-curvature 0.3d0)
  (screen-radius 0.2d0)
  (static-noise 0.12d0)
  (window-opacity 1.0d0)
  (margin 0.5d0)
  (blinking-cursor nil)
  (frame-size 0.2d0)
  (frame-color "#ffffff")
  (frame-shininess 0.2d0))

;;; Serialisation ----------------------------------------------------------------
;;;
;;; The wire names are camelCase because they are cool-retro-term's, and a
;;; profile file has to travel between the two programs.

(defparameter +profile-keys+
  '(("ambientLight"     profile-ambient-light     :real)
    ("backgroundColor"  profile-background-color  :string)
    ("bloom"            profile-bloom             :real)
    ("brightness"       profile-brightness        :real)
    ("burnIn"           profile-burn-in           :real)
    ("chromaColor"      profile-chroma-color      :real)
    ("contrast"         profile-contrast          :real)
    ("flickering"       profile-flickering        :real)
    ("fontColor"        profile-font-color        :string)
    ("fontName"         profile-font-name         :string)
    ("fontSource"       profile-font-source       :integer)
    ("fontWidth"        profile-font-width        :real)
    ("lineSpacing"      profile-line-spacing      :real)
    ("glowingLine"      profile-glowing-line      :real)
    ("horizontalSync"   profile-horizontal-sync   :real)
    ("jitter"           profile-jitter            :real)
    ("rasterization"    profile-rasterization     :integer)
    ("rgbShift"         profile-rgb-shift         :real)
    ("saturationColor"  profile-saturation-color  :real)
    ("screenCurvature"  profile-screen-curvature  :real)
    ("screenRadius"     profile-screen-radius     :real)
    ("staticNoise"      profile-static-noise      :real)
    ("windowOpacity"    profile-window-opacity    :real)
    ("margin"           profile-margin            :real)
    ("blinkingCursor"   profile-blinking-cursor   :boolean)
    ("frameSize"        profile-frame-size        :real)
    ("frameColor"       profile-frame-color       :string)
    ("frameShininess"   profile-frame-shininess   :real))
  "(WIRE-NAME ACCESSOR TYPE), in cool-retro-term's own order.")

(defun profile-to-alist (profile)
  (loop for (name accessor nil) in +profile-keys+
        collect (cons name (funcall accessor profile))))

(defun profile-from-alist (alist &key (into (make-profile)) name)
  "Apply ALIST to a profile, leaving anything it does not mention alone.

`Does not mention' is deliberate and matches loadProfileString, which applies
each key only when it is not undefined -- so a partial profile overrides what it
names and nothing else, and a profile written by a future version with extra
keys still loads."
  (loop for (wire accessor type) in +profile-keys+
        for entry = (assoc wire alist :test #'string-equal)
        when entry
          do (let ((value (cdr entry)))
               ;; (funcall #'(setf accessor) VALUE OBJECT) -- the new value comes
               ;; FIRST.  The other order type-errors with the value as the
               ;; datum, which reads like the value is wrong rather than the
               ;; call.
               (funcall (fdefinition (list 'setf accessor))
                        (ecase type
                          (:real (coerce value 'double-float))
                          (:integer (round value))
                          (:string (string value))
                          (:boolean (and value (not (eq value :false)))))
                        into)))
  (when name (setf (profile-name into) name))
  into)

(defun profile-to-json (profile &key (version 2))
  "A profile as cool-retro-term writes one, so its importer accepts ours."
  (com.inuoe.jzon:stringify
   (let ((table (make-hash-table :test 'equal)))
     (loop for (wire accessor type) in +profile-keys+
           do (setf (gethash wire table)
                    (let ((value (funcall accessor profile)))
                      (if (eq type :real)
                          ;; stringify() in Storage.qml rounds every number to
                          ;; four decimals.  Matching it keeps a round trip
                          ;; through either program byte-identical.
                          (/ (fround (* value 10000)) 10000)
                          value))))
     (setf (gethash "name" table) (profile-name profile)
           (gethash "version" table) version)
     table)
   :pretty t))

(defun profile-from-json (string &key (into (make-profile)))
  "Read a cool-retro-term profile.  Signals when the version is not 2."
  (let* ((table (com.inuoe.jzon:parse string))
         (version (gethash "version" table))
         (alist (loop for key being the hash-keys of table using (hash-value value)
                      collect (cons key value))))
    (when (and version (/= (round version) 2))
      (error "This is a version ~A profile; cathode-ray-tube reads version 2."
             version))
    (profile-from-alist alist :into into :name (gethash "name" table))))
