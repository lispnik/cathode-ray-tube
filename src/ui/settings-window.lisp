;;;; src/ui/settings-window.lisp -- the settings window.
;;;;
;;;; Four tabs, as upstream has: General, Terminal, Effects, Advanced.  Each
;;;; control is a row in a table and the tables are the whole of the layout, so
;;;; adding a setting is a line rather than a diagram.
;;;;
;;;; WHAT IS BEING EDITED is the interesting part.  Upstream keeps one live
;;;; `appSettings' singleton; a profile is loaded INTO it and saved OUT of it,
;;;; and the terminal follows it continuously.  The fourteen built-in profiles
;;;; here are a shared, read-only table -- FIND-PROFILE hands out the same
;;;; structure to everyone -- so editing one in place would rewrite Default Amber
;;;; for every window and for the rest of the process.
;;;;
;;;; So opening this window takes a COPY of the key session's profile and gives
;;;; the session the copy.  From then on the controls edit that, which is exactly
;;;; upstream's arrangement with the shared table left intact.  Saving names the
;;;; copy and puts it in the custom profiles; the built-in it came from is
;;;; untouched either way.

(in-package #:cathode-ray-tube.ui)

(defvar *settings-window* nil
  "The one settings window.  Built on first use and kept, as its controls hold
closures over the session they were built for -- see REFRESH-SETTINGS-WINDOW.")

(defstruct (settings-window (:constructor %make-settings-window (handle tabs)))
  handle tabs
  (session nil)
  (profile nil))

;;; Applying an edit ------------------------------------------------------------

(defun apply-profile-edit (session &key font)
  "Make SESSION show its profile again after something in it changed.

FONT when the change moved a CELL -- the face, the line spacing, the font width,
the margin.  That rebuilds the renderer and re-fits the grid, which is expensive
and changes how many columns the child thinks it has, so it is not done for a
slider that only a shader reads.

Everything else needs only SET-GRAPH-PROFILE, and it needs that rather than
nothing because the bezel is CACHED: it has no time uniform and is redrawn only
when something invalidates it.  A screen radius you could drag with no effect
until the window was resized would look like the setting did nothing."
  (when session
    (when (session-graph session)
      (crt.effects:set-graph-profile (session-graph session)
                                     (session-profile session)))
    (when font
      (setf (session-margin session)
            (float (crt.settings:margin (session-profile session)) 1.0))
      (apply-font-scaling session))))

(defun editable-profile (session)
  "SESSION's profile, made private to it first if it was not already.

The built-in profiles are a shared table and FIND-PROFILE hands out the same
structure to every caller, so the first edit has to copy or Default Amber is
edited for the whole process."
  (let ((profile (session-profile session)))
    (if (crt.settings:builtin-profile-p (crt.settings:profile-name profile))
        (let ((copy (copy-structure profile)))
          (setf (session-profile session) copy)
          (when (session-graph session)
            (crt.effects:set-graph-profile (session-graph session) copy))
          copy)
        profile)))

;;; The rows --------------------------------------------------------------------

(defmacro define-profile-slider (window label reader writer
                                 &key (minimum 0) (maximum 1) font)
  "A row: LABEL, a slider from MINIMUM to MAXIMUM, and the value as a percentage."
  (let ((value (gensym)) (readout (gensym)) (session (gensym)) (profile (gensym)))
    `(let* ((,session (settings-window-session ,window))
            (,profile (settings-window-profile ,window))
            (,readout (make-label "" :alignment :right)))
       (list ,label
             (make-slider (,reader ,profile) ,minimum ,maximum
                          (lambda (,value)
                            (let ((p (editable-profile ,session)))
                              (,writer ,value p)
                              (objc:invoke ,readout "setStringValue:"
                                           (format nil "~D%" (round (* 100 ,value))))
                              (apply-profile-edit ,session :font ,font))))
             ,readout))))

(defun percent-label (value)
  (make-label (format nil "~D%" (round (* 100 value))) :alignment :right))

(defun effects-rows (window)
  "SettingsEffectsTab.qml, in its order.

Upstream's CheckableSlider is a checkbox and a slider together, where unchecking
sets the value to zero.  A slider that reaches zero is the same control with one
fewer thing to explain, and zero is what every one of these means by `off' --
the pipeline specialisation is chosen by testing exactly that."
  (let ((session (settings-window-session window))
        (profile (settings-window-profile window)))
    (flet ((row (label reader writer)
             (let ((readout (percent-label (funcall reader profile))))
               (list label
                     (make-slider
                      (funcall reader profile) 0 1
                      (lambda (value)
                        (funcall writer value (editable-profile session))
                        (objc:invoke readout "setStringValue:"
                                     (format nil "~D%" (round (* 100 value))))
                        (apply-profile-edit session)))
                     readout))))
      (list* (list :section "Effects")
            (list (row "Bloom" #'crt.settings:profile-bloom
                 (lambda (v p) (setf (crt.settings:profile-bloom p) v)))
            (row "Burn-in" #'crt.settings:profile-burn-in
                 (lambda (v p) (setf (crt.settings:profile-burn-in p) v)))
            (row "Static noise" #'crt.settings:profile-static-noise
                 (lambda (v p) (setf (crt.settings:profile-static-noise p) v)))
            (row "Jitter" #'crt.settings:profile-jitter
                 (lambda (v p) (setf (crt.settings:profile-jitter p) v)))
            (row "Glow line" #'crt.settings:profile-glowing-line
                 (lambda (v p) (setf (crt.settings:profile-glowing-line p) v)))
            (row "Screen curvature" #'crt.settings:profile-screen-curvature
                 (lambda (v p) (setf (crt.settings:profile-screen-curvature p) v)))
            (row "Ambient light" #'crt.settings:profile-ambient-light
                 (lambda (v p) (setf (crt.settings:profile-ambient-light p) v)))
            (row "Flickering" #'crt.settings:profile-flickering
                 (lambda (v p) (setf (crt.settings:profile-flickering p) v)))
            (row "Horizontal sync" #'crt.settings:profile-horizontal-sync
                 (lambda (v p) (setf (crt.settings:profile-horizontal-sync p) v)))
            (row "RGB shift" #'crt.settings:profile-rgb-shift
                 (lambda (v p) (setf (crt.settings:profile-rgb-shift p) v)))
            (row "Frame shininess" #'crt.settings:profile-frame-shininess
                 (lambda (v p) (setf (crt.settings:profile-frame-shininess p) v))))))))

(defun general-rows (window)
  "SettingsGeneralTab.qml: the profile list, and the five geometry sliders.

Opacity is here too, which upstream hides on macOS -- `visible:
!appSettings.isMacOS'.  That is a Qt limitation rather than a decision: a
translucent QML window on macOS did not composite correctly.  A CAMetalLayer
does, two of the fourteen profiles ask for it, and hiding a control that works
would be porting the workaround instead of the feature."
  (let* ((session (settings-window-session window))
         (profile (settings-window-profile window))
         ;; ALL-PROFILES returns PROFILES; the pop-up wants their names, and the
         ;; custom ones have to be in the list or saving one would make it
         ;; unreachable from the window that saved it.
         (names (mapcar #'crt.settings:profile-name (crt.settings:all-profiles))))
    (flet ((row (label reader writer &key font)
             (let ((readout (percent-label (funcall reader profile))))
               (list label
                     (make-slider
                      (funcall reader profile) 0 1
                      (lambda (value)
                        (funcall writer value (editable-profile session))
                        (objc:invoke readout "setStringValue:"
                                     (format nil "~D%" (round (* 100 value))))
                        (apply-profile-edit session :font font)))
                     readout))))
      (list*
       (list :section "Profile")
       ;; No row label: the heading immediately above it already says Profile,
       ;; and a section called Profile whose only row is called Profile reads
       ;; like a mistake.  The pop-up takes the full width instead.
       (list nil
             (make-popup names
                         (position (crt.settings:profile-name profile) names
                                   :test #'string=)
                         (lambda (index name)
                           (declare (ignore index))
                           (when session
                             (set-session-profile session name)
                             (setf (settings-window-profile window)
                                   (session-profile session))
                             (refresh-settings-window window)))))
       ;; One row, three buttons, each the width of its own title.  They were a
       ;; row each at the full width of the window before, which is how a Save
       ;; button ends up four hundred and sixty points wide.
       (list :group
             (make-push-button "Save as..." (lambda () (save-current-profile window)))
             (make-push-button "Export..." (lambda () (export-current-profile window)))
             (make-push-button "Import..." (lambda () (import-profile window))))
       (list :section "Screen")
       (list (row "Brightness" #'crt.settings:profile-brightness
                  (lambda (v p) (setf (crt.settings:profile-brightness p) v)))
             (row "Contrast" #'crt.settings:profile-contrast
                  (lambda (v p) (setf (crt.settings:profile-contrast p) v)))
             (row "Margin" #'crt.settings:profile-margin
                  (lambda (v p) (setf (crt.settings:profile-margin p) v)) :font t)
             (row "Radius" #'crt.settings:profile-screen-radius
                  (lambda (v p) (setf (crt.settings:profile-screen-radius p) v))
                  :font t)
             (row "Frame size" #'crt.settings:profile-frame-size
                  (lambda (v p) (setf (crt.settings:profile-frame-size p) v)))
             (row "Opacity" #'crt.settings:profile-window-opacity
                  (lambda (v p)
                    (setf (crt.settings:profile-window-opacity p) v)
                    (apply-session-opacity session))))))))

(defun terminal-rows (window)
  "SettingsTerminalTab.qml: where the glyphs come from and what colour they are."
  (let* ((session (settings-window-session window))
         (profile (settings-window-profile window))
         (rasterization (max 0 (min 4 (crt.settings:profile-rasterization profile))))
         ;; The Name list is filtered by the RENDERING mode, which is upstream's
         ;; `modernMode == !font.lowResolutionFont' (fontmanager.cpp:441).  The
         ;; sixteen bitmap faces and the eight outline ones are never offered
         ;; together: choosing a 32-pixel outline face in a profile that magnifies
         ;; bitmaps by a whole number is a combination with no sensible rendering.
         (bundled (crt.text:bundled-font-display-names :modern (= rasterization 4)))
         (system (crt.text:system-monospace-families))
         (source (if (eql 1 (crt.settings:profile-font-source profile)) 1 0))
         (names (if (= source 1) system bundled)))
    (flet ((colour (label reader writer)
             (list label
                   (make-color-well
                    (funcall reader profile)
                    (lambda (hex)
                      (funcall writer hex (editable-profile session))
                      (apply-profile-edit session)))
                   nil
                   crt.ui::+well-width+)))
      (list
       (list :section "Font")
       (list "Source"
             (make-popup '("Bundled" "System") source
                         (lambda (index name)
                           (declare (ignore name))
                           (let ((p (editable-profile session)))
                             (setf (crt.settings:profile-font-source p) index)
                             ;; The name in hand belongs to the other list, so
                             ;; changing source changes the face as well -- which
                             ;; is what upstream does when the current name is
                             ;; not in the filtered list (fontmanager.cpp:453).
                             (let ((first-name (first (if (= index 1) system bundled))))
                               (when first-name
                                 (setf (crt.settings:profile-font-name p)
                                       (if (= index 1)
                                           first-name
                                           (crt.text:profile-name-for-display
                                            first-name)))))
                             (apply-profile-edit session :font t)
                             (refresh-settings-window window)))))
       ;; Five modes, not two: 0 none, 1 scanlines, 2 pixels, 3 sub-pixels,
       ;; 4 modern (ApplicationSettings.qml:104-108).  The first four all
       ;; magnify a bitmap face and differ only in what the shader draws over it
       ;; -- they are CRT_RASTER_MODE, which is a function constant -- while the
       ;; fifth changes which faces exist.
       (list "Rendering"
             (make-popup '("Default" "Scanlines" "Pixels" "Sub-pixels" "Modern")
                         rasterization
                         (lambda (index name)
                           (declare (ignore name))
                           (let ((p (editable-profile session)))
                             (setf (crt.settings:profile-rasterization p) index)
                             ;; Crossing into or out of Modern changes the list of
                             ;; faces, so the name in hand may no longer be in it
                             ;; -- upstream picks the first of the filtered list
                             ;; in that case (fontmanager.cpp:453).
                             (let ((offered (crt.text:bundled-font-display-names
                                             :modern (= index 4))))
                               (unless (member (font-display-name p) offered
                                               :test #'string-equal)
                                 (setf (crt.settings:profile-font-name p)
                                       (crt.text:profile-name-for-display
                                        (first offered)))))
                             (apply-profile-edit session :font t)
                             (refresh-settings-window window)))))
       (list "Name"
             (make-popup names
                         (or (position (font-display-name profile) names
                                       :test #'string-equal)
                             0)
                         (lambda (index name)
                           (declare (ignore index))
                           (let ((p (editable-profile session)))
                             (setf (crt.settings:profile-font-name p)
                                   (if (= source 1)
                                       name
                                       (crt.text:profile-name-for-display name)))
                             (apply-profile-edit session :font t)))))
       (let ((readout (make-label (format nil "~,2F" (crt.settings:profile-font-width
                                                      profile))
                                  :alignment :right)))
         (list "Font width"
               (make-slider (crt.settings:profile-font-width profile) 0.5 2.0
                            (lambda (value)
                              (setf (crt.settings:profile-font-width
                                     (editable-profile session))
                                    value)
                              (objc:invoke readout "setStringValue:"
                                           (format nil "~,2F" value))
                              (apply-profile-edit session :font t)))
               readout))
       (let ((readout (make-label (format nil "~,2F"
                                          (crt.settings:profile-line-spacing profile))
                                  :alignment :right)))
         (list "Line spacing"
               (make-slider (crt.settings:profile-line-spacing profile) 0.0 0.5
                            (lambda (value)
                              (setf (crt.settings:profile-line-spacing
                                     (editable-profile session))
                                    value)
                              (objc:invoke readout "setStringValue:"
                                           (format nil "~,2F" value))
                              (apply-profile-edit session :font t)))
               readout))
       (list :section "Colour")
       (colour "Font colour" #'crt.settings:profile-font-color
               (lambda (v p) (setf (crt.settings:profile-font-color p) v)))
       (colour "Background" #'crt.settings:profile-background-color
               (lambda (v p) (setf (crt.settings:profile-background-color p) v)))
       (colour "Frame colour" #'crt.settings:profile-frame-color
               (lambda (v p) (setf (crt.settings:profile-frame-color p) v)))
       (let ((readout (percent-label (crt.settings:profile-chroma-color profile))))
         (list "Chroma"
               (make-slider (crt.settings:profile-chroma-color profile) 0 1
                            (lambda (value)
                              (setf (crt.settings:profile-chroma-color
                                     (editable-profile session))
                                    value)
                              (objc:invoke readout "setStringValue:"
                                           (format nil "~D%" (round (* 100 value))))
                              (apply-profile-edit session)))
               readout))
       (let ((readout (percent-label (crt.settings:profile-saturation-color profile))))
         (list "Saturation"
               (make-slider (crt.settings:profile-saturation-color profile) 0 1
                            (lambda (value)
                              (setf (crt.settings:profile-saturation-color
                                     (editable-profile session))
                                    value)
                              (objc:invoke readout "setStringValue:"
                                           (format nil "~D%" (round (* 100 value))))
                              (apply-profile-edit session)))
               readout))))))

(defun advanced-rows (window)
  "SettingsAdvancedTab.qml: the four global quality knobs and the shell.

One checkbox of upstream's is missing and is meant to be: `Show Menubar'.  Qt
draws its own menu bar inside the window, so hiding it is a real choice there.
On macOS the menu bar belongs to the system and to the frontmost application,
and an application that could hide it would be doing something other than what
that checkbox means.  Full Screen is the control that actually corresponds, and
it is already in the View menu where this platform puts it."
  (let ((session (settings-window-session window))
        (settings crt.settings:*settings*))
    (flet ((quality (label reader writer)
             (let ((readout (percent-label (funcall reader settings))))
               (list label
                     (make-slider (funcall reader settings) 0.25 1.0
                                  (lambda (value)
                                    (funcall writer value settings)
                                    (objc:invoke readout "setStringValue:"
                                                 (format nil "~D%"
                                                         (round (* 100 value))))
                                    (apply-quality-settings)
                                    (crt.settings:save-settings)))
                     readout))))
      (list
       (list :section "Shell")
       (list nil (make-checkbox
                  "Use a custom command instead of a shell"
                  (crt.settings:settings-use-custom-command settings)
                  (lambda (on)
                    (setf (crt.settings:settings-use-custom-command settings) on)
                    (crt.settings:save-settings))))
       (list "Command"
             (make-text-field (crt.settings:settings-custom-command settings)
                              (lambda (text)
                                (setf (crt.settings:settings-custom-command settings)
                                      text)
                                (crt.settings:save-settings))))
       (list :section "Terminal")
       (list nil (make-checkbox
                  "Blinking cursor"
                  (crt.settings:profile-blinking-cursor
                   (settings-window-profile window))
                  (lambda (on)
                    (setf (crt.settings:profile-blinking-cursor
                           (editable-profile session))
                          on)
                    (apply-profile-edit session))))
       (list nil (make-checkbox
                  "Show the terminal size while resizing"
                  (crt.settings:settings-show-terminal-size settings)
                  (lambda (on)
                    (setf (crt.settings:settings-show-terminal-size settings) on)
                    (apply-overlay-setting)
                    (crt.settings:save-settings))))
       (list :section "Quality")
       ;; Upstream calls this "Effects FPS" and shows 100/N as a percentage, so
       ;; the slider runs over the SKIP and the readout over the rate.  Keeping
       ;; the skip as the quantity means the label says what the number is.
       (let* ((skip (crt.settings:settings-effects-frame-skip settings))
              (readout (make-label (format nil "1 in ~D" skip) :alignment :right)))
         (list "Effects frames"
               (make-slider skip 1 10
                            (lambda (value)
                              (let ((n (max 1 (min 10 (round value)))))
                                (setf (crt.settings:settings-effects-frame-skip
                                       settings)
                                      n)
                                (objc:invoke readout "setStringValue:"
                                             (format nil "1 in ~D" n))
                                (apply-quality-settings)
                                (crt.settings:save-settings))))
               readout))
       (quality "Texture quality" #'crt.settings:settings-window-scaling
                (lambda (v s) (setf (crt.settings:settings-window-scaling s) v)))
       (quality "Bloom quality" #'crt.settings:settings-bloom-quality
                (lambda (v s) (setf (crt.settings:settings-bloom-quality s) v)))
       (quality "Burn-in quality" #'crt.settings:settings-burn-in-quality
                (lambda (v s) (setf (crt.settings:settings-burn-in-quality s) v)))))))

;;; Applying the global settings ------------------------------------------------

(defun apply-quality-settings ()
  "Push the global knobs into every open session.

Global, so every window: these are not part of a profile and upstream's are not
either."
  (let ((settings crt.settings:*settings*))
    (dolist (session *sessions*)
      (setf (view-frame-skip (session-view session))
            (crt.settings:settings-effects-frame-skip settings))
      (when (session-graph session)
        (crt.effects:set-graph-quality
         (session-graph session)
         :window-scaling (crt.settings:settings-window-scaling settings)
         :bloom-quality (crt.settings:settings-bloom-quality settings)
         :burn-in-quality (crt.settings:settings-burn-in-quality settings))))))

(defun apply-overlay-setting ()
  "Give every session an overlay, or take it away, to match the setting."
  (let ((wanted (crt.settings:settings-show-terminal-size crt.settings:*settings*)))
    (dolist (session *sessions*)
      (cond ((and wanted (null (session-overlay session)))
             (setf (session-overlay session) (crt.text:make-overlay)))
            ((and (not wanted) (session-overlay session))
             (crt.text:release-overlay (session-overlay session))
             (setf (session-overlay session) nil))))))

;;; The window ------------------------------------------------------------------

(defconstant +settings-width+ 560)
(defconstant +settings-height+ 520)
(defconstant +tab-inset+ 8
  "Breathing room between the scroller and the tab's own content rect.")

(defun font-display-name (profile)
  "What the Name pop-up should have selected for PROFILE."
  (if (eql 1 (crt.settings:profile-font-source profile))
      (crt.settings:profile-font-name profile)
      (let ((face (crt.text:font-for-profile-name
                   (crt.settings:profile-font-name profile))))
        (if face (crt.text:font-display-name-for face) ""))))

(defun make-scrolling-tab (title rows width height)
  "One tab of the settings window, scrolling if its rows do not fit.

Scrolling because the Effects tab is eleven sliders and the window is a fixed
size: a tab that simply clipped its last two controls would look like the port
had stopped halfway.

WIDTH and HEIGHT are the tab view's OWN content rect, measured and passed in
rather than derived from the window size by subtracting a guess at the chrome.
The guess was wrong by four points, which is enough to put a horizontal
scroller under a form that fits.

MINIMUM-HEIGHT makes a short form fill the scroller.  Without it the document
view is shorter than the clip view, and AppKit pins a short document to the
BOTTOM -- which drew eight rows in the lower half of the tab under a band of
empty grey.  The flipped form view decides where the rows start; this decides
what is behind them."
  (let* ((form (make-form width rows :minimum-height height))
         (scroll (objc:invoke (objc:invoke "NSScrollView" "alloc")
                              "initWithFrame:"
                              (vector 0d0 0d0 (float width 1d0) (float height 1d0))))
         (item (objc:invoke (objc:invoke "NSTabViewItem" "alloc")
                            "initWithIdentifier:" title)))
    (objc:invoke scroll "setHasVerticalScroller:" t)
    (objc:invoke scroll "setDrawsBackground:" nil)
    (objc:invoke scroll "setAutohidesScrollers:" t)
    (objc:invoke scroll "setBorderType:" 0)          ; NSNoBorder
    ;; NSViewWidthSizable | NSViewHeightSizable, so the scroller follows the tab
    ;; view.  Its frame used to be a guess, and the guess was VISIBLE: NSTabView
    ;; resizes only the selected tab's view, so the three not showing kept what
    ;; they were built with and jumped on first selection.
    (objc:invoke scroll "setAutoresizingMask:" (logior 2 16))
    (objc:invoke scroll "setDocumentView:" (objc:objc-object-pointer form))
    (objc:invoke item "setLabel:" title)
    (objc:invoke item "setView:" (objc:objc-object-pointer scroll))
    item))

(defun build-settings-window (session)
  (let* ((handle (objc:invoke (objc:invoke "NSWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              (vector 0d0 0d0 (float +settings-width+ 1d0)
                                      (float +settings-height+ 1d0))
                              ;; Titled | Closable | Miniaturizable: no resize,
                              ;; because the forms are laid out at a fixed width
                              ;; and a resizable window that did not re-lay them
                              ;; out would be worse than one that cannot be
                              ;; dragged.  1 | 2 | 4.
                              7 +ns-backing-store-buffered+ nil))
         (tabs (objc:invoke (objc:invoke "NSTabView" "alloc")
                            "initWithFrame:"
                            (vector 0d0 0d0 (float +settings-width+ 1d0)
                                    (float +settings-height+ 1d0))))
         (window (%make-settings-window handle tabs)))
    (objc:invoke handle "setTitle:" "Settings")
    (objc:invoke handle "setReleasedWhenClosed:" nil)
    (objc:invoke handle "setContentView:" (objc:objc-object-pointer tabs))
    (setf (settings-window-session window) session
          (settings-window-profile window) (and session (session-profile session)))
    (populate-settings-window window)
    window))

(defun populate-settings-window (window)
  "Build every tab from scratch, for the window's current session.

From scratch on every refresh, and that is the simplest thing that is correct:
each control closes over the session and the profile it was built for, so a
window that changed session -- or a profile that changed underneath it -- would
otherwise be editing something that is no longer there.  Forty controls is
nothing to rebuild, and it happens when a person clicks a pop-up."
  (let ((tabs (settings-window-tabs window)))
    (loop while (plusp (objc:invoke-into 'integer tabs "numberOfTabViewItems"))
          do (objc:invoke tabs "removeTabViewItem:"
                          (objc:invoke tabs "tabViewItemAtIndex:" 0)))
    (when (settings-window-session window)
      ;; ASK the tab view how much room a tab actually gets, rather than
      ;; subtracting a guess at the strip and the margins from the window size.
      (let* ((content (objc:invoke-into (vector 0d0 0d0 0d0 0d0) tabs "contentRect"))
             (width (max 200 (- (floor (aref content 2)) (* 2 +tab-inset+))))
             (height (max 200 (floor (aref content 3)))))
        (dolist (tab (list (cons "General" (general-rows window))
                           (cons "Terminal" (terminal-rows window))
                           (cons "Effects" (effects-rows window))
                           (cons "Advanced" (advanced-rows window))))
          (objc:invoke tabs "addTabViewItem:"
                       (objc:objc-object-pointer
                        (make-scrolling-tab (car tab) (cdr tab) width height))))))))

(defun refresh-settings-window (window)
  (setf (settings-window-profile window)
        (and (settings-window-session window)
             (session-profile (settings-window-session window))))
  (populate-settings-window window))

(defun show-settings-window ()
  "Open the settings window on the key session.  Main thread only."
  (ensure-appkit)
  (let ((session (key-session)))
    (cond
      ((null session) nil)
      (t
       (unless *settings-window*
         (setf *settings-window* (build-settings-window session)))
       (let ((window *settings-window*))
         (unless (eq session (settings-window-session window))
           (setf (settings-window-session window) session)
           (refresh-settings-window window))
         (objc:invoke (settings-window-handle window) "center")
         (objc:invoke (settings-window-handle window) "makeKeyAndOrderFront:"
                      (cffi:null-pointer))
         window)))))

;;; Profiles: saving, importing, exporting --------------------------------------

(defun prompt-for-text (message default)
  "A modal NSAlert with one text field.  The string, or NIL if cancelled.

NSAlert rather than a window of our own because this is upstream's
InsertNameDialog and there is nothing in it but a name."
  (let* ((alert (objc:invoke (objc:invoke "NSAlert" "alloc") "init"))
         (field (objc:invoke (objc:invoke "NSTextField" "alloc") "initWithFrame:"
                             (vector 0d0 0d0 240d0 24d0))))
    (objc:invoke alert "setMessageText:" message)
    (objc:invoke alert "addButtonWithTitle:" "Save")
    (objc:invoke alert "addButtonWithTitle:" "Cancel")
    (objc:invoke field "setStringValue:" (or default ""))
    (objc:invoke alert "setAccessoryView:" (objc:objc-object-pointer field))
    ;; NSAlertFirstButtonReturn is 1000, and it is the only number here: the
    ;; alternative is comparing against the count of buttons added, which is the
    ;; same constant written less clearly.
    (let ((response (objc:invoke-into 'integer alert "runModal")))
      (when (= response 1000)
        (let ((text (objc:invoke-into 'string field "stringValue")))
          (when (and text (plusp (length (string-trim " " text))))
            (string-trim " " text)))))))

(defun save-current-profile (window)
  "Name the edited profile and keep it, leaving the built-in it came from alone."
  (let* ((profile (settings-window-profile window))
         (suggestion (crt.settings:profile-name profile))
         (name (prompt-for-text "Save this profile as:"
                                (if (crt.settings:builtin-profile-p suggestion)
                                    (concatenate 'string suggestion " copy")
                                    suggestion))))
    (when name
      (setf (crt.settings:profile-name profile) name)
      ;; SAVE-CUSTOM-PROFILE serialises and saves the settings itself.
      (crt.settings:save-custom-profile name profile)
      (refresh-settings-window window)
      name)))

(defun run-file-panel (kind &key (name "profile.json"))
  "An open or save panel.  The pathname, or NIL.

KIND is :OPEN or :SAVE.  -runModal on either returns 1 for OK, which is
NSModalResponseOK -- the one place AppKit's answer is not the 1000-based
NSAlert numbering."
  (ecase kind
    (:open
     (let ((panel (objc:invoke "NSOpenPanel" "openPanel")))
       (objc:invoke panel "setAllowsMultipleSelection:" nil)
       (objc:invoke panel "setCanChooseDirectories:" nil)
       (when (= 1 (objc:invoke-into 'integer panel "runModal"))
         (let ((url (objc:invoke panel "URL")))
           (unless (crt.metal:null-object-p url)
             (objc:invoke-into 'string url "path"))))))
    (:save
     (let ((panel (objc:invoke "NSSavePanel" "savePanel")))
       (objc:invoke panel "setNameFieldStringValue:" name)
       (when (= 1 (objc:invoke-into 'integer panel "runModal"))
         (let ((url (objc:invoke panel "URL")))
           (unless (crt.metal:null-object-p url)
             (objc:invoke-into 'string url "path"))))))))

(defun export-current-profile (window)
  "Write the edited profile as cool-retro-term's own JSON.

Its JSON, exactly, so that the file can be imported by the other program --
which is the whole reason a port keeps upstream's wire names."
  (let* ((profile (settings-window-profile window))
         (path (run-file-panel
                :save :name (format nil "~A.json"
                                    (substitute #\- #\Space
                                                (crt.settings:profile-name profile))))))
    (when path
      (with-open-file (stream path :direction :output :if-exists :supersede
                                   :external-format :utf-8)
        (write-string (crt.settings:profile-to-json profile) stream))
      path)))

(defun import-profile (window)
  "Read a cool-retro-term profile and apply it to the session."
  (let ((path (run-file-panel :open)))
    (when path
      (handler-case
          (let* ((json (uiop:read-file-string path))
                 (session (settings-window-session window))
                 (profile (crt.settings:profile-from-json
                           json :into (copy-structure (session-profile session)))))
            (setf (crt.settings:profile-name profile)
                  (pathname-name (pathname path))
                  (session-profile session) profile)
            (apply-session-opacity session)
            (apply-profile-edit session :font t)
            (refresh-settings-window window)
            profile)
        (error (condition)
          (let ((alert (objc:invoke (objc:invoke "NSAlert" "alloc") "init")))
            (objc:invoke alert "setMessageText:" "That file is not a profile.")
            (objc:invoke alert "setInformativeText:" (princ-to-string condition))
            (objc:invoke alert "runModal"))
          nil)))))
