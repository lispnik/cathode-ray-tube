;;;; tools/settings-shot.lisp -- the settings window, rendered into docs/settings.
;;;;
;;;; `make settings-shot'.  The counterpart to tools/gallery.lisp, and checked in
;;;; for the same reason: a picture nobody can regenerate is a picture nobody can
;;;; trust after the next change.  The four images under docs/settings/ came from
;;;; an ad-hoc script the first time, which meant the only record of how they
;;;; were made was a commit message.
;;;;
;;;; TWO THINGS ABOUT THE CAPTURE, both learned the hard way.
;;;;
;;;; -dataWithPDFInsideRect: draws the text and SKIPS the layer-backed controls,
;;;; so the first attempt produced labels floating on white with no sliders,
;;;; pop-ups or checkboxes anywhere -- and a colour well stretched across the
;;;; whole form went unnoticed because it was invisible.
;;;; -cacheDisplayInRect:toBitmapImageRep: captures everything.
;;;;
;;;; The appearance is forced to Aqua.  NSColor's labelColor follows the system,
;;;; so on a machine in dark mode the form renders near-white text on a
;;;; transparent ground, which converts to white on white and looks like a bug in
;;;; the layout rather than in the screenshot.
;;;;
;;;; Neither of these needs Screen Recording permission, which is what makes this
;;;; possible at all: the window is never photographed, it is asked to draw
;;;; itself into a bitmap.

(defpackage #:crt-settings-shot
  (:use #:cl)
  (:export #:render-all))

(in-package #:crt-settings-shot)

(defparameter +output+ "docs/settings/")

(defun aqua (view)
  "Force a light appearance, so labelColor is dark whatever the machine is set to."
  (objc:invoke view "setAppearance:"
               (objc:invoke "NSAppearance" "appearanceNamed:" "NSAppearanceNameAqua"))
  view)

(defun shoot (view path)
  (let* ((bounds (objc:invoke-into (vector 0d0 0d0 0d0 0d0) view "bounds"))
         (rep (objc:invoke view "bitmapImageRepForCachingDisplayInRect:" bounds)))
    (when (crt.metal:null-object-p rep)
      (error "Could not make a bitmap for ~A." path))
    (objc:invoke view "cacheDisplayInRect:toBitmapImageRep:" bounds rep)
    (let ((data (objc:invoke rep "representationUsingType:properties:"
                             4               ; NSBitmapImageFileTypePNG
                             (objc:invoke "NSDictionary" "dictionary"))))
      (objc:invoke data "writeToFile:atomically:" (namestring path) t)
      (format t "~&~24A ~,0Fx~,0F~%" (file-namestring path)
              (aref bounds 2) (aref bounds 3)))))

(defun render-all (&key (directory +output+))
  "Render every tab of the settings window.  Needs a window server and a GPU."
  (crt.ui:ensure-appkit)
  (objc.runloop:shared-application :activation-policy 0)
  (ensure-directories-exist directory)
  (let ((session (crt.ui:make-session
                  :width 900 :height 560
                  :command '("/bin/sh" "-c" "sleep 60"))))
    (unwind-protect
         (let* ((window (crt.ui:show-settings-window))
                (handle (crt.ui:settings-window-handle window))
                (tabs (crt.ui:settings-window-tabs window)))
           (aqua handle)
           (objc:invoke handle "makeKeyAndOrderFront:" (cffi:null-pointer))
           (objc.runloop:pump-events :seconds 0.02d0 :max-seconds 1.5d0)
           (dotimes (i (objc:invoke-into 'integer tabs "numberOfTabViewItems"))
             (objc:invoke tabs "selectTabViewItemAtIndex:" i)
             (objc.runloop:pump-events :seconds 0.02d0 :max-seconds 0.8d0)
             (let* ((item (objc:invoke tabs "tabViewItemAtIndex:" i))
                    (label (objc:invoke-into 'string item "label"))
                    (form (objc:invoke (objc:invoke item "view") "documentView")))
               (aqua form)
               (objc:invoke form "setNeedsDisplay:" t)
               (objc:invoke form "displayIfNeeded")
               (objc.runloop:pump-events :seconds 0.02d0 :max-seconds 0.3d0)
               (shoot form (merge-pathnames (format nil "~(~A~).png" label)
                                            directory)))))
      (crt.ui:end-session session))))
