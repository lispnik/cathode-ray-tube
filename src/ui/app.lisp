;;;; src/ui/app.lisp -- the application object, its delegate, and the run loop.

(in-package #:cathode-ray-tube.ui)

(objc:define-objc-class application-delegate ()
  ((launched :initform nil :accessor delegate-launched-p))
  (:objc-class-name "CathodeRayTubeAppDelegate"))

(objc:define-objc-method ("applicationDidFinishLaunching:" :void)
    ((self application-delegate) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (handling-errors ("applicationDidFinishLaunching:")
    (setf (delegate-launched-p self) t)))

;;; Closing the last window quits, which is what a terminal does.  (A browser
;;; would answer NO here and stay in the Dock.)
(objc:define-objc-method ("applicationShouldTerminateAfterLastWindowClosed:"
                          objc:objc-bool)
    ((self application-delegate) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  t)

(defvar *delegate* nil)

(defun make-menu-bar (&optional (name "cathode-ray-tube"))
  "The minimum menu bar an application needs to be quittable by Cmd-Q.

Without a menu bar AppKit gives the application no key-equivalent handling at
all, so Cmd-Q does nothing and the only way out is the window's close button.
The full menu arrives with the rest of the UI in M4."
  (let* ((main (objc:invoke (objc:invoke "NSMenu" "alloc") "init"))
         (app-item (objc:invoke (objc:invoke "NSMenuItem" "alloc") "init"))
         (app-menu (objc:invoke (objc:invoke "NSMenu" "alloc") "init"))
         (quit (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                            "initWithTitle:action:keyEquivalent:"
                            (format nil "Quit ~A" name)
                            (objc:coerce-to-selector "terminate:")
                            "q")))
    (objc:invoke app-menu "addItem:" quit)
    (objc:invoke app-item "setSubmenu:" app-menu)
    (objc:invoke main "addItem:" app-item)
    (objc:invoke (objc.runloop:shared-application) "setMainMenu:" main)
    main))

(defun gradient-frame (view texture drawable time)
  "The M1 draw function: a gradient, at vsync.

Replaced in M3 by the effect chain.  It is not a placeholder for its own sake --
it exercises the synthesised quad, a uniform block delivered by
setFragmentBytes:, a specialised pipeline, and presenting a drawable, which is
every mechanism the real graph is built from."
  (declare (ignore view))
  (let ((pipeline (crt.metal:pipeline :fragment "gradient_fragment"
                                      :constants (list 3 t)
                                      :pixel-format crt.metal:+pixel-format-bgra8unorm+
                                      :label "gradient")))
    (cffi:with-foreign-object (uniforms :float 2)
      (setf (cffi:mem-aref uniforms :float 0) (float time 1.0)
            (cffi:mem-aref uniforms :float 1) 1.0)
      (crt.metal:with-render-pass (encoder texture
                                   :clear '(0d0 0d0 0d0 1d0)
                                   :present drawable
                                   :label "gradient")
        (crt.metal:use-pipeline encoder pipeline)
        (crt.metal:bind-fragment-bytes encoder uniforms 8 0)
        (crt.metal:draw-quad encoder)))))

(defun run (&key (width 1024) (height 768) (draw-function #'gradient-frame))
  "Open a window and run the application.  Blocks; AppKit owns this thread.

Must be the MAIN thread -- AppKit is not merely thread-hostile about this, it
refuses.  MAIN is responsible for getting here on the right one."
  (ensure-appkit)
  (let ((app (objc.runloop:shared-application
              :activation-policy +ns-application-activation-policy-regular+)))
    (setf *delegate* (make-instance 'application-delegate))
    (objc:invoke app "setDelegate:" (objc:objc-object-pointer *delegate*))
    (make-menu-bar)
    (show-crt-window (make-crt-window :width width :height height
                                      :draw-function draw-function))
    (objc.runloop:run-cocoa-application)))
