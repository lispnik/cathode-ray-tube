;;;; src/ui/window.lisp -- the NSWindow and its delegate.

(in-package #:cathode-ray-tube.ui)

(defvar *windows* '() "Every live window, so nothing is collected under AppKit.")

(objc:define-objc-class window-delegate ()
  ((window :initform nil :accessor delegate-window))
  (:objc-class-name "CathodeRayTubeWindowDelegate"))

(objc:define-objc-method ("windowWillClose:" :void)
    ((self window-delegate) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (handling-errors ("windowWillClose:")
    (let ((window (delegate-window self)))
      (when window (close-crt-window window)))))

;;; A window that is off screen or fully covered still gets display-link ticks,
;;; and drawing frames nobody can see is the easiest waste in the program to
;;; avoid.  Effects genuinely animate every tick -- noise, flicker, jitter -- so
;;; pausing is only correct when nothing is visible, which is exactly what
;;; occlusion state reports.
(objc:define-objc-method ("windowDidChangeOcclusionState:" :void)
    ((self window-delegate) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (handling-errors ("windowDidChangeOcclusionState:")
    (let* ((crt (delegate-window self))
           (ns (and crt (crt-window-handle crt)))
           (view (and crt (crt-window-view crt))))
      (when (and ns view (view-link view))
        (let ((visible (logtest 2 (objc:invoke-into 'integer ns "occlusionState"))))
          (objc:invoke (view-link view) "setPaused:" (not visible)))))))

(defstruct (crt-window (:constructor %make-crt-window (handle view delegate)))
  handle view delegate)

(defun make-crt-window (&key (width 1024) (height 768) (title "cathode-ray-tube")
                             draw-function)
  "An NSWindow hosting a Metal view.  Main thread only."
  (ensure-appkit)
  (let* ((rect (vector 0d0 0d0 (float width 1d0) (float height 1d0)))
         (handle (objc:invoke (objc:invoke "NSWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              rect +ns-window-style-mask+
                              +ns-backing-store-buffered+ nil))
         ;; MAKE-INSTANCE on a DEFINE-OBJC-CLASS class is what registers the
         ;; Objective-C class and allocates the instance; the pointer is what
         ;; AppKit is handed.  The Lisp object must outlive it, which is what
         ;; *WINDOWS* is for.
         (view (make-instance 'crt-view))
         (delegate (make-instance 'window-delegate)))
    (setf (view-draw-function view) draw-function)
    ;; Lisp owns the window: closing it must not free it under us.
    (objc:invoke handle "setReleasedWhenClosed:" nil)
    (objc:invoke handle "setTitle:" title)
    (objc:invoke handle "setMinSize:" (vector 320d0 240d0))
    (objc:invoke (objc:objc-object-pointer view) "setFrame:" rect)
    (objc:invoke handle "setContentView:" (objc:objc-object-pointer view))
    (objc:invoke handle "setDelegate:" (objc:objc-object-pointer delegate))
    (objc:invoke handle "makeFirstResponder:" (objc:objc-object-pointer view))
    (objc:invoke handle "center")
    (attach-metal-layer view)
    (let ((window (%make-crt-window handle view delegate)))
      (setf (delegate-window delegate) window)
      (push window *windows*)
      window)))

(defun show-crt-window (window)
  (objc:invoke (crt-window-handle window) "makeKeyAndOrderFront:" nil)
  (objc:invoke (objc.runloop:shared-application) "activateIgnoringOtherApps:" t)
  (start-display-link (crt-window-view window))
  window)

(defun close-crt-window (window)
  "Stop the clock and forget the window.  Idempotent: -windowWillClose: can
arrive after an explicit close.

It does NOT terminate the application, and that is deliberate.  Quitting when the
last window closes is AppKit's decision, made by
-applicationShouldTerminateAfterLastWindowClosed:, which answers YES -- and
AppKit only asks while it is running its own event loop.  Calling -terminate:
from here instead exits the process the moment the last window goes, which is
indistinguishable from correct behaviour in the application and catastrophic in
a test: the suite's own window teardown ended the run, mid-suite, with status 0.
A green exit code and no summary."
  (stop-display-link (crt-window-view window))
  (setf *windows* (remove window *windows*)))
