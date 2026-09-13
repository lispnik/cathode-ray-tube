;;;; src/ui/frameworks.lisp -- bringing AppKit up, and the constants it needs.

(in-package #:cathode-ray-tube.ui)

;;; Cocoa constants.  `objc' has no constant tables and will not grow any, so
;;; these are hand-defined -- as they are in lem-cocoa, and for the same reason.
;;; Unlike Metal's they are few and famous enough not to need generating.

(defconstant +ns-window-style-titled+          1)
(defconstant +ns-window-style-closable+        2)
(defconstant +ns-window-style-miniaturizable+  4)
(defconstant +ns-window-style-resizable+       8)
(defconstant +ns-window-style-mask+
  (logior +ns-window-style-titled+ +ns-window-style-closable+
          +ns-window-style-miniaturizable+ +ns-window-style-resizable+))

(defconstant +ns-backing-store-buffered+ 2)

;;; NSApplicationActivationPolicy: Regular has a Dock icon and a menu bar, which
;;; is what a terminal wants.  (Accessory, 1, is the menu-bar-app policy
;;; utc-status-app uses.)
(defconstant +ns-application-activation-policy-regular+ 0)

;;; Run loop modes, named individually -- and NOT kCFRunLoopCommonModes.
;;;
;;; MEASURED, because the failure is silent.  -[CADisplayLink addToRunLoop:forMode:]
;;; accepts @"kCFRunLoopCommonModes" without complaint and then never fires:
;;;
;;;   mode kCFRunLoopCommonModes -> 0 frames in 1.5s
;;;   mode kCFRunLoopDefaultMode -> 91 frames in 1.5s
;;;
;;; kCFRunLoopCommonModes is a PSEUDO-mode: CFRunLoopAddSource understands it as
;;; "every mode in the common set", but a display link is not added through that
;;; path, and whatever it does add is filed under a mode name that no run loop
;;; ever runs in.  lem-cocoa/main-thread.lisp records the identical trap for
;;; -performSelectorOnMainThread:withObject:waitUntilDone:modes:, which is why
;;; that code also names its three modes one at a time.
;;;
;;; So they are named individually.  Default is the one that matters; the other
;;; two are why the effects keep animating while a window is being resized or a
;;; menu is open, instead of freezing the moment you grab a window edge.
(defparameter +run-loop-modes+
  '("kCFRunLoopDefaultMode"
    "NSEventTrackingRunLoopMode"
    "NSModalPanelRunLoopMode"))

(defun ensure-appkit ()
  "Bring the Objective-C runtime up.  Idempotent.

Delegates to CRT.METAL:ENSURE-FRAMEWORKS, which owns the one complete list of
modules.  There is exactly one initialisation point in this program because
objc's is process-global and once-only: see the note on +FRAMEWORKS+ for what
splitting it across two partial lists actually does."
  (crt.metal:ensure-frameworks)
  (register-application-defaults)
  t)

(defun register-application-defaults ()
  "Turn off press-and-hold, so holding a letter key REPEATS it.

macOS's press-and-hold shows the accent palette instead of repeating, which in a
terminal means that holding `j' in vi moves the cursor once and then puts up a
menu of j-with-diacritics.  Upstream does this at main.cpp:58 with
CFPreferencesSetAppValue.

-registerDefaults: rather than CFPreferencesSetAppValue, and the difference
matters: CFPreferencesSetAppValue WRITES the app's preferences, so a user who
had deliberately turned press-and-hold ON for this application would find it
turned back off, permanently, by launching it.  The registration domain is the
lowest-priority one, so this is a default rather than a decision -- anyone who
sets it explicitly still wins.  Same effect, one fewer thing taken away."
  (let* ((defaults (objc:invoke "NSUserDefaults" "standardUserDefaults"))
         (no (objc:invoke "NSNumber" "numberWithBool:" nil))
         (dictionary (objc:invoke "NSDictionary" "dictionaryWithObject:forKey:"
                                  no "ApplePressAndHoldEnabled")))
    (objc:invoke defaults "registerDefaults:" dictionary)))

(defmacro handling-errors ((what) &body body)
  "Run BODY, logging anything that escapes rather than letting it out.

MANDATORY around the body of every Lisp-implemented Objective-C method.  There
is no @try/@catch in this bridge, and a Lisp condition unwinding into AppKit
takes the process down with no backtrace worth reading -- so the boundary is
where conditions stop.  A frame that fails is a frame that is missing; a frame
that signals is an application that is gone."
  `(handler-case (progn ,@body)
     (error (condition)
       (format *error-output* "~&cathode-ray-tube: error in ~A: ~A~%" ,what condition)
       (finish-output *error-output*)
       nil)))
