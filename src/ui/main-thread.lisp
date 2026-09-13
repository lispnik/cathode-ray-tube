;;;; src/ui/main-thread.lisp -- getting onto the main thread from elsewhere.
;;;;
;;;; Almost nothing needs this.  The design keeps AppKit and Metal on the main
;;;; thread and the pty on its own, and the two meet through a lock and a
;;;; snapshot rather than through calls.  What is left is the one genuinely
;;;; asynchronous event: the child exits, the reader thread notices, and a
;;;; window has to close -- which is AppKit's business and must happen there.
;;;;
;;;; THE MODES ARE NAMED ONE AT A TIME, and never kCFRunLoopCommonModes.
;;;; lem-cocoa/main-thread.lisp records that a perform posted to the common-modes
;;;; pseudo-mode from a Lisp thread measurably never ran, and this project found
;;;; the same thing independently for CADisplayLink -- see the note in
;;;; frameworks.lisp, which has the frame counts.  Twice is a pattern.

(in-package #:cathode-ray-tube.ui)

(defvar *main-thread-queue* '())
(defvar *main-thread-lock* (bt2:make-lock :name "main thread queue"))
(defvar *main-thread-target* nil
  "The Objective-C object whose drain method AppKit calls.")

(objc:define-objc-class main-thread-pump ()
  ()
  (:objc-class-name "CathodeRayTubeMainThreadPump"))

(objc:define-objc-method ("crtDrainQueue:" :void)
    ((self main-thread-pump) (argument objc:objc-object-pointer))
  (declare (ignore argument))
  (handling-errors ("crtDrainQueue:")
    (drain-main-thread-queue)))

(defun ensure-main-thread-target ()
  (or *main-thread-target*
      (setf *main-thread-target* (make-instance 'main-thread-pump))))

(defun drain-main-thread-queue ()
  (let ((pending (bt2:with-lock-held (*main-thread-lock*)
                   (prog1 (nreverse *main-thread-queue*)
                     (setf *main-thread-queue* nil)))))
    (dolist (thunk pending)
      ;; One failing thunk must not swallow the rest, and none of them may
      ;; unwind into AppKit.
      (handler-case (funcall thunk)
        (error (condition)
          (format *error-output* "~&cathode-ray-tube: main-thread work: ~A~%"
                  condition))))))

(defun on-main-thread (thunk)
  "Run THUNK on the main thread, soon.  Safe from any thread.

Returns immediately: nothing here waits, because the only caller is the pty
reader announcing that its child has gone, and blocking that thread on the UI
would be backwards."
  (if (objc.runloop:main-thread-p)
      (funcall thunk)
      (progn
        (bt2:with-lock-held (*main-thread-lock*) (push thunk *main-thread-queue*))
        (let ((target (ensure-main-thread-target)))
          (objc:invoke (objc:objc-object-pointer target)
                       "performSelectorOnMainThread:withObject:waitUntilDone:modes:"
                       (objc:coerce-to-selector "crtDrainQueue:")
                       (cffi:null-pointer)
                       nil
                       (let ((modes (objc:invoke "NSMutableArray" "array")))
                         (dolist (mode +run-loop-modes+ modes)
                           (objc:invoke modes "addObject:" mode))))))))
