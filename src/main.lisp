;;;; src/main.lisp -- the entry point.

(in-package #:cathode-ray-tube)

(defun main ()
  "Start the application on the main thread.

TRIVIAL-MAIN-THREAD rather than simply calling RUN: from a REPL, or from any
image where Lisp's initial thread is not the one AppKit considers main, opening
a window on the wrong thread is not an error but a hang.  When we already ARE
the main thread this costs nothing."
  (handler-case
      (tmt:with-body-in-main-thread (:blocking t)
        (crt.ui:run-terminal))
    (error (condition)
      (format *error-output* "~&cathode-ray-tube: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
