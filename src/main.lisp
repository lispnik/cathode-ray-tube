;;;; src/main.lisp -- the entry point.

(in-package #:cathode-ray-tube)

(defun quiet-compiler-notes ()
  "Stop objc's trampoline compilation from filling the application log.

`objc' compiles one sb-alien trampoline per distinct method signature, at
runtime, and SBCL prints a `doing SAP to pointer coercion' note for each.  They
are a property of the bridge rather than of this program, and in a bundle they
go to ~/Library/Logs/cathode-ray-tube.log -- which should hold things that went
wrong, not a hundred notes about a coercion nobody can act on."
  (proclaim '(sb-ext:muffle-conditions sb-ext:compiler-note)))

(defun main ()
  "Start the application on the main thread.

TRIVIAL-MAIN-THREAD rather than simply calling RUN: from a REPL, or from any
image where Lisp's initial thread is not the one AppKit considers main, opening
a window on the wrong thread is not an error but a hang.  When we already ARE
the main thread this costs nothing."
  (quiet-compiler-notes)
  (handler-case
      (tmt:with-body-in-main-thread (:blocking t)
        (crt.ui:run-terminal))
    (error (condition)
      (format *error-output* "~&cathode-ray-tube: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
