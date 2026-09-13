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

(defun command-line-arguments ()
  "The arguments, without the program name or a leading `--'.

The `--' appears when the program is run as `sbcl ... -- args', which is how
`make run' passes them, and is not one of ours."
  (let ((arguments (rest (or #+sbcl sb-ext:*posix-argv*
                             #-sbcl (uiop:raw-command-line-arguments)))))
    (if (equal (first arguments) "--") (rest arguments) arguments)))

(defun write-to-terminal (text)
  "Print TEXT where a person will see it.

IN A BUNDLE, STDOUT IS A LOG FILE.  asdf-macos-app redirects stdio to
~/Library/Logs/<name>.log so that a Finder-launched application has somewhere to
put its complaints -- which is right for an application and wrong for
`--help', whose entire purpose is to be read now.  So a print-and-exit flag
writes to /dev/tty when there is one, and falls back to stdout when there is
not, which is exactly the case the log exists for."
  (handler-case
      (with-open-file (tty "/dev/tty" :direction :output :if-exists :append
                                      :if-does-not-exist nil)
        (if tty
            (progn (write-string text tty) (finish-output tty))
            (progn (write-string text) (finish-output))))
    (error ()
      (write-string text)
      (finish-output))))

(defun apply-options (options)
  "Turn parsed options into the settings and the arguments RUN-TERMINAL takes."
  (unless (getf options :default-settings)
    (crt.settings:load-settings-into-place))
  (let ((settings crt.settings:*settings*))
    (list :profile (or (getf options :profile)
                       (crt.settings:settings-profile-name settings))
          :columns (or (getf options :columns) (crt.settings:settings-columns settings))
          :rows (or (getf options :rows) (crt.settings:settings-rows settings))
          :effects (if (member :effects options)
                       (getf options :effects)
                       (crt.settings:settings-effects settings))
          :fullscreen (getf options :fullscreen)
          :directory (getf options :directory)
          ;; `-e' only.  The custom-command setting is CRT.UI's business now --
          ;; see DEFAULT-SESSION-COMMAND -- because New Window and New Tab do not
          ;; come through here, and a setting honoured by the first terminal and
          ;; by none of the others is worse than one that does nothing.
          :command (getf options :command))))

(defun main ()
  "Start the application on the main thread.

TRIVIAL-MAIN-THREAD rather than simply calling RUN: from a REPL, or from any
image where Lisp's initial thread is not the one AppKit considers main, opening
a window on the wrong thread is not an error but a hang.  When we already ARE
the main thread this costs nothing."
  (quiet-compiler-notes)
  (let ((parsed (parse-command-line (command-line-arguments))))
    (cond
      ((getf parsed :exit)
       (write-to-terminal (getf parsed :exit))
       (uiop:quit 0))
      ((getf parsed :error)
       (write-to-terminal (format nil "~&cathode-ray-tube: ~A~%~%~A"
                                  (getf parsed :error) +usage+))
       (uiop:quit 2))))
  (handler-case
      (tmt:with-body-in-main-thread (:blocking t)
        (apply #'crt.ui:run-terminal
               (apply-options (parse-command-line (command-line-arguments)))))
    (error (condition)
      (format *error-output* "~&cathode-ray-tube: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
