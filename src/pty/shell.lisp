;;;; src/pty/shell.lisp -- what to run, when nobody said.

(in-package #:cathode-ray-tube.pty)

(defun default-shell ()
  "The user's shell: $SHELL, then the system default.

/bin/zsh rather than /bin/sh as the fallback, because it has been the macOS
default since Catalina and a terminal that silently drops you into sh when
$SHELL is unset looks broken rather than conservative."
  (let ((shell (uiop:getenv "SHELL")))
    (or (and shell (plusp (length shell)) (probe-file shell) shell)
        (and (probe-file "/bin/zsh") "/bin/zsh")
        "/bin/sh")))

(defun login-shell-arguments (&optional (shell (default-shell)))
  "The argument list for SHELL as an interactive login shell.

macOS expects `-i -l', and cool-retro-term passes exactly that
(PreprocessedTerminal.qml: `ksession.setArgs([\"-i\", \"-l\"])' on macOS).  It
matters more here than on Linux: /etc/zprofile runs path_helper, and without a
login shell a terminal on macOS has a $PATH missing everything in
/etc/paths.d -- which users notice immediately and blame on the terminal."
  (list shell "-i" "-l"))

(defun shell-command (&key command)
  "COMMAND as a list of strings, or the login shell when it is NIL."
  (cond ((null command) (login-shell-arguments))
        ((stringp command) (list command))
        (t command)))
