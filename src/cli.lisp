;;;; src/cli.lisp -- the command line.
;;;;
;;;; cool-retro-term's flags, with the same names and the same meanings, so that
;;;; a habit or a script carries over.  `-e' catches everything after it, which
;;;; is why it has to be last -- upstream says so in its own help text.

(in-package #:cathode-ray-tube)

(defparameter +usage+
  "cathode-ray-tube -- a terminal emulator that looks like a cathode-ray tube.

  --default-settings   start with the defaults, ignoring the saved settings
  --workdir <dir>      start the shell in <dir>
  -e <cmd> [args...]   run <cmd> instead of a shell.  Catches every following
                       argument, so it must come last
  -p, --profile <name> start with the named profile
  --list-profiles      print the profile names and exit
  --geometry <C>x<R>   start at C columns by R rows (default 80x25)
  --no-effects         no CRT effects; plain text on black
  --fullscreen         start full screen
  -v, --version        print the version and exit
  -h, --help           print this and exit

Use the SHORT forms from inside the .app bundle.  Its executable is the SBCL
runtime, which parses --help and --version as its own before any of this runs
and prints SBCL's instead; -h and -v are not runtime options and reach us.
(asdf-macos-app would have to dump with :save-runtime-options to stop that.)
")

(defun tokenize-command-line (string)
  "Split STRING into a program and arguments, honouring quotes and backslashes.

A port of utils.js's tokenizeCommandLine, which is GLib's tokeniser, which is
what the custom-command setting has always been parsed by.  SPLIT-STRING would
break on the first path containing a space.

An explicitly empty argument -- `echo \"\"' -- survives, which is why the
tokeniser tracks whether a token was STARTED separately from whether it has any
characters in it."
  (let ((arguments '())
        (current (make-array 0 :element-type 'character :adjustable t
                               :fill-pointer 0))
        (started nil)
        (quote-character nil)
        (escaped nil))
    (flet ((finish ()
             (when (or started (plusp (fill-pointer current)))
               (push (copy-seq current) arguments))
             (setf (fill-pointer current) 0
                   started nil)))
      (loop for character across string
            do (cond
                 (escaped
                  (setf escaped nil started t)
                  (vector-push-extend character current))
                 (quote-character
                  (cond ((char= character #\\) (setf escaped t))
                        ((char= character quote-character)
                         (setf quote-character nil))
                        (t (vector-push-extend character current))))
                 (t
                  (case character
                    (#\\ (setf escaped t))
                    ((#\Space #\Tab #\Newline #\Return) (finish))
                    ((#\' #\") (setf quote-character character started t))
                    (t (setf started t)
                       (vector-push-extend character current))))))
      (finish))
    (nreverse arguments)))

(defun parse-command-line (arguments)
  "ARGUMENTS to a plist, or (:error MESSAGE), or (:exit TEXT).

Returns rather than exits, so that the parser is testable without a subprocess."
  (let ((options '()) (index 0) (count (length arguments)))
    (flet ((next (flag)
             (incf index)
             (when (>= index count)
               (return-from parse-command-line
                 (list :error (format nil "~A needs a value" flag))))
             (nth index arguments)))
      (loop while (< index count)
            for argument = (nth index arguments)
            do (cond
                 ((member argument '("-h" "--help") :test #'string=)
                  (return-from parse-command-line (list :exit +usage+)))
                 ((member argument '("-v" "--version") :test #'string=)
                  (return-from parse-command-line
                    (list :exit (format nil "cathode-ray-tube ~A~%" (version)))))
                 ((string= argument "--list-profiles")
                  (return-from parse-command-line
                    (list :exit (format nil "~{~A~%~}"
                                        (mapcar #'crt.settings:profile-name
                                                (crt.settings:all-profiles))))))
                 ((string= argument "--default-settings")
                  (setf (getf options :default-settings) t))
                 ((string= argument "--no-effects")
                  (setf (getf options :effects) nil))
                 ((string= argument "--fullscreen")
                  (setf (getf options :fullscreen) t))
                 ((string= argument "--workdir")
                  (setf (getf options :directory) (next "--workdir")))
                 ((member argument '("-p" "--profile") :test #'string=)
                  (setf (getf options :profile) (next "--profile")))
                 ((string= argument "--geometry")
                  (let* ((text (next "--geometry"))
                         (x (position-if (lambda (c) (find c "xX")) text)))
                    (unless x
                      (return-from parse-command-line
                        (list :error "--geometry wants COLUMNSxROWS, as in 80x25")))
                    (let ((columns (parse-integer text :end x :junk-allowed t))
                          (rows (parse-integer text :start (1+ x) :junk-allowed t)))
                      (unless (and columns rows (plusp columns) (plusp rows))
                        (return-from parse-command-line
                          (list :error "--geometry wants COLUMNSxROWS, as in 80x25")))
                      (setf (getf options :columns) columns
                            (getf options :rows) rows))))
                 ((string= argument "-e")
                  ;; Everything after -e belongs to the child, including things
                  ;; that look like our own flags.  That is what makes
                  ;; `-e ssh host -v' work.
                  (setf (getf options :command) (nthcdr (1+ index) arguments)
                        index count))
                 ((and (plusp (length argument)) (char= (char argument 0) #\-)
                       ;; A lone "-" is a legitimate argument to a child, but a
                       ;; -flag we do not know is a mistake worth reporting.
                       (> (length argument) 1)
                       ;; AppKit passes -psn_0_12345 to a bundle launched from
                       ;; the Finder; it is not ours and not an error.
                       (not (eql 0 (search "-psn_" argument))))
                  (return-from parse-command-line
                    (list :error (format nil "unknown option ~A" argument))))
                 (t nil))
               (incf index)))
    options))

(defun version ()
  (or (asdf:component-version (asdf:find-system :cathode-ray-tube nil)) "0.1.0"))
