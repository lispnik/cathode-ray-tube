;;;; tests/pty-tests.lisp -- Tier 1: real child processes, no GPU, no window.

(in-package #:cathode-ray-tube/tests)
(in-suite pty)

(defun drain (pty &key (timeout 3.0))
  "Everything the child writes until it stops, as a string."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (with-output-to-string (out)
      (cffi:with-foreign-object (buffer :uint8 4096)
        (loop
          (let ((n (crt.pty:pty-read pty buffer 4096)))
            (cond ((plusp n)
                   (dotimes (i n)
                     (write-char (code-char (cffi:mem-aref buffer :uint8 i)) out)))
                  ;; A pty master reports EIO, not EOF, when the child goes.
                  ((<= n 0) (return)))
            (when (> (get-internal-real-time) deadline) (return))))))))

(test spawn-and-read
  (let ((pty (crt.pty:spawn-pty '("/bin/echo" "hello from the child") 24 80)))
    (unwind-protect
         (is (search "hello from the child" (drain pty)))
      (crt.pty:pty-close pty))))

(test winsize-reaches-the-child
  "The test the whole shim exists for.

`stty size' in the child reports the KERNEL's idea of the window, which is the
only witness that TIOCSWINSZ arrived.  The child blocks on a read first, so the
parent's resize is guaranteed to land before stty runs -- no sleep, no race.

Measured against the same call made with a plain cffi:foreign-funcall: ioctl
returns -1 and the child says \"0 0\".  ioctl is variadic, and on Apple arm64 a
variadic argument goes on the stack rather than in a register."
  (let ((pty (crt.pty:spawn-pty '("/bin/sh" "-c" "read x; stty size") 24 80)))
    (unwind-protect
         (progn
           (is-true (crt.pty:set-winsize (crt.pty:pty-fd pty) 30 100))
           (crt.pty:pty-write pty (babel:string-to-octets (format nil "~%")))
           (let ((text (drain pty)))
             (is (search "30 100" text)
                 "the child saw ~S, not 30 rows by 100 columns"
                 (string-trim '(#\Space #\Newline #\Return) text))))
      (crt.pty:pty-close pty))))

(test winsize-round-trips
  (let ((pty (crt.pty:spawn-pty '("/bin/sh" "-c" "read x") 24 80)))
    (unwind-protect
         (progn
           (multiple-value-bind (rows cols) (crt.pty:get-winsize (crt.pty:pty-fd pty))
             (is (= 24 rows) "forkpty's initial size should be what we asked for")
             (is (= 80 cols)))
           (crt.pty:set-winsize (crt.pty:pty-fd pty) 40 120)
           (multiple-value-bind (rows cols) (crt.pty:get-winsize (crt.pty:pty-fd pty))
             (is (= 40 rows))
             (is (= 120 cols))))
      (crt.pty:pty-close pty))))

(test child-exit-is-reaped
  (let ((pty (crt.pty:spawn-pty '("/bin/sh" "-c" "exit 7") 24 80)))
    (unwind-protect
         (progn
           (drain pty)
           ;; waitpid is non-blocking, and the child may not have been reaped
           ;; by the kernel the instant its output ended.
           (let ((status (loop repeat 100
                               for s = (crt.pty:pty-reap pty)
                               when s return s
                               do (sleep 0.02))))
             (is (eql 7 status) "exit status should be 7, got ~S" status)))
      (crt.pty:pty-close pty))))

(test environment-forces-term
  "TERM is a promise about what we implement, not something to inherit."
  (let ((pty (crt.pty:spawn-pty '("/bin/sh" "-c" "printf %s \"$TERM/$COLORTERM\"") 24 80)))
    (unwind-protect
         (let ((text (drain pty)))
           (is (search "xterm-256color/truecolor" text)
               "child saw ~S" (string-trim '(#\Space #\Newline #\Return) text)))
      (crt.pty:pty-close pty))))

(test environment-inherits-the-rest
  "Everything else comes from C's `environ' -- not SB-EXT:POSIX-ENVIRON, which
does not exist on ECL, where half this program still has to load."
  (let ((entries (crt.pty:child-environment)))
    (is (find "TERM=xterm-256color" entries :test #'string=))
    (is (find-if (lambda (e) (eql 0 (search "PATH=" e))) entries)
        "PATH should have been inherited")
    (is (= 1 (count-if (lambda (e) (eql 0 (search "TERM=" e))) entries))
        "TERM must appear exactly once, not twice")))

(test default-shell-is-usable
  (let ((shell (crt.pty:default-shell)))
    (is (probe-file shell) "~S does not exist" shell)
    (is (equal (list shell "-i" "-l") (crt.pty:login-shell-arguments shell))
        "macOS needs a login shell, or $PATH is missing everything path_helper adds")))

(test working-directory
  (let ((pty (crt.pty:spawn-pty '("/bin/sh" "-c" "pwd") 24 80 :directory #p"/tmp/")))
    (unwind-protect
         (is (search "tmp" (drain pty)))
      (crt.pty:pty-close pty))))

(test the-locale-is-forced-to-utf8
  "LC_CTYPE=UTF-8, as upstream's main.cpp:55 does under Q_OS_MAC.

A GUI application on macOS inherits launchd's environment, not a login shell's,
so LANG and LC_* are routinely absent altogether -- and a child that believes it
is in the C locale will not emit a multi-byte character.  The symptom is a
terminal that cannot type an accent, which looks like a font or an input
problem and is neither.

Forced rather than defaulted, and overriding an inherited value, because that is
what upstream's overwrite flag does."
  (let ((environment (crt.pty:child-environment)))
    (is (member "LC_CTYPE=UTF-8" environment :test #'string=)
        "LC_CTYPE must be forced")
    (is (= 1 (count-if (lambda (entry)
                         (let ((equals (position #\= entry)))
                           (and equals (string= "LC_CTYPE" (subseq entry 0 equals))))) 
                       environment))
        "and must appear exactly once, not shadowing an inherited copy"))
  (is (null (member "LC_CTYPE=UTF-8"
                    (crt.pty:child-environment :lc-ctype nil) :test #'string=))
      "and can be turned off for a caller that knows better"))

(test the-child-really-sees-utf8
  "Through the pty, not just in the list: the environment we build has to be the
environment that arrives."
  (let ((pty (crt.pty:spawn-pty '("/bin/sh" "-c" "printf %s \"$LC_CTYPE\"") 24 80)))
    (unwind-protect
         (let ((text (drain pty)))
           (is (search "UTF-8" text) "the child's LC_CTYPE was ~S"
               (string-trim '(#\Space #\Newline #\Return) text)))
      (crt.pty:pty-close pty))))

(test a-childs-exit-status-is-never-invented
  "PTY-WAIT answers the real status even when asked the instant the child goes.

The race this exists for: the reader loop stops on poll() reporting HUP, HUP
means the child closed the pty rather than that the kernel has finished making a
zombie, and a WNOHANG waitpid in that window says `not yet'.  Reporting that as
0 says the child SUCCEEDED -- so a shell that died with status 3 was reported as
having exited cleanly, intermittently, on whichever machine happened to be slow
enough.

Asked immediately and repeatedly rather than once, because the window is
microseconds wide and a single try lands in it roughly never on an idle
machine.  Twenty runs of a child that exits 3: every one of them must say 3, and
the old code's answer -- 0 -- is a status a child really can exit with, which is
why nothing about it looked wrong."
  (dotimes (i 20)
    (let ((pty (crt.pty:spawn-pty '("/bin/sh" "-c" "exit 3") 24 80)))
      (unwind-protect
           (let ((status (crt.pty:pty-wait pty)))
             (is (eql 3 status) "run ~D reported ~S instead of 3" i status)
             ;; Asking twice must answer twice.  Reaping clears the pid, and a
             ;; guard that led with the pid made every later call answer NIL.
             (is (eql 3 (crt.pty:pty-wait pty)) "and must keep saying so"))
        (crt.pty:pty-close pty)))))

(test a-signalled-child-reports-128-plus-the-signal
  "The shell's convention, and the one anyone reading the number expects.

SIGKILL rather than SIGTERM, because the child here is a session leader with a
controlling terminal -- that is what forkpty is for -- and a shell in that
position CATCHES SIGTERM and exits 0.  Measured: the same `kill -TERM $$' that
reports 143 from an ordinary shell reports 0 through a pty.  Testing that would
be testing what bash does with a signal, which is not this program's business
and not a thing it can fix.  SIGKILL cannot be caught, so what is left is the
decoding, which is what this is about."
  (let ((pty (crt.pty:spawn-pty '("/bin/sh" "-c" "kill -KILL $$") 24 80)))
    (unwind-protect
         (let ((status (crt.pty:pty-wait pty)))
           (is (eql (+ 128 9) status)
               "SIGKILL should report 137, got ~S" status))
      (crt.pty:pty-close pty))))
