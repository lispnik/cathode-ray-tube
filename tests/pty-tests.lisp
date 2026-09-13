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
