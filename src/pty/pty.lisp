;;;; src/pty/pty.lisp -- a child process on a pseudo-terminal.
;;;;
;;;; forkpty does the hard part: allocate a pty pair, fork, and in the child
;;;; make the slave the controlling terminal -- setsid, TIOCSCTTY, and dup2 onto
;;;; 0, 1 and 2.  What is left is getting the FORK right.
;;;;
;;;; NO LISP ALLOCATION BETWEEN FORK AND EXEC.  Only the forking thread survives
;;;; into the child, and any lock another thread was holding at that instant --
;;;; the allocator's included -- is held forever by a thread that no longer
;;;; exists.  So argv and envp are built in the PARENT, and the child does
;;;; nothing but execve and _exit.
;;;;
;;;; Adapted from revision-term/src/pty.lisp, which solved all of this already,
;;;; with two changes.  The window size goes through the shim rather than
;;;; through a hand-built variadic libffi call, which is why this file needs
;;;; neither cffi-libffi nor a C toolchain at Lisp build time.  And the child's
;;;; environment is read from C's `environ' rather than from SB-EXT:POSIX-ENVIRON,
;;;; so this file stays loadable on ECL.

(in-package #:cathode-ray-tube.pty)

;;; forkpty is in libSystem on macOS and libutil elsewhere; libSystem is already
;;; open in any process, so nothing has to be loaded on the platform we target.
(cffi:defcfun ("forkpty" %forkpty) :int
  (amaster :pointer) (name :pointer) (termios :pointer) (winsize :pointer))

(cffi:defcfun ("execve" %execve) :int
  (path :pointer) (argv :pointer) (envp :pointer))
(cffi:defcfun ("_exit" %_exit) :void (status :int))
(cffi:defcfun ("read" %read) :long (fd :int) (buf :pointer) (count :unsigned-long))
(cffi:defcfun ("write" %write) :long (fd :int) (buf :pointer) (count :unsigned-long))
(cffi:defcfun ("close" %close) :int (fd :int))
(cffi:defcfun ("kill" %kill) :int (pid :int) (signal :int))
(cffi:defcfun ("waitpid" %waitpid) :int (pid :int) (status :pointer) (options :int))
(cffi:defcfun ("chdir" %chdir) :int (path :pointer))
(cffi:defcfun ("pipe" %pipe) :int (fds :pointer))
(cffi:defcfun ("fcntl" %fcntl) :int (fd :int) (cmd :int) (arg :int))

;;; The window size lives in ioctl.lisp: ioctl is variadic, which CFFI cannot
;;; express, so the call itself is the one thing that varies by implementation.

(defconstant +sighup+ 1)
(defconstant +sigterm+ 15)
(defconstant +sigkill+ 9)
(defconstant +wnohang+ 1)
(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)
(defconstant +o-nonblock+ #x0004)   ; Darwin

;;; Building argv and envp, in the parent -------------------------------------

(defun %foreign-string-array (strings)
  "A NULL-terminated C array of freshly allocated C strings.

Built in the PARENT so the child can execve without allocating."
  (let* ((n (length strings))
         (array (cffi:foreign-alloc :pointer :count (1+ n))))
    (loop for s in strings for i from 0
          do (setf (cffi:mem-aref array :pointer i) (cffi:foreign-string-alloc s)))
    (setf (cffi:mem-aref array :pointer n) (cffi:null-pointer))
    array))

(defun %free-string-array (array)
  (unless (or (null array) (cffi:null-pointer-p array))
    (loop for i from 0
          for p = (cffi:mem-aref array :pointer i)
          until (cffi:null-pointer-p p)
          do (cffi:foreign-string-free p))
    (cffi:foreign-free array)))

(defun c-environment ()
  "The process environment, read from C's `environ'.

Not SB-EXT:POSIX-ENVIRON: this file is in the half of the program that has to
stay loadable on ECL, where that symbol does not exist.  `environ' is a plain
global in libSystem and reading it is portable across both."
  (let ((environ (cffi:foreign-symbol-pointer "environ")))
    (when environ
      (let ((table (cffi:mem-ref environ :pointer)))
        (unless (cffi:null-pointer-p table)
          (loop for i from 0
                for entry = (cffi:mem-aref table :pointer i)
                until (cffi:null-pointer-p entry)
                collect (cffi:foreign-string-to-lisp entry)))))))

(defun child-environment (&key (term "xterm-256color") (colorterm "truecolor")
                              (lc-ctype "UTF-8") extra)
  "The environment the child should see.

TERM is forced, because it is a promise about what WE implement rather than
something to inherit from whatever launched us -- and the terminal the child
talks to is libvterm, which is an xterm.

LC_CTYPE is forced for the same kind of reason and is upstream's, verbatim:
main.cpp:55 is `setenv(\"LC_CTYPE\", \"UTF-8\", 1)' under Q_OS_MAC, with the
overwrite flag set, and its comment says it is what allows UTF-8 characters to
be used at all.  A GUI application on macOS inherits launchd's environment rather
than a login shell's, so LANG and LC_* are routinely absent entirely -- and a
child that believes it is in the C locale will not emit a multi-byte character,
which shows up as a terminal that cannot type an accent rather than as anything
to do with locales."
  (let* ((forced (append (when term (list (cons "TERM" term)))
                         (when colorterm (list (cons "COLORTERM" colorterm)))
                         (when lc-ctype (list (cons "LC_CTYPE" lc-ctype)))
                         extra))
         (names (mapcar #'car forced)))
    (append (mapcar (lambda (pair) (format nil "~A=~A" (car pair) (cdr pair))) forced)
            (remove-if (lambda (entry)
                         (let ((equals (position #\= entry)))
                           (and equals (member (subseq entry 0 equals) names
                                               :test #'string=))))
                       (c-environment)))))

;;; The pty --------------------------------------------------------------------

(defstruct (pty (:constructor %make-pty (fd pid argv envp)))
  (fd -1 :type fixnum)
  (pid -1 :type fixnum)
  argv envp
  (exit-status nil)
  ;; Guards the PID and the EXIT-STATUS, which two threads reach at once as a
  ;; matter of course: the reader thread reaps the child when the loop ends, and
  ;; the main thread calls PTY-CLOSE, and TERMINAL-CLOSE's join is BOUNDED -- so
  ;; the two genuinely overlap whenever the reaping takes longer than the join
  ;; will wait.
  ;;
  ;; What the lock prevents is not a torn read but a SIGNAL SENT TO A PID THAT IS
  ;; NO LONGER OURS.  PTY-CLOSE sends SIGHUP and then SIGKILL, and between the
  ;; other thread's waitpid and this thread's kill the pid belongs to the
  ;; operating system again.  Reusing it takes a busy machine and some bad luck,
  ;; and the failure -- an unrelated process dying -- would never be traced back
  ;; here.
  (lock (bt2:make-lock :name "crt pty") :read-only t))

(defmacro with-pty-locked ((pty) &body body)
  `(bt2:with-lock-held ((pty-lock ,pty)) ,@body))

(defun set-nonblocking (fd)
  (let ((flags (%fcntl fd +f-getfl+ 0)))
    (when (>= flags 0)
      (zerop (%fcntl fd +f-setfl+ (logior flags +o-nonblock+))))))

(defun spawn-pty (command rows cols &key directory environment)
  "Run COMMAND on a fresh pty sized ROWS by COLS.

COMMAND is a list of strings whose first element is an absolute program path.
Returns a PTY, or signals.  The child execve's and never returns to Lisp."
  (let* ((argv (%foreign-string-array command))
         (envp (%foreign-string-array (or environment (child-environment))))
         (path (first command))
         ;; Allocated before the fork, because the child must not.
         (c-path (cffi:foreign-string-alloc path))
         (c-directory (when directory
                        (cffi:foreign-string-alloc (namestring directory)))))
    (cffi:with-foreign-objects ((amaster :int) (ws :unsigned-short 4))
      (setf (cffi:mem-aref ws :unsigned-short 0) rows
            (cffi:mem-aref ws :unsigned-short 1) cols
            (cffi:mem-aref ws :unsigned-short 2) 0
            (cffi:mem-aref ws :unsigned-short 3) 0)
      (let ((pid (%forkpty amaster (cffi:null-pointer) (cffi:null-pointer) ws)))
        (cond
          ((zerop pid)
           ;; --- the child.  Nothing here may allocate, cons, or signal. ---
           (when c-directory (%chdir c-directory))
           (%execve c-path argv envp)
           (%_exit 127))
          ((minusp pid)
           (cffi:foreign-string-free c-path)
           (when c-directory (cffi:foreign-string-free c-directory))
           (%free-string-array argv)
           (%free-string-array envp)
           (error "forkpty failed for ~S." command))
          (t
           (cffi:foreign-string-free c-path)
           (when c-directory (cffi:foreign-string-free c-directory))
           (let ((fd (cffi:mem-ref amaster :int)))
             (%make-pty fd pid argv envp))))))))

(defun pty-alive-p (pty)
  (and pty (> (pty-pid pty) 0) (null (pty-exit-status pty))))

(cffi:defcfun ("crt_errno" %errno) :int)
(cffi:defcfun ("crt_eintr" %eintr) :int)
(cffi:defcfun ("crt_eagain" %eagain) :int)
(cffi:defcfun ("crt_eio" %eio) :int)

(defun interrupted-p ()
  "True when the call that just failed was INTERRUPTED rather than broken.

EINTR, or EAGAIN on a descriptor someone has made non-blocking.  Neither says
anything about the child, and both are routine: SBCL's collector stops the world
by signalling every other thread, and a thread sitting in a blocking poll() or
read() comes back -1/EINTR when that happens.

A reader loop that treated a negative return as `the child is gone' therefore
ended whenever a collection landed on it, leaving a terminal that had died with
its child still running -- pid valid, master open, reader thread alive.  It
depends on when the collector runs, so it never happened here and happened
regularly on a CI runner."
  (let ((e (%errno)))
    (or (= e (%eintr)) (= e (%eagain)))))

(defun pty-read (pty buffer count)
  "read(2) up to COUNT bytes into the foreign BUFFER.

Returns the count, 0 at end of file, :AGAIN when the call was interrupted, or a
negative number on a real error.  A pty master reports EIO rather than EOF when
the child goes, which is normal and not a problem.

:AGAIN rather than a negative number, because the whole bug this distinguishes
was one caller reading `negative' as `finished'.  A keyword cannot be compared
with ZEROP or MINUSP by accident."
  (let ((n (%read (pty-fd pty) buffer count)))
    (if (and (minusp n) (interrupted-p)) :again n)))

(defun pty-write (pty octets &key (start 0) end)
  "Write OCTETS to the child.  Returns the number of bytes actually written.

LOOPS until everything is gone, and that is the whole point of this function
rather than an optimisation.  write(2) is permitted to write FEWER bytes than it
was given and return that count, and it returns -1/EINTR when a signal arrives
mid-call -- and SBCL's collector signals every thread it stops.  A version that
called write once and returned what it got DROPPED the remainder silently.

The symptom was as good as invisible and as bad as it sounds: type a line, and
sometimes the newline at the end of it is the byte that goes missing.  The child
sits waiting for the rest of a line that will never arrive, the echo on screen
shows exactly what was typed because the TTY echoed the part that did get
through, and the terminal looks like it has hung.  Caught by a test that forced
collections while typing; the screen read `second' with no newline after it.

Only the thread that owns the fd should call this: a pty master write BLOCKS
when the child is not reading, so calling it from the UI thread is a hang
waiting for a slow program."
  (let* ((end (or end (length octets)))
         (count (- end start)))
    (if (or (not (plusp count)) (minusp (pty-fd pty)))
        0
        (cffi:with-foreign-object (buffer :uint8 count)
          (loop for i from 0 below count
                do (setf (cffi:mem-aref buffer :uint8 i) (aref octets (+ start i))))
          (loop with written = 0
                while (< written count)
                do (let ((n (%write (pty-fd pty)
                                    (cffi:inc-pointer buffer written)
                                    (- count written))))
                     (cond ((plusp n) (incf written n))
                           ;; Interrupted, or the child is not reading and the
                           ;; descriptor is non-blocking: neither has lost
                           ;; anything, so try the rest again.
                           ((and (minusp n) (interrupted-p)))
                           ;; A real error, or a zero-byte write that is going
                           ;; nowhere.  Stop, and report what did get through.
                           (t (return written))))
                finally (return written))))))

(defun pty-reap (pty)
  "Non-blocking waitpid.  Returns the exit status if the child has gone.

128 + signal when it was killed, which is the shell's convention and the one
anybody reading the number will expect."
  ;; The cached status FIRST, and the live pid second.  Reaping sets the pid to
  ;; -1, so a guard that starts by requiring a live pid answers NIL for every
  ;; call after the one that succeeded -- which makes asking twice mean
  ;; something different from asking once, for no reason a caller could guess.
  (when pty
    (with-pty-locked (pty)
      (or (pty-exit-status pty)
        (and (> (pty-pid pty) 0)
             (cffi:with-foreign-object (status :int)
          (let ((result (%waitpid (pty-pid pty) status +wnohang+)))
            (when (> result 0)
              (let* ((raw (cffi:mem-ref status :int))
                     (code (if (zerop (logand raw #x7f))
                               (logand (ash raw -8) #xff)
                               (+ 128 (logand raw #x7f)))))
                (setf (pty-pid pty) -1
                      (pty-exit-status pty) code))))))))))

(defun pty-wait (pty &key (timeout 2.0))
  "Reap the child, waiting for it.  The status, or NIL if it has not gone.

PTY-REAP asks whether the child has already become a zombie and answers NIL when
it has not.  That is the right answer to that question and the WRONG one to ask
at the end of the reader loop: the loop stops the instant poll() reports HUP on
the master, and HUP means the child has closed its side of the pty, not that the
kernel has finished turning it into something waitpid can collect.  In that
window WNOHANG returns 0 and the status is simply not available yet.

Reporting that as 0 -- which is what `(or (pty-reap pty) 0)' did -- says the
child EXITED CLEANLY.  So a shell that died with status 3 was reported as having
succeeded, on a machine slow enough to open the window.

BOUNDED, and never a blocking waitpid.  The first version of this ended with one
and it deadlocked: waitpid with no WNOHANG waits for a child that only PTY-CLOSE
will signal, PTY-CLOSE first has to claim the pid under the lock, and the lock
was being held across the wait.  The suite hung on CI.  A child that has really
gone is collectable within microseconds, so the loop below finds it immediately
and the timeout is only ever reached by a child that is still running -- which is
not a case that wants waiting for at all."
  (when pty
    (or (pty-exit-status pty)
        (pty-reap pty)
        (let ((deadline (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))))
          (loop (let ((status (pty-reap pty)))
                  (when status (return status))
                  (when (> (get-internal-real-time) deadline) (return nil))
                  (sleep 0.002)))))))

(defun pty-close (pty)
  "Close the fd, hang the child up, reap it, and free argv and envp.

SIGHUP then SIGKILL: a shell that has been hung up exits, and one that has not
noticed within a moment is not going to."
  (when pty
    ;; CLAIM the pid under the lock and set it to -1 in the same breath, so that
    ;; whichever thread gets here first is the only one that will ever signal or
    ;; wait on it.  Reading it and clearing it separately is the race this exists
    ;; to close.
    (let ((fd (pty-fd pty))
          (pid (with-pty-locked (pty)
                 (prog1 (pty-pid pty) (setf (pty-pid pty) -1)))))
      (when (>= fd 0)
        (%close fd)
        (setf (pty-fd pty) -1))
      (when (> pid 0)
        (%kill pid +sighup+)
        (cffi:with-foreign-object (status :int)
          (let ((reaped nil))
            (loop repeat 20
                  until (setf reaped (> (%waitpid pid status +wnohang+) 0))
                  do (sleep 0.01))
            (unless reaped
              (%kill pid +sigkill+)
              (%waitpid pid status 0))))))
    (%free-string-array (pty-argv pty))
    (%free-string-array (pty-envp pty))
    (setf (pty-argv pty) nil (pty-envp pty) nil))
  pty)
