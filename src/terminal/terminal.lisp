;;;; src/terminal/terminal.lisp -- a pty and a screen, and the thread between them.
;;;;
;;;; THE THREADING RULE, in one place, because everything else depends on it:
;;;;
;;;;   The MAIN thread owns AppKit, Metal, and every pixel.  It never reads or
;;;;   writes the pty fd.
;;;;
;;;;   The READER thread owns the fd.  It polls, reads, feeds the VT under the
;;;;   lock, and drains the outbound queue.  It never touches Objective-C or
;;;;   Metal.
;;;;
;;;; and there is ONE lock, guarding the VT and the dirty flags.
;;;;
;;;; WHY THE MAIN THREAD MUST NOT WRITE THE FD: a pty master write BLOCKS when
;;;; the child is not reading.  A program that stops reading -- one paused at a
;;;; prompt, one stopped by SIGSTOP, one simply slow -- would hang the UI, and it
;;;; would look like the terminal had crashed rather than like the child had
;;;; stopped.  So input is queued and the reader thread is woken through a pipe.
;;;;
;;;; The renderer does not read the VT directly either.  It takes a SNAPSHOT
;;;; under the lock -- a copy of the dirty rows and the cursor -- and then draws
;;;; from that with the lock released.  Holding a lock across a GPU submission
;;;; would make the reader thread wait on the display.

(in-package #:cathode-ray-tube.terminal)

;;; poll(2) ---------------------------------------------------------------------

(cffi:defcstruct pollfd
  (fd :int) (events :short) (revents :short))

(cffi:defcfun ("poll" %poll) :int
  (fds :pointer) (nfds :unsigned-long) (timeout :int))
(cffi:defcfun ("pipe" %pipe) :int (fds :pointer))
(cffi:defcfun ("close" %close) :int (fd :int))
(cffi:defcfun ("read" %read) :long (fd :int) (buf :pointer) (count :unsigned-long))
(cffi:defcfun ("write" %write) :long (fd :int) (buf :pointer) (count :unsigned-long))

(defconstant +pollin+  #x0001)
(defconstant +pollerr+ #x0008)
(defconstant +pollhup+ #x0010)
(defconstant +pollnval+ #x0020)

;;; The snapshot ----------------------------------------------------------------

(defstruct snapshot
  "What the renderer is allowed to look at.

Taken under the lock and read outside it.  CELLS is a vector of row vectors,
reused between frames -- the whole point of VT-ROW-CELLS filling structures in
place is that this does not allocate once it has warmed up."
  (rows 0 :type fixnum)
  (cols 0 :type fixnum)
  (cells #() :type simple-vector)
  (dirty #* :type simple-bit-vector)
  (cursor-row 0 :type fixnum)
  (cursor-col 0 :type fixnum)
  (cursor-visible t)
  ;; PAINTED is true when the screen changed since the last snapshot.  It is
  ;; cool-retro-term's `imagePainted' signal, and it decides whether the burn-in
  ;; accumulator advances this frame.  Burn-in is event-driven, not per-frame:
  ;; advancing it on a frame where nothing changed would decay the phosphor at
  ;; the display's refresh rate rather than at the terminal's.
  ;;
  ;; (A comment and not a :documentation string -- DEFSTRUCT slot options are
  ;; :TYPE and :READ-ONLY, and nothing else.)
  (painted nil))

;;; The terminal ----------------------------------------------------------------

(defclass terminal ()
  ((vt :reader terminal-vt)
   (pty :reader terminal-pty)
   (lock :reader terminal-lock :initform (bt2:make-lock :name "terminal"))
   (painted :initform nil :accessor terminal-painted)
   (reader :initform nil :accessor terminal-reader)
   (running :initform t :accessor terminal-running)
   (outbound :initform '() :accessor terminal-outbound)
   (wake-read :initform -1 :accessor terminal-wake-read)
   (wake-write :initform -1 :accessor terminal-wake-write)
   (snapshot :initform nil :accessor terminal-cached-snapshot)
   (exit-status :initform nil :accessor terminal-exit-status)
   (on-exit :initarg :on-exit :initform nil :accessor terminal-on-exit)
   (on-title :initarg :on-title :initform nil :accessor terminal-on-title)))

(defmacro with-terminal-locked ((terminal) &body body)
  `(bt2:with-lock-held ((terminal-lock ,terminal)) ,@body))

(defun terminal-rows (terminal) (vt:vt-rows (terminal-vt terminal)))
(defun terminal-cols (terminal) (vt:vt-cols (terminal-vt terminal)))
(defun terminal-title (terminal) (vt:vt-title (terminal-vt terminal)))
(defun terminal-alive-p (terminal)
  (and (terminal-running terminal) (null (terminal-exit-status terminal))))

(defun make-terminal (&key (rows 24) (cols 80) command directory on-exit on-title)
  "Start COMMAND on a pty and attach a terminal screen to it.

COMMAND defaults to the user's login shell."
  (let ((terminal (make-instance 'terminal :on-exit on-exit :on-title on-title))
        (vt (vt:make-vt rows cols)))
    (setf (slot-value terminal 'vt) vt)
    ;; Replies the terminal wants to send -- a cursor-position report, a mouse
    ;; event -- are QUEUED here rather than written.  This hook runs inside
    ;; vterm_input_write, on the reader thread, with the lock held and the fd
    ;; possibly not writable.
    (setf (vt:vt-output-hook vt)
          (lambda (octets) (push octets (terminal-outbound terminal))))
    (setf (slot-value terminal 'pty)
          (pty:spawn-pty (pty:shell-command :command command) rows cols
                         :directory directory))
    (cffi:with-foreign-object (fds :int 2)
      (if (zerop (%pipe fds))
          (setf (terminal-wake-read terminal) (cffi:mem-aref fds :int 0)
                (terminal-wake-write terminal) (cffi:mem-aref fds :int 1))
          (error "could not make the wake-up pipe")))
    (setf (terminal-reader terminal)
          (bt2:make-thread (lambda () (reader-loop terminal))
                           :name "cathode-ray-tube pty reader"))
    terminal))

;;; The reader thread ------------------------------------------------------------

(defun reader-loop (terminal)
  "Poll the pty and the wake pipe until the child goes or we are asked to stop."
  (let ((pty (terminal-pty terminal)))
    (unwind-protect
         (cffi:with-foreign-objects ((fds '(:struct pollfd) 2)
                                     (buffer :uint8 65536))
           (loop while (terminal-running terminal) do
             (let ((master (pty:pty-fd pty))
                   (wake (terminal-wake-read terminal)))
               (when (minusp master) (return))
               (flet ((set-fd (index fd)
                        (let ((p (cffi:mem-aptr fds '(:struct pollfd) index)))
                          (setf (cffi:foreign-slot-value p '(:struct pollfd) 'fd) fd
                                (cffi:foreign-slot-value p '(:struct pollfd) 'events)
                                +pollin+
                                (cffi:foreign-slot-value p '(:struct pollfd) 'revents)
                                0))))
                 (set-fd 0 master)
                 (set-fd 1 wake))
               ;; A timeout rather than an indefinite wait, so that a terminal
               ;; asked to stop between polls does not sit here until the child
               ;; happens to say something.
               (let ((ready (%poll fds 2 250)))
                 (when (minusp ready) (return))
                 (let ((master-events (cffi:foreign-slot-value
                                       (cffi:mem-aptr fds '(:struct pollfd) 0)
                                       '(:struct pollfd) 'revents))
                       (wake-events (cffi:foreign-slot-value
                                     (cffi:mem-aptr fds '(:struct pollfd) 1)
                                     '(:struct pollfd) 'revents)))
                   (when (logtest wake-events +pollin+)
                     ;; Drain the pipe; its only content is "look at the queue".
                     (%read wake buffer 4096))
                   (when (logtest master-events +pollin+)
                     (unless (drain-pty terminal buffer 65536) (return)))
                   ;; HUP or ERR means the child has gone.  Read once more
                   ;; first: the last output and the hangup arrive together, and
                   ;; returning here would lose it.
                   (when (logtest master-events (logior +pollhup+ +pollerr+ +pollnval+))
                     (drain-pty terminal buffer 65536)
                     (return))))
               (flush-outbound terminal))))
      (finish-terminal terminal))))

(defun drain-pty (terminal buffer size)
  "Read what is available and feed it to the screen.  NIL when the child is gone."
  (let ((n (pty:pty-read (terminal-pty terminal) buffer size)))
    (cond
      ((plusp n)
       (let ((octets (make-array n :element-type '(unsigned-byte 8))))
         (dotimes (i n) (setf (aref octets i) (cffi:mem-aref buffer :uint8 i)))
         (with-terminal-locked (terminal)
           (vt:vt-write (terminal-vt terminal) octets)
           ;; The flag the burn-in pass reads.  Set here, cleared when the
           ;; renderer takes a snapshot.
           (setf (terminal-painted terminal) t)))
       t)
      ;; 0 is end of file; a pty master usually reports EIO instead, which comes
      ;; back negative.  Both mean the child has gone.
      (t nil))))

(defun flush-outbound (terminal)
  "Write anything the terminal wanted to say back to the child."
  (let ((pending (with-terminal-locked (terminal)
                   (prog1 (nreverse (terminal-outbound terminal))
                     (setf (terminal-outbound terminal) nil)))))
    (dolist (octets pending)
      (pty:pty-write (terminal-pty terminal) octets))))

(defun finish-terminal (terminal)
  (setf (terminal-running terminal) nil)
  (let ((status (or (pty:pty-reap (terminal-pty terminal)) 0)))
    (setf (terminal-exit-status terminal) status)
    (let ((hook (terminal-on-exit terminal)))
      (when hook
        ;; The hook reaches AppKit, and this is not the main thread.  It is the
        ;; hook's business to get there -- see CRT.UI's main-thread queue --
        ;; but it must not signal here, where there is nobody to catch it.
        (handler-case (funcall hook terminal status)
          (error (condition)
            (format *error-output* "~&cathode-ray-tube: on-exit hook: ~A~%"
                    condition)))))))

;;; Input -------------------------------------------------------------------------

(defun wake-reader (terminal)
  (let ((fd (terminal-wake-write terminal)))
    (when (>= fd 0)
      (cffi:with-foreign-object (byte :uint8)
        (setf (cffi:mem-ref byte :uint8) 1)
        (%write fd byte 1)))))

(defun terminal-send (terminal octets)
  "Queue OCTETS for the child and wake the reader thread to write them.

Never writes the fd from here: see the header."
  (with-terminal-locked (terminal)
    (push octets (terminal-outbound terminal)))
  (wake-reader terminal))

(defun terminal-send-string (terminal string)
  (terminal-send terminal (babel:string-to-octets string :encoding :utf-8)))

;;; Resizing ------------------------------------------------------------------------

(defun terminal-resize (terminal rows cols)
  "Resize the screen and tell the kernel, in that order.

Two separate operations with different failure modes, deliberately not folded
together: the screen resize is ours and cannot fail, while TIOCSWINSZ is the
kernel's and can.  Doing them together hides the second behind the first."
  (when (and (plusp rows) (plusp cols)
             (or (/= rows (terminal-rows terminal))
                 (/= cols (terminal-cols terminal))))
    (with-terminal-locked (terminal)
      (vt:vt-resize (terminal-vt terminal) rows cols)
      (setf (terminal-painted terminal) t
            (terminal-cached-snapshot terminal) nil))
    (pty:set-winsize (pty:pty-fd (terminal-pty terminal)) rows cols))
  terminal)

;;; Snapshots -------------------------------------------------------------------------

(defun ensure-snapshot (terminal)
  (let ((snapshot (terminal-cached-snapshot terminal))
        (rows (terminal-rows terminal))
        (cols (terminal-cols terminal)))
    (if (and snapshot (= rows (snapshot-rows snapshot))
             (= cols (snapshot-cols snapshot)))
        snapshot
        (setf (terminal-cached-snapshot terminal)
              (make-snapshot
               :rows rows :cols cols
               :cells (let ((grid (make-array rows)))
                        (dotimes (r rows grid)
                          (setf (aref grid r)
                                (let ((row (make-array cols)))
                                  (dotimes (c cols row)
                                    (setf (aref row c) (vt:make-cell)))))))
               :dirty (make-array rows :element-type 'bit :initial-element 1))))))

(defun terminal-snapshot (terminal)
  "A copy of what the renderer needs, taken under the lock.

Only DIRTY rows are copied out, which is what makes this cheap enough to do
every frame: a terminal at rest copies nothing."
  (with-terminal-locked (terminal)
    (let* ((snapshot (ensure-snapshot terminal))
           (vt (terminal-vt terminal))
           (dirty (vt:vt-dirty-rows vt))
           (cells (snapshot-cells snapshot)))
      (dotimes (row (snapshot-rows snapshot))
        (when (= 1 (sbit dirty row))
          (vt:vt-row-cells vt row (aref cells row))))
      (replace (snapshot-dirty snapshot) dirty)
      (vt:vt-clear-dirty vt)
      (multiple-value-bind (crow ccol visible) (vt:vt-cursor vt)
        (setf (snapshot-cursor-row snapshot) crow
              (snapshot-cursor-col snapshot) ccol
              (snapshot-cursor-visible snapshot) visible))
      (setf (snapshot-painted snapshot) (shiftf (terminal-painted terminal) nil))
      snapshot)))

(defun terminal-take-dirty (terminal)
  "True when anything has changed since the last snapshot."
  (with-terminal-locked (terminal)
    (vt:vt-dirty-p (terminal-vt terminal))))

;;; Teardown ---------------------------------------------------------------------------

(defun terminal-close (terminal)
  (setf (terminal-running terminal) nil)
  (wake-reader terminal)
  (let ((thread (terminal-reader terminal)))
    (when (and thread (bt2:thread-alive-p thread))
      ;; Bounded: the reader polls with a 250ms timeout, so it notices within
      ;; that.  A join with no bound would make a wedged reader wedge the quit.
      (handler-case (bt2:join-thread thread :timeout 2)
        (error () nil))))
  (pty:pty-close (terminal-pty terminal))
  (vt:vt-close (terminal-vt terminal))
  (dolist (fd (list (terminal-wake-read terminal) (terminal-wake-write terminal)))
    (when (>= fd 0) (%close fd)))
  (setf (terminal-wake-read terminal) -1
        (terminal-wake-write terminal) -1)
  terminal)
