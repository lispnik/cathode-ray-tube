;;;; src/vt/protocol.lisp -- the seam a terminal core plugs into.
;;;;
;;;; Everything above this package talks to these generic functions and never to
;;;; CFFI.  That is the whole insurance policy: libvterm is vendored and stable,
;;;; but if libghostty-vt tags a release and turns out to be worth having, it is
;;;; a file next to libvterm.lisp rather than a rewrite of the renderer.
;;;;
;;;; It is also what makes the VT suite meaningful: the tests exercise these
;;;; functions, so they are a conformance suite for any backend rather than a
;;;; test of one.

(in-package #:cathode-ray-tube.vt)

(defclass vt ()
  ((rows :initarg :rows :reader vt-rows)
   (cols :initarg :cols :reader vt-cols)
   (title :initform nil :accessor vt-title
          :documentation "What OSC 0/2 last said, for the window title.")
   (bell-count :initform 0 :accessor vt-bell-count)
   (mouse-reporting :initform nil :accessor vt-mouse-reporting-p
    :documentation "True once the child has asked for mouse events.

Which audience a click belongs to -- the child or the user selecting text -- is
the child's decision, and this is where it says so.")
   (alternate-screen :initform nil :accessor vt-alternate-screen-p
    :documentation "True on the alternate screen, where there is no scrollback
because a full-screen program owns the whole display.")
   (output-hook :initform nil :accessor vt-output-hook
                :documentation "Called with an octet vector the terminal wants
written back to the pty -- a device-status reply, a mouse report.  The pty
thread owns the fd, so this queues rather than writes."))
  (:documentation "A terminal screen.  Subclassed once per backend."))

(defgeneric vt-close (vt)
  (:documentation "Release everything the backend holds.  Idempotent."))

(defgeneric vt-open-p (vt)
  (:documentation "True until VT-CLOSE has run.

A closed VT is not a dead object -- callers keep hold of one routinely, because
the child exiting and the last frame that drew it are two different events on
two different threads, and whichever loses the race reads a terminal that has
just been torn down.  So every operation on a closed VT answers as an empty
screen rather than signalling, and this is how a caller that cares can ask.")
  (:method ((vt vt)) t))

(defgeneric vt-write (vt octets &key start end)
  (:documentation "Feed bytes from the pty.  Callers hold the terminal lock."))

(defgeneric vt-resize (vt rows cols)
  (:documentation "Resize the screen.  The caller is responsible for telling the
kernel separately -- see CRT.PTY:SET-WINSIZE -- because the two have different
failure modes and doing them together hides one behind the other."))

(defgeneric vt-reset (vt &key hard))

(defgeneric vt-cell (vt row col)
  (:documentation "The CELL at ROW, COL.  Allocates; VT-ROW-CELLS does not."))

(defgeneric vt-row-cells (vt row cells)
  (:documentation "Fill the simple-vector CELLS with ROW's cells, reusing the
CELL structures already in it.

Reusing them is the point.  The renderer walks every dirty row every frame, and
a fresh CELL per character would allocate tens of thousands of short-lived
structures per second for no reason."))

(defgeneric vt-cursor (vt)
  (:documentation "(values ROW COL VISIBLE-P)."))

(defgeneric vt-dirty-rows (vt)
  (:documentation "A simple-bit-vector of VT-ROWS bits: 1 where the row has
changed since the last VT-CLEAR-DIRTY."))

(defgeneric vt-clear-dirty (vt))
(defgeneric vt-damage-all (vt)
  (:documentation "Mark every row dirty.  After a resize, a profile change, or
anything else that invalidates what the renderer has cached."))

(defgeneric vt-text (vt start-row end-row &key start-col end-col)
  (:documentation "The text of a rectangle, for copying to the pasteboard."))

(defgeneric vt-mouse-move (vt row col modifiers)
  (:documentation "Report the pointer's position to the child, if it asked."))

(defgeneric vt-mouse-button (vt button pressed modifiers)
  (:documentation "Report a button to the child, if it asked.

BUTTON is 1-3 for the three buttons and 4-5 for wheel up and down, which is
what a terminal's mouse protocol calls them."))

(defgeneric vt-start-paste (vt))
(defgeneric vt-end-paste (vt)
  (:documentation "Bracket a paste, so a shell does not execute every newline in
it the moment it arrives and an editor can tell pasted text from typed text.

These emit nothing unless the child turned bracketed paste on, so they are
always safe to call."))

(defgeneric vt-scrollback-length (vt))
(defgeneric vt-scrollback-line (vt n)
  (:documentation "Line N of the scrollback, 0 being the most recent, as a
simple-vector of CELLs."))

(defun vt-dirty-p (vt)
  "True when any row has changed."
  (find 1 (vt-dirty-rows vt)))

;;; The default backend --------------------------------------------------------

(defvar *default-backend* :libvterm
  "Which core MAKE-VT builds.  One keyword, so that switching is a binding.")

(defgeneric make-vt-backend (backend rows cols &key &allow-other-keys))

(defun make-vt (rows cols &rest args &key (backend *default-backend*) &allow-other-keys)
  (apply #'make-vt-backend backend rows cols
         (alexandria:remove-from-plist args :backend)))
