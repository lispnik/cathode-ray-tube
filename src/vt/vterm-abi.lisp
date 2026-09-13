;;;; src/vt/vterm-abi.lisp -- the libvterm boundary, minus the C.
;;;;
;;;; What varies between implementations, and nothing else.  The counterparts
;;;; are vterm-abi-sbcl.lisp and vterm-abi-ecl.lisp; this file holds the part
;;;; they share, which is most of it.
;;;;
;;;; libvterm passes VTermRect (four ints) and VTermPos (two ints) BY VALUE, in
;;;; both directions:
;;;;
;;;;   outbound   vterm_screen_get_cell(screen, VTermPos, cell *)
;;;;              vterm_screen_get_text(screen, str, len, VTermRect)
;;;;   inbound    int (*damage)(VTermRect, void *)
;;;;              int (*moverect)(VTermRect, VTermRect, void *)
;;;;              int (*movecursor)(VTermPos, VTermPos, int, void *)
;;;;
;;;; CFFI can express none of them -- foreign-funcall refuses the call and
;;;; defcallback signals CASE-FAILURE -- which is why these went through C.
;;;; sb-alien can do both directions; ECL's dynamic FFI can too, on lispnik/ecl.
;;;;
;;;; THE INBOUND THREE ARE FLATTENED HERE, exactly as the C trampolines did it.
;;;; Each seam builds a function with libvterm's real signature whose only job is
;;;; to take the structure apart and call one of the handlers below with loose
;;;; integers.  So everything above this file -- every callback body in
;;;; libvterm.lisp -- is unchanged and stays portable.

(in-package #:cathode-ray-tube.vt)

;;; The handlers the trampolines call ----------------------------------------
;;;
;;; Variables rather than direct calls, and that is about LOAD ORDER rather than
;;; taste: a seam file is compiled before libvterm.lisp, so it cannot name a
;;; function defined there.  It can read a variable this file declares.

(defvar *damage-handler* nil
  "Called (START-ROW END-ROW START-COL END-COL USER-POINTER) -> int.")

(defvar *moverect-handler* nil
  "Called (DSR DER DSC DEC SSR SER SSC SEC USER-POINTER) -> int.")

(defvar *movecursor-handler* nil
  "Called (ROW COL OLD-ROW OLD-COL VISIBLE USER-POINTER) -> int.")

(defmacro without-unwinding ((what) &body body)
  "Run BODY, answering 0 rather than unwinding.

MANDATORY in a trampoline.  These run inside vterm_input_write, with C frames
between here and any handler, and a condition unwinding through them is
undefined at best -- the same rule objc states for an IMP.  A callback that
fails silently loses a screen update; one that unwinds loses the process."
  `(handler-case (progn ,@body)
     (serious-condition (condition)
       (format *error-output* "~&cathode-ray-tube: vt trampoline ~A: ~A~%"
               ,what condition)
       0)))

(defun call-damage (start-row end-row start-col end-col user)
  (without-unwinding ("damage")
    (if *damage-handler*
        (funcall *damage-handler* start-row end-row start-col end-col user)
        0)))

(defun call-moverect (dsr der dsc dec ssr ser ssc sec user)
  (without-unwinding ("moverect")
    (if *moverect-handler*
        (funcall *moverect-handler* dsr der dsc dec ssr ser ssc sec user)
        0)))

(defun call-movecursor (row col old-row old-col visible user)
  (without-unwinding ("movecursor")
    (if *movecursor-handler*
        (funcall *movecursor-handler* row col old-row old-col visible user)
        0)))

;;; The real VTermScreenCallbacks --------------------------------------------
;;;
;;; Nine function pointers, in libvterm's order.  Six of them take only scalars
;;; and pointers, so they are ordinary cffi:defcallbacks and need no seam; the
;;; three that take structures come from %SCREEN-TRAMPOLINES.

(cffi:defcstruct vterm-screen-callbacks
  (damage :pointer)
  (moverect :pointer)
  (movecursor :pointer)
  (settermprop :pointer)
  (bell :pointer)
  (resize :pointer)
  (sb-pushline :pointer)
  (sb-popline :pointer)
  (sb-clear :pointer))

(defun screen-get-row (screen row cols cells)
  "Fill CELLS with COLS cells of ROW.  Returns how many were filled.

A loop here where the C was a loop there, so the crossing count goes from one
per row to one per CELL.  That is the one part of removing the shim with a cost
attached, and it was measured rather than waved through, because this runs for
every dirty row of every frame:

  80 crossings                    0.68 us   (~8.5 ns per alien-funcall)
  the same row, decoded           5.9  us
  a full 25-row frame             0.148 ms

So the crossings are about a ninth of the row's cost and a thousandth of a 60Hz
frame; DECODE-CELL is what a row costs.  A first attempt at this measurement
said 5.6 ms per frame and was wrong -- it had not warmed the scratch buffer, so
it was timing an allocation per call.  Worth recording because the wrong number
was thirty times the right one and looked plausible."
  (loop for col from 0 below cols
        do (unless (plusp (%screen-get-cell screen row col
                                            (cffi:mem-aptr
                                             cells '(:struct vterm-screen-cell) col)))
             (return col))
        finally (return cols)))
