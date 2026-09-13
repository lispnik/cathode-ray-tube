;;;; src/vt/libvterm.lisp -- the libvterm backend.

(in-package #:cathode-ray-tube.vt)

(defparameter *scrollback-limit* 10000
  "How many lines of history to keep.  libvterm keeps none of its own -- it
hands each scrolled-off line to sb_pushline and forgets it -- so the limit is
entirely ours.")

(defclass libvterm-vt (vt)
  ((handle :initform nil :accessor vt-handle)
   (screen :initform nil :accessor vt-screen)
   (state  :initform nil :accessor vt-state)
   (callbacks :initform nil :accessor vt-callbacks
              :documentation "The VTermScreenCallbacks table, malloc'd.

libvterm keeps the POINTER and reads through it on every write, so this outlives
the call that installed it and is freed after vterm_free -- not before, because
libvterm calls back during teardown and would read freed memory.")
   (id :initform nil :accessor vt-id
       :documentation "Our key in *TERMINALS*, passed to C as the user datum.
An integer rather than a pointer to a Lisp object: the collector moves Lisp
objects and C would be holding a stale address.")
   (dirty :initform nil :accessor vt-dirty)
   (cursor-row :initform 0 :accessor vt-cursor-row)
   (cursor-col :initform 0 :accessor vt-cursor-col)
   (cursor-visible :initform t :accessor vt-cursor-visible)
   (scrollback :initform nil :accessor vt-scrollback)
   (scratch :initform nil :accessor vt-scratch
            :documentation "One foreign row of VTermScreenCells, reused.")
   (scratch-cols :initform 0 :accessor vt-scratch-cols)
   (title-buffer :initform nil :accessor vt-title-buffer
                 :documentation "Open while an OSC title is arriving in pieces.")))

;;; The registry ---------------------------------------------------------------
;;;
;;; Every libvterm callback carries a `void *user', so a STATIC cffi:defcallback
;;; plus a table keyed on that pointer covers what would otherwise need a
;;; closure per terminal.  The key is a small integer cast to a pointer, not a
;;; pointer to a Lisp object: the collector moves Lisp objects and C would be
;;; holding an address that used to mean something.

(defvar *terminals* (make-hash-table)
  "ID -> LIBVTERM-VT.")
(defvar *next-id* 0)

(defun register-terminal (vt)
  (let ((id (incf *next-id*)))
    (setf (gethash id *terminals*) vt
          (vt-id vt) id)
    id))

(defun unregister-terminal (vt)
  (when (vt-id vt)
    (remhash (vt-id vt) *terminals*)
    (setf (vt-id vt) nil)))

(defun terminal-for (user)
  (gethash (cffi:pointer-address user) *terminals*))

(defmacro define-vt-callback (name return (&rest args) &body body)
  "A libvterm callback that finds its terminal and cannot signal.

Cannot signal is the operative half.  These run inside vterm_input_write, called
from the pty thread, with C frames between here and any handler -- a condition
unwinding through them is undefined at best.  A callback that fails silently
loses a screen update; one that unwinds loses the process."
  (let ((user (car (last args))))
    ;; The body's leading declarations are hoisted to where they are legal.
    ;; Splicing the whole body into a PROGN would put a DECLARE in an
    ;; evaluated position, which SBCL rejects with a message about undefined
    ;; behaviour that does not mention macros at all.
    (multiple-value-bind (forms declarations) (alexandria:parse-body body)
      `(cffi:defcallback ,name ,return ,args
         ,@declarations
         (handler-case
             (let ((vt (terminal-for ,(first user))))
               (if vt (progn ,@forms) 0))
           (error (condition)
             (format *error-output* "~&cathode-ray-tube: vt callback ~A: ~A~%"
                     ',name condition)
             0))))))

(defmacro define-vt-handler (name (&rest args) &body body)
  "A libvterm callback whose C signature passes a structure BY VALUE.

Identical in shape to DEFINE-VT-CALLBACK and different in one respect: this
produces an ordinary Lisp FUNCTION rather than a cffi:defcallback, because CFFI
cannot receive a structure by value at all.  The seam -- vterm-abi-sbcl.lisp or
vterm-abi-ecl.lisp -- builds the real C entry point and calls this with the
structure already taken apart, which is exactly what the C trampolines in
vendor/shim did before them.

So the body is the same code it always was, and stays portable."
  (let ((user (car (last args))))
    (multiple-value-bind (forms declarations) (alexandria:parse-body body)
      `(defun ,name ,args
         ,@declarations
         (let ((vt (terminal-for ,user)))
           (if vt (progn ,@forms) 0))))))

(define-vt-handler vt-damage (start-row end-row start-col end-col user)
  (declare (ignore start-col end-col))
  ;; Rows, not cells: the renderer uploads whole dirty rows, so finer damage
  ;; would be discarded.  vterm_screen_set_damage_merge is told the same thing.
  (let ((dirty (vt-dirty vt)))
    (loop for row from (max 0 start-row) below (min end-row (length dirty))
          do (setf (sbit dirty row) 1)))
  1)

(define-vt-handler vt-moverect (dsr der dsc dec ssr ser ssc sec user)
  (declare (ignore dsc dec ssc sec))
  (let ((dirty (vt-dirty vt)))
    (loop for row from (max 0 (min dsr ssr)) below (min (max der ser) (length dirty))
          do (setf (sbit dirty row) 1)))
  1)

(define-vt-handler vt-movecursor (row col old-row old-col visible user)
  (let ((dirty (vt-dirty vt)))
    ;; Both rows: the cursor is drawn over a cell, so the one it left has to be
    ;; repainted as well as the one it arrived at.
    (dolist (r (list old-row row))
      (when (< -1 r (length dirty)) (setf (sbit dirty r) 1))))
  (setf (vt-cursor-row vt) row
        (vt-cursor-col vt) col
        (vt-cursor-visible vt) (plusp visible))
  1)

(define-vt-callback vt-bell :int ((user :pointer))
  (incf (vt-bell-count vt))
  1)

(define-vt-callback vt-resize-cb :int ((rows :int) (cols :int) (user :pointer))
  (setf (slot-value vt 'rows) rows
        (slot-value vt 'cols) cols)
  (reset-dirty vt)
  1)

(define-vt-callback vt-sb-pushline :int
    ((cols :int) (cells :pointer) (user :pointer))
  (let ((line (make-array cols)))
    (dotimes (i cols)
      (setf (aref line i)
            (decode-cell (cffi:mem-aptr cells '(:struct vterm-screen-cell) i))))
    (let ((scrollback (vt-scrollback vt)))
      (vector-push-extend line scrollback)
      ;; Trim from the front when it grows past the limit.  Done in bulk rather
      ;; than one line at a time so that a long build log does not spend its
      ;; time shuffling a vector.
      (when (> (fill-pointer scrollback) (* 2 *scrollback-limit*))
        (replace scrollback scrollback :start2 (- (fill-pointer scrollback)
                                                  *scrollback-limit*))
        (setf (fill-pointer scrollback) *scrollback-limit*))))
  1)

(define-vt-callback vt-sb-popline :int
    ((cols :int) (cells :pointer) (user :pointer))
  ;; Scrolling back down: hand libvterm the line it gave us, so that text
  ;; reappears with its colours rather than as blanks.
  (let ((scrollback (vt-scrollback vt)))
    (if (zerop (fill-pointer scrollback))
        0
        (let ((line (vector-pop scrollback)))
          (dotimes (i (min cols (length line)))
            (encode-cell (aref line i)
                         (cffi:mem-aptr cells '(:struct vterm-screen-cell) i)))
          1))))

(define-vt-callback vt-sb-clear :int ((user :pointer))
  (setf (fill-pointer (vt-scrollback vt)) 0)
  1)

;;; VTermProp: 1 cursorvisible, 2 cursorblink, 3 altscreen, 4 title, 5 iconname,
;;; 6 reverse, 7 cursorshape, 8 mouse.  Only the title and cursor visibility
;;; matter to us today.
(defun accumulate-title (vt value)
  "VTermProp 4 (title), which arrives in FRAGMENTS.

VTermValue is a union whose `string' member is a VTermStringFragment BY VALUE --
`const char *str' then `size_t len:30, initial:1, final:1' -- so VALUE points at
the fragment itself.  Reading a pointer out of it first, as though the union
held a pointer to a fragment, yields garbage and then nothing: the title test
failed with NIL for exactly that reason.

A title arrives in one fragment when it is short and several when it is not, and
`initial' and `final' are what say where it starts and stops."
  (let* ((str (cffi:mem-ref value :pointer))
         (packed (cffi:mem-ref value :uint64 8))
         (len (ldb (byte 30 0) packed))
         (initial (logbitp 30 packed))
         (final (logbitp 31 packed)))
    (when initial (setf (vt-title-buffer vt) (make-string-output-stream)))
    (unless (or (cffi:null-pointer-p str) (zerop len))
      (let ((octets (make-array len :element-type '(unsigned-byte 8))))
        (dotimes (i len)
          (setf (aref octets i) (cffi:mem-aref str :uint8 i)))
        (write-string (babel:octets-to-string octets :encoding :utf-8 :errorp nil)
                      (or (vt-title-buffer vt)
                          (setf (vt-title-buffer vt) (make-string-output-stream))))))
    (when (and final (vt-title-buffer vt))
      (setf (vt-title vt) (get-output-stream-string (vt-title-buffer vt))
            (vt-title-buffer vt) nil))))

(define-vt-callback vt-settermprop :int
    ((prop :int) (value :pointer) (user :pointer))
  ;; VTermProp: 1 cursorvisible, 2 cursorblink, 3 altscreen, 4 title,
  ;; 5 iconname, 6 reverse, 7 cursorshape, 8 mouse.
  (case prop
    (1 (setf (vt-cursor-visible vt) (plusp (cffi:mem-ref value :int))))
    (3 (setf (vt-alternate-screen-p vt) (plusp (cffi:mem-ref value :int))))
    (4 (accumulate-title vt value))
    ;; VTERM_PROP_MOUSE is an ENUM, not a boolean: 0 none, 1 click, 2 drag,
    ;; 3 move.  Anything but 0 means the child wants events, and treating it as
    ;; a boolean would be wrong only for a child that turned reporting off by
    ;; setting it to 0 -- which is exactly how it IS turned off.
    (8 (setf (vt-mouse-reporting-p vt) (plusp (cffi:mem-ref value :int)))))
  1)

;;; The output callback is not a screen callback and takes no structure, so it
;;; needs no shim.
(cffi:defcallback vt-output :void
    ((bytes :pointer) (len :unsigned-long) (user :pointer))
  (handler-case
      (let ((vt (terminal-for user)))
        (when (and vt (vt-output-hook vt))
          (let ((octets (make-array len :element-type '(unsigned-byte 8))))
            (dotimes (i len)
              (setf (aref octets i) (cffi:mem-aref bytes :uint8 i)))
            (funcall (vt-output-hook vt) octets))))
    (error (condition)
      (format *error-output* "~&cathode-ray-tube: vt output callback: ~A~%" condition))))

;;; Cells ----------------------------------------------------------------------

(defconstant +wide-continuation+ #xFFFFFFFF
  "What libvterm puts in chars[0] of the SECOND cell of a double-width glyph.

screen.c: `getcell(screen, pos.row, pos.col + col)->chars[0] = (uint32_t)-1'.
It is not a character and CODE-CHAR will not take it -- the first run of the
wide-character test died with `4294967295 is not of type (MOD 1114112)'.  The
renderer has to skip these cells rather than draw a blank in them, which is what
width 0 tells it.")

(defun decode-cell (pointer &optional (into (make-cell)))
  "One VTermScreenCell into a CELL, reusing INTO."
  (let* ((chars (cffi:foreign-slot-pointer pointer '(:struct vterm-screen-cell) 'chars))
         (base (cffi:mem-aref chars :uint32 0))
         (continuation (= base +wide-continuation+)))
    (setf (cell-char into)
          (if (or continuation (zerop base) (> base char-code-limit))
              #\Space
              (code-char base)))
    ;; Combining marks are almost never present, so the list is only built when
    ;; there is something to put in it.
    (let ((combining nil))
      (unless continuation
        (loop for i from 1 below +max-chars-per-cell+
              for cp = (cffi:mem-aref chars :uint32 i)
              while (and (plusp cp) (< cp char-code-limit))
              do (push (code-char cp) combining)))
      (setf (cell-combining into) (nreverse combining)))
    (setf (cell-width into)
          (if continuation
              0
              (max 0 (min 2 (cffi:foreign-slot-value pointer
                                                     '(:struct vterm-screen-cell)
                                                     'width))))
          (cell-attrs into)
          (decode-attrs (cffi:foreign-slot-value pointer '(:struct vterm-screen-cell)
                                                 'attrs))
          (cell-fg into)
          (decode-color (cffi:foreign-slot-value pointer '(:struct vterm-screen-cell) 'fg))
          (cell-bg into)
          (decode-color (cffi:foreign-slot-value pointer '(:struct vterm-screen-cell) 'bg)))
    into))

(defun encode-cell (cell pointer)
  "A CELL back into a VTermScreenCell, for sb_popline."
  (let ((chars (cffi:foreign-slot-pointer pointer '(:struct vterm-screen-cell) 'chars)))
    (dotimes (i +max-chars-per-cell+)
      (setf (cffi:mem-aref chars :uint32 i) 0))
    (setf (cffi:mem-aref chars :uint32 0) (char-code (cell-char cell)))
    (loop for c in (cell-combining cell)
          for i from 1 below +max-chars-per-cell+
          do (setf (cffi:mem-aref chars :uint32 i) (char-code c))))
  (setf (cffi:foreign-slot-value pointer '(:struct vterm-screen-cell) 'width)
        (cell-width cell))
  ;; The attribute and colour words are handed back as libvterm gave them only
  ;; in the sense that a popped line keeps its own colours; re-encoding the full
  ;; bitfield would mean carrying libvterm's exact layout in the cell, which is
  ;; the coupling this file exists to avoid.  Scrolled-back text keeps its
  ;; characters and width; its attributes come back as default.
  pointer)

;;; Construction ---------------------------------------------------------------

(defun reset-dirty (vt)
  (setf (vt-dirty vt) (make-array (vt-rows vt) :element-type 'bit :initial-element 1)))

(defmethod make-vt-backend ((backend (eql :libvterm)) rows cols &key)
  (ensure-libcathode)
  (let ((vt (make-instance 'libvterm-vt :rows rows :cols cols)))
    (setf (vt-scrollback vt) (make-array 1024 :adjustable t :fill-pointer 0))
    (reset-dirty vt)
    (let ((handle (%vterm-new rows cols)))
      (when (cffi:null-pointer-p handle)
        (error "vterm_new(~D, ~D) failed." rows cols))
      (setf (vt-handle vt) handle)
      (%vterm-set-utf8 handle 1)
      (let ((screen (%vterm-obtain-screen handle))
            (state (%vterm-obtain-state handle))
            (id (register-terminal vt)))
        (setf (vt-screen vt) screen (vt-state vt) state)
        (%vterm-output-set-callback handle (cffi:callback vt-output)
                                    (cffi:make-pointer id))
        ;; Callbacks BEFORE the reset: a reset damages the whole screen, and
        ;; installing afterwards drops that -- so the first frame looks clean
        ;; when it is not, and nothing repaints until something else changes.
        (setf *damage-handler* #'vt-damage
              *moverect-handler* #'vt-moverect
              *movecursor-handler* #'vt-movecursor)
        (multiple-value-bind (damage moverect movecursor) (%screen-trampolines)
          ;; The table is libvterm's own VTermScreenCallbacks now, not a
          ;; flattened stand-in, so this is vterm_screen_set_callbacks directly.
          ;; It is kept ALIVE for the terminal's life -- libvterm stores the
          ;; pointer and reads it on every write -- which is what VT-CALLBACKS
          ;; is for; a with-foreign-object here would be freed before the first
          ;; callback arrived.
          (let ((cbs (cffi:foreign-alloc '(:struct vterm-screen-callbacks))))
            (macrolet ((set-cb (slot value)
                         `(setf (cffi:foreign-slot-value
                                 cbs '(:struct vterm-screen-callbacks) ',slot)
                                ,value)))
              (set-cb damage damage)
              (set-cb moverect moverect)
              (set-cb movecursor movecursor)
              (set-cb settermprop (cffi:callback vt-settermprop))
              (set-cb bell (cffi:callback vt-bell))
              (set-cb resize (cffi:callback vt-resize-cb))
              (set-cb sb-pushline (cffi:callback vt-sb-pushline))
              (set-cb sb-popline (cffi:callback vt-sb-popline))
              (set-cb sb-clear (cffi:callback vt-sb-clear)))
            (setf (vt-callbacks vt) cbs)
            (%vterm-screen-set-callbacks screen cbs (cffi:make-pointer id))))
        (%vterm-screen-reset screen 1)
        (%vterm-screen-enable-altscreen screen 1)
        (%vterm-screen-enable-reflow screen t)
        (%vterm-screen-set-damage-merge screen +damage-scroll+)
        vt))))

(defmethod vt-close ((vt libvterm-vt))
  (when (vt-handle vt)
    (%vterm-free (vt-handle vt))
    (setf (vt-handle vt) nil (vt-screen vt) nil (vt-state vt) nil))
  ;; After vterm_free, not before: libvterm calls back during teardown, and the
  ;; table it reads is this one.
  (when (vt-callbacks vt)
    (cffi:foreign-free (vt-callbacks vt))
    (setf (vt-callbacks vt) nil))
  (when (vt-scratch vt)
    (cffi:foreign-free (vt-scratch vt))
    (setf (vt-scratch vt) nil (vt-scratch-cols vt) 0))
  (unregister-terminal vt)
  vt)

;;; The protocol ---------------------------------------------------------------

(defmethod vt-open-p ((vt libvterm-vt))
  (and (vt-handle vt) t))

(defmacro define-closed-answers (&body clauses)
  "For each (GENERIC LAMBDA-LIST ANSWER), an :AROUND method answering ANSWER
when the VT is closed.

VT-CLOSE nulls the handle and the object outlives it, so every method below that
reaches into C would hand NIL to a CFFI :POINTER -- which SBCL reports as `NIL is
not of type SB-SYS:SYSTEM-AREA-POINTER', from inside the foreign call, naming
nothing that would lead you back here.

That is not a hypothetical.  A child exiting closes the terminal from the exit
handler while a reader is still pulling cells out of it, and which of the two
wins is the scheduler's business: the same code is green on one machine and dies
on another.  It reached CI as one red assertion on one of three runners.

An answer per generic rather than a blanket NIL, because the callers want an
empty screen and not a type error one frame later: VT-TEXT of a dead terminal is
the empty string, its cells are blank cells, and writing to it is a no-op."
  `(progn
     ,@(loop for (generic lambda-list answer) in clauses
             for variables = (remove-if (lambda (x)
                                          (member x lambda-list-keywords))
                                        (mapcar (lambda (x)
                                                  (if (consp x) (first x) x))
                                                lambda-list))
             collect `(defmethod ,generic :around ,lambda-list
                        (declare (ignorable ,@variables))
                        (if (vt-open-p vt) (call-next-method) ,answer)))))

(define-closed-answers
  (vt-write        ((vt libvterm-vt) octets &key start end)             nil)
  (vt-resize       ((vt libvterm-vt) rows cols)                         vt)
  (vt-reset        ((vt libvterm-vt) &key hard)                         vt)
  (vt-row-cells    ((vt libvterm-vt) row cells)                         cells)
  (vt-cell         ((vt libvterm-vt) row col)                           (make-cell))
  (vt-text         ((vt libvterm-vt) start-row end-row
                    &key start-col end-col)                             "")
  (vt-mouse-move   ((vt libvterm-vt) row col modifiers)                 nil)
  (vt-mouse-button ((vt libvterm-vt) button pressed modifiers)          nil)
  (vt-start-paste  ((vt libvterm-vt))                                   nil)
  (vt-end-paste    ((vt libvterm-vt))                                   nil))

(defmethod vt-write ((vt libvterm-vt) octets &key (start 0) end)
  (let* ((end (or end (length octets)))
         (count (- end start)))
    (when (plusp count)
      (cffi:with-foreign-object (buffer :uint8 count)
        (loop for i from 0 below count
              do (setf (cffi:mem-aref buffer :uint8 i) (aref octets (+ start i))))
        (%vterm-input-write (vt-handle vt) buffer count)))))

(defmethod vt-resize ((vt libvterm-vt) rows cols)
  (unless (and (= rows (vt-rows vt)) (= cols (vt-cols vt)))
    (%vterm-set-size (vt-handle vt) rows cols)
    (setf (slot-value vt 'rows) rows (slot-value vt 'cols) cols)
    (when (vt-scratch vt)
      (cffi:foreign-free (vt-scratch vt))
      (setf (vt-scratch vt) nil (vt-scratch-cols vt) 0))
    (reset-dirty vt))
  vt)

(defmethod vt-reset ((vt libvterm-vt) &key (hard t))
  (%vterm-screen-reset (vt-screen vt) (if hard 1 0))
  (reset-dirty vt)
  vt)

(defun ensure-scratch (vt)
  "One foreign row of cells, reused across frames."
  (let ((cols (vt-cols vt)))
    (unless (and (vt-scratch vt) (>= (vt-scratch-cols vt) cols))
      (when (vt-scratch vt) (cffi:foreign-free (vt-scratch vt)))
      (setf (vt-scratch vt)
            (cffi:foreign-alloc '(:struct vterm-screen-cell) :count cols)
            (vt-scratch-cols vt) cols))
    (vt-scratch vt)))

(defmethod vt-row-cells ((vt libvterm-vt) row cells)
  (let* ((cols (min (vt-cols vt) (length cells)))
         (scratch (ensure-scratch vt)))
    ;; One FFI call per ROW.  Per cell would be sixty times as many crossings
    ;; for the same bytes.
    (screen-get-row (vt-screen vt) row cols scratch)
    (dotimes (i cols cells)
      (let ((existing (aref cells i)))
        (setf (aref cells i)
              (decode-cell (cffi:mem-aptr scratch '(:struct vterm-screen-cell) i)
                           (or existing (make-cell))))))))

(defmethod vt-cell ((vt libvterm-vt) row col)
  (cffi:with-foreign-object (cell '(:struct vterm-screen-cell))
    (if (plusp (%screen-get-cell (vt-screen vt) row col cell))
        (decode-cell cell)
        (make-cell))))

(defmethod vt-cursor ((vt libvterm-vt))
  (values (vt-cursor-row vt) (vt-cursor-col vt) (vt-cursor-visible vt)))

(defmethod vt-dirty-rows ((vt libvterm-vt))
  (vt-dirty vt))

(defmethod vt-clear-dirty ((vt libvterm-vt))
  (fill (vt-dirty vt) 0)
  vt)

(defmethod vt-damage-all ((vt libvterm-vt))
  (fill (vt-dirty vt) 1)
  vt)

(defmethod vt-text ((vt libvterm-vt) start-row end-row &key (start-col 0) end-col)
  (let* ((end-col (or end-col (vt-cols vt)))
         ;; Ask for the length first, then fill: the usual two-call dance, and
         ;; necessary because a cell can be several UTF-8 bytes.
         (size (%screen-get-text (vt-screen vt) (cffi:null-pointer) 0
                                     start-row end-row start-col end-col)))
    (if (zerop size)
        ""
        (cffi:with-foreign-object (buffer :uint8 (1+ size))
          (let ((written (%screen-get-text (vt-screen vt) buffer (1+ size)
                                               start-row end-row start-col end-col)))
            (babel:octets-to-string
             (let ((octets (make-array written :element-type '(unsigned-byte 8))))
               (dotimes (i written octets)
                 (setf (aref octets i) (cffi:mem-aref buffer :uint8 i))))
             :encoding :utf-8 :errorp nil))))))

(defmethod vt-mouse-move ((vt libvterm-vt) row col modifiers)
  (%vterm-mouse-move (vt-handle vt) row col modifiers))

(defmethod vt-mouse-button ((vt libvterm-vt) button pressed modifiers)
  (%vterm-mouse-button (vt-handle vt) button (and pressed t) modifiers))

(defmethod vt-start-paste ((vt libvterm-vt))
  (%vterm-keyboard-start-paste (vt-handle vt)))

(defmethod vt-end-paste ((vt libvterm-vt))
  (%vterm-keyboard-end-paste (vt-handle vt)))

(defmethod vt-scrollback-length ((vt libvterm-vt))
  (fill-pointer (vt-scrollback vt)))

(defmethod vt-scrollback-line ((vt libvterm-vt) n)
  (let* ((scrollback (vt-scrollback vt))
         (count (fill-pointer scrollback)))
    (when (< -1 n count)
      (aref scrollback (- count 1 n)))))
