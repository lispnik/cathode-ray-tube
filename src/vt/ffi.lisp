;;;; src/vt/ffi.lisp -- libvterm, through vendor/shim.
;;;;
;;;; Only what we use.  Not generated: of libvterm's surface we need about
;;;; thirty entry points, and hand-writing them is an afternoon while a
;;;; generator needs a C parser and a toolchain at Lisp build time -- which is
;;;; the discipline objc explicitly rejects and this project inherits.
;;;;
;;;; Reading a structure THROUGH A POINTER is fine in CFFI, so VTermScreenCell
;;;; arrives here unchanged and is decoded with DEFCSTRUCT.  Only crossings BY
;;;; VALUE need the shim, and those are the crt_* entry points below.

(in-package #:cathode-ray-tube.vt)

(cffi:define-foreign-library libcathode
  (t (:default "libcathode")))

(defun ensure-libcathode ()
  (unless (cffi:foreign-library-loaded-p 'libcathode)
    (cffi:use-foreign-library libcathode)))

;;; Types ----------------------------------------------------------------------

(defconstant +max-chars-per-cell+ 6
  "VTERM_MAX_CHARS_PER_CELL: a base character and up to five combining marks.
Past that libvterm discards, which is a real limit on emoji ZWJ sequences and a
theoretical one on a terminal whose fonts are IBM VGA 8x16 and Commodore PET.")

;;; Measured with a C program rather than guessed -- 40 bytes, and `width' sits
;;; at offset 24 while the bitfield that follows starts at 28, because it
;;; realigns.  Getting this wrong reads colours out of the padding.
(cffi:defcstruct (vterm-screen-cell :size 40)
  (chars :uint32 :count 6)
  (width :char   :offset 24)
  (attrs :uint32 :offset 28)
  (fg    :uint32 :offset 32)
  (bg    :uint32 :offset 36))

;;; VTermColor is a four-byte tagged union: byte 0 is the type, and for an
;;; indexed colour byte 1 is the palette index, while for an RGB one bytes 1-3
;;; are red, green and blue.  Decoding it from a single uint32 avoids a
;;; defcstruct for four bytes.
(defconstant +color-type-mask+    #x01)
(defconstant +color-indexed+      #x01)
(defconstant +color-default-fg+   #x02)
(defconstant +color-default-bg+   #x04)

(defun decode-color (word)
  (let ((type (ldb (byte 8 0) word)))
    (make-vt-color
     :indexed-p (= +color-indexed+ (logand type +color-type-mask+))
     :index (ldb (byte 8 8) word)
     :red (ldb (byte 8 8) word)
     :green (ldb (byte 8 16) word)
     :blue (ldb (byte 8 24) word)
     :default-fg-p (logtest type +color-default-fg+)
     :default-bg-p (logtest type +color-default-bg+))))

;;; The attribute bitfield, LSB first as clang lays it out on arm64:
;;;   bold:1 underline:2 italic:1 blink:1 reverse:1 conceal:1 strike:1
;;;   font:4 dwl:1 dhl:2 small:1 baseline:2
(defun decode-attrs (word)
  (let ((attrs 0)
        (underline (ldb (byte 2 1) word)))
    (when (logbitp 0 word) (setf attrs (logior attrs +attr-bold+)))
    (when (plusp underline)
      (setf attrs (logior attrs +attr-underline+))
      (setf (ldb (byte 2 +attr-underline-shift+) attrs) underline))
    (when (logbitp 3 word) (setf attrs (logior attrs +attr-italic+)))
    (when (logbitp 4 word) (setf attrs (logior attrs +attr-blink+)))
    (when (logbitp 5 word) (setf attrs (logior attrs +attr-reverse+)))
    (when (logbitp 6 word) (setf attrs (logior attrs +attr-conceal+)))
    (when (logbitp 7 word) (setf attrs (logior attrs +attr-strike+)))
    attrs))

;;; libvterm ------------------------------------------------------------------

(cffi:defcfun ("vterm_new" %vterm-new) :pointer (rows :int) (cols :int))
(cffi:defcfun ("vterm_free" %vterm-free) :void (vt :pointer))
(cffi:defcfun ("vterm_set_utf8" %vterm-set-utf8) :void (vt :pointer) (utf8 :int))
(cffi:defcfun ("vterm_set_size" %vterm-set-size) :void
  (vt :pointer) (rows :int) (cols :int))
(cffi:defcfun ("vterm_input_write" %vterm-input-write) :unsigned-long
  (vt :pointer) (bytes :pointer) (len :unsigned-long))
(cffi:defcfun ("vterm_output_set_callback" %vterm-output-set-callback) :void
  (vt :pointer) (fn :pointer) (user :pointer))

(cffi:defcfun ("vterm_obtain_screen" %vterm-obtain-screen) :pointer (vt :pointer))
(cffi:defcfun ("vterm_obtain_state" %vterm-obtain-state) :pointer (vt :pointer))
(cffi:defcfun ("vterm_screen_reset" %vterm-screen-reset) :void
  (screen :pointer) (hard :int))
(cffi:defcfun ("vterm_screen_enable_altscreen" %vterm-screen-enable-altscreen) :void
  (screen :pointer) (altscreen :int))
(cffi:defcfun ("vterm_screen_enable_reflow" %vterm-screen-enable-reflow) :void
  (screen :pointer) (reflow :bool))
(cffi:defcfun ("vterm_screen_set_damage_merge" %vterm-screen-set-damage-merge) :void
  (screen :pointer) (size :int))
(cffi:defcfun ("vterm_screen_flush_damage" %vterm-screen-flush-damage) :void
  (screen :pointer))
(cffi:defcfun ("vterm_state_get_cursorpos" %vterm-state-get-cursorpos) :void
  (state :pointer) (pos :pointer))

;;; VTERM_DAMAGE_SCROLL merges damage to whole rows, which is exactly the
;;; granularity the renderer works in -- it uploads dirty ROWS -- so finer
;;; damage would be thrown away anyway.
(defconstant +damage-cell+   0)
(defconstant +damage-row+    1)
(defconstant +damage-screen+ 2)
(defconstant +damage-scroll+ 3)

;;; Keyboard and mouse: libvterm turns events into the bytes a terminal expects,
;;; which is a large amount of detail nobody should reimplement.
(cffi:defcfun ("vterm_keyboard_unichar" %vterm-keyboard-unichar) :void
  (vt :pointer) (c :uint32) (mod :int))
(cffi:defcfun ("vterm_keyboard_key" %vterm-keyboard-key) :void
  (vt :pointer) (key :int) (mod :int))
(cffi:defcfun ("vterm_keyboard_start_paste" %vterm-keyboard-start-paste) :void
  (vt :pointer))
(cffi:defcfun ("vterm_keyboard_end_paste" %vterm-keyboard-end-paste) :void
  (vt :pointer))
(cffi:defcfun ("vterm_mouse_move" %vterm-mouse-move) :void
  (vt :pointer) (row :int) (col :int) (mod :int))
(cffi:defcfun ("vterm_mouse_button" %vterm-mouse-button) :void
  (vt :pointer) (button :int) (pressed :bool) (mod :int))

;;; The shim's flattened entry points -----------------------------------------

(cffi:defcstruct crt-screen-callbacks
  (damage :pointer) (moverect :pointer) (movecursor :pointer)
  (settermprop :pointer) (bell :pointer) (resize :pointer)
  (sb-pushline :pointer) (sb-popline :pointer) (sb-clear :pointer))

(cffi:defcfun ("crt_screen_set_callbacks" %crt-screen-set-callbacks) :pointer
  (screen :pointer) (cbs :pointer) (user :pointer))
(cffi:defcfun ("crt_screen_context_free" %crt-screen-context-free) :void
  (ctx :pointer))
(cffi:defcfun ("crt_screen_get_row" %crt-screen-get-row) :int
  (screen :pointer) (row :int) (cols :int) (cells :pointer))
(cffi:defcfun ("crt_screen_get_cell" %crt-screen-get-cell) :int
  (screen :pointer) (row :int) (col :int) (cell :pointer))
(cffi:defcfun ("crt_screen_get_text" %crt-screen-get-text) :unsigned-long
  (screen :pointer) (str :pointer) (len :unsigned-long)
  (start-row :int) (end-row :int) (start-col :int) (end-col :int))
