;;;; src/vt/cells.lisp -- what a terminal cell is, independent of the backend.
;;;;
;;;; This is the boundary the renderer sees.  It deliberately does not resemble
;;;; libvterm's VTermScreenCell: a backend converts into this, and a different
;;;; backend converting into the same thing is what makes the backend
;;;; replaceable.

(in-package #:cathode-ray-tube.vt)

;;; Attributes, as a bitfield.  One fixnum per cell rather than a struct with
;;; nine boolean slots, because the text pass walks every cell of every dirty
;;; row and the difference is real.
(defconstant +attr-bold+      #x01)
(defconstant +attr-underline+ #x02)   ; see CELL-UNDERLINE-STYLE for which kind
(defconstant +attr-italic+    #x04)
(defconstant +attr-blink+     #x08)
(defconstant +attr-reverse+   #x10)
(defconstant +attr-conceal+   #x20)
(defconstant +attr-strike+    #x40)
;;; Bits 8-9 carry the underline STYLE (single, double, curly), because
;;; libvterm's `underline' is a two-bit field and not a flag.
(defconstant +attr-underline-shift+ 8)

(declaim (inline attr-set-p cell-underline-style))

(defun attr-set-p (attrs flag)
  (logtest attrs flag))

(defun cell-underline-style (attrs)
  "0 none, 1 single, 2 double, 3 curly."
  (ldb (byte 2 +attr-underline-shift+) attrs))

;;; Colour ---------------------------------------------------------------------
;;;
;;; A terminal colour is one of three things and the renderer has to tell them
;;; apart: an RGB triple, an index into the 256-colour palette, or "whatever the
;;; default is", which the PROFILE decides rather than the terminal.  That last
;;; case is why this is not simply an RGB triple: cool-retro-term's profiles
;;; recolour the default foreground and background, and a cell that has been
;;; explicitly set to white must not be recoloured with them.

(defstruct (vt-color (:constructor make-vt-color (&key indexed-p index red green blue
                                                       default-fg-p default-bg-p)))
  (indexed-p nil :type boolean)
  (index 0 :type (unsigned-byte 8))
  (red 0 :type (unsigned-byte 8))
  (green 0 :type (unsigned-byte 8))
  (blue 0 :type (unsigned-byte 8))
  (default-fg-p nil :type boolean)
  (default-bg-p nil :type boolean))

(defparameter +default-palette+
  (let ((palette (make-array 256)))
    ;; The 16 ANSI colours, in the values xterm uses.  cool-retro-term does not
    ;; ship a colour scheme -- the shader recolours everything -- so these are
    ;; only ever seen through CONVERT-WITH-CHROMA, but they still have to be
    ;; right relative to each other.
    (loop for i from 0
          for (r g b) in '((0 0 0) (205 0 0) (0 205 0) (205 205 0)
                           (0 0 238) (205 0 205) (0 205 205) (229 229 229)
                           (127 127 127) (255 0 0) (0 255 0) (255 255 0)
                           (92 92 255) (255 0 255) (0 255 255) (255 255 255))
          do (setf (aref palette i) (list r g b)))
    ;; The 6x6x6 colour cube, 16-231.
    (let ((steps #(0 95 135 175 215 255)))
      (loop for i from 16 below 232
            for n = (- i 16)
            do (setf (aref palette i)
                     (list (aref steps (floor n 36))
                           (aref steps (mod (floor n 6) 6))
                           (aref steps (mod n 6))))))
    ;; The 24 greys, 232-255.
    (loop for i from 232 below 256
          for level = (+ 8 (* 10 (- i 232)))
          do (setf (aref palette i) (list level level level)))
    palette)
  "The xterm 256-colour palette, as (R G B) triples.")

(defun resolve-color (color &key (palette +default-palette+) default)
  "COLOR as (values R G B), or DEFAULT's values when it is a default colour.

DEFAULT is what the PROFILE says the foreground or background is; a terminal
that has not been told otherwise inherits it, and one that has does not."
  (cond ((null color) (values-list (or default '(0 0 0))))
        ((or (vt-color-default-fg-p color) (vt-color-default-bg-p color))
         (values-list (or default '(0 0 0))))
        ((vt-color-indexed-p color)
         (values-list (aref palette (vt-color-index color))))
        (t (values (vt-color-red color) (vt-color-green color)
                   (vt-color-blue color)))))

;;; The cell -------------------------------------------------------------------

(defstruct (cell (:constructor make-cell (&key (char #\Space) combining (width 1)
                                               fg bg (attrs 0))))
  "One character cell.

CHAR is the base character; COMBINING is NIL or a list of combining marks, which
is almost always NIL and is a list rather than a fixed array because the common
case should not pay for the rare one.  WIDTH is 1, or 2 for a CJK glyph, or 0
for the second half of one."
  (char #\Space :type character)
  (combining nil :type list)
  (width 1 :type (integer 0 2))
  (fg nil)
  (bg nil)
  (attrs 0 :type fixnum))

(defun cell-blank-p (cell)
  "True when nothing would be drawn for CELL but its background."
  (and (or (char= (cell-char cell) #\Space) (char= (cell-char cell) #\Nul))
       (null (cell-combining cell))
       (not (attr-set-p (cell-attrs cell) +attr-underline+))
       (not (attr-set-p (cell-attrs cell) +attr-strike+))))
