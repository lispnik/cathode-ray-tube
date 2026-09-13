;;;; src/vt/vterm-abi-sbcl.lisp -- libvterm's by-value crossings, on SBCL.
;;;;
;;;; sb-alien, in both directions.  See vterm-abi.lisp for what this is, and
;;;; vterm-abi-ecl.lisp for the other half.
;;;;
;;;; The three trampolines are written out rather than generated: libvterm's
;;;; callback signatures are fixed and known here, so there is nothing to
;;;; discover at runtime.  objc builds its equivalents with EVAL because it is
;;;; handed an arbitrary type encoding; this is the same mechanism with the
;;;; hard part already answered.

(in-package #:cathode-ray-tube.vt)

(sb-alien:define-alien-type nil
  (sb-alien:struct vterm-rect-alien
                   (start-row sb-alien:int) (end-row sb-alien:int)
                   (start-col sb-alien:int) (end-col sb-alien:int)))

(sb-alien:define-alien-type nil
  (sb-alien:struct vterm-pos-alien (row sb-alien:int) (col sb-alien:int)))

;;; Outbound -----------------------------------------------------------------

(defun %screen-get-cell (screen row col cell)
  "vterm_screen_get_cell(screen, (VTermPos){row, col}, cell)."
  (sb-alien:with-alien ((pos (sb-alien:struct vterm-pos-alien)))
    (setf (sb-alien:slot pos 'row) row
          (sb-alien:slot pos 'col) col)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "vterm_screen_get_cell"
                            (function sb-alien:int
                                      sb-sys:system-area-pointer
                                      (sb-alien:struct vterm-pos-alien)
                                      sb-sys:system-area-pointer))
     (sb-sys:int-sap (cffi:pointer-address screen))
     pos
     (sb-sys:int-sap (cffi:pointer-address cell)))))

(defun %screen-get-text (screen str len start-row end-row start-col end-col)
  "vterm_screen_get_text(screen, str, len, (VTermRect){...}).

Returns the number of bytes the text needs, which is what makes the usual
two-call dance work: ask with a null pointer and a zero length first."
  (sb-alien:with-alien ((rect (sb-alien:struct vterm-rect-alien)))
    (setf (sb-alien:slot rect 'start-row) start-row
          (sb-alien:slot rect 'end-row) end-row
          (sb-alien:slot rect 'start-col) start-col
          (sb-alien:slot rect 'end-col) end-col)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "vterm_screen_get_text"
                            (function sb-alien:unsigned-long
                                      sb-sys:system-area-pointer
                                      sb-sys:system-area-pointer
                                      sb-alien:unsigned-long
                                      (sb-alien:struct vterm-rect-alien)))
     (sb-sys:int-sap (cffi:pointer-address screen))
     (sb-sys:int-sap (cffi:pointer-address str))
     len
     rect)))

;;; Inbound ------------------------------------------------------------------
;;;
;;; A callable's structure parameter is NOT ADDRESSABLE -- (addr p) is rejected
;;; with "P is not a valid L-value" -- so each is copied into a WITH-ALIEN local
;;; before its slots are read.  objc's abi.lisp records the same trap.

(sb-alien:define-alien-callable %damage-tramp sb-alien:int
    ((rect (sb-alien:struct vterm-rect-alien))
     (user sb-sys:system-area-pointer))
  (sb-alien:with-alien ((r (sb-alien:struct vterm-rect-alien)))
    (setf r rect)
    (call-damage (sb-alien:slot r 'start-row) (sb-alien:slot r 'end-row)
                 (sb-alien:slot r 'start-col) (sb-alien:slot r 'end-col)
                 (cffi:make-pointer (sb-sys:sap-int user)))))

(sb-alien:define-alien-callable %moverect-tramp sb-alien:int
    ((dest (sb-alien:struct vterm-rect-alien))
     (src (sb-alien:struct vterm-rect-alien))
     (user sb-sys:system-area-pointer))
  (sb-alien:with-alien ((d (sb-alien:struct vterm-rect-alien))
                        (s (sb-alien:struct vterm-rect-alien)))
    (setf d dest s src)
    (call-moverect (sb-alien:slot d 'start-row) (sb-alien:slot d 'end-row)
                   (sb-alien:slot d 'start-col) (sb-alien:slot d 'end-col)
                   (sb-alien:slot s 'start-row) (sb-alien:slot s 'end-row)
                   (sb-alien:slot s 'start-col) (sb-alien:slot s 'end-col)
                   (cffi:make-pointer (sb-sys:sap-int user)))))

(sb-alien:define-alien-callable %movecursor-tramp sb-alien:int
    ((pos (sb-alien:struct vterm-pos-alien))
     (oldpos (sb-alien:struct vterm-pos-alien))
     (visible sb-alien:int)
     (user sb-sys:system-area-pointer))
  (sb-alien:with-alien ((p (sb-alien:struct vterm-pos-alien))
                        (o (sb-alien:struct vterm-pos-alien)))
    (setf p pos o oldpos)
    (call-movecursor (sb-alien:slot p 'row) (sb-alien:slot p 'col)
                     (sb-alien:slot o 'row) (sb-alien:slot o 'col)
                     visible
                     (cffi:make-pointer (sb-sys:sap-int user)))))

(defun %screen-trampolines ()
  "(values DAMAGE MOVERECT MOVECURSOR) as C function pointers.

ALIEN-CALLABLE-FUNCTION returns an alien value; libvterm wants the address, and
handing it the alien is a type error."
  (flet ((address (name)
           (cffi:make-pointer
            (sb-sys:sap-int (sb-alien:alien-sap
                             (sb-alien:alien-callable-function name))))))
    (values (address '%damage-tramp)
            (address '%moverect-tramp)
            (address '%movecursor-tramp))))
