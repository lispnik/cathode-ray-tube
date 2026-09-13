;;;; src/vt/vterm-abi-ecl.lisp -- libvterm's by-value crossings, on ECL.
;;;;
;;;; ECL's DYNAMIC FFI, which is libffi: SI:CALL-CFUN outbound and
;;;; SI::MAKE-DYNAMIC-CALLBACK inbound.  See vterm-abi.lisp for what this is and
;;;; vterm-abi-sbcl.lisp for the other half.
;;;;
;;;; NEEDS lispnik/ecl.  Stock ECL refuses a structure outright -- "does not
;;;; denote an elementary foreign type" -- which is the right way round: it
;;;; signals rather than passing the wrong thing.  The README says why, and
;;;; ci-ecl.yml proves the ECL in use is the fork before running anything.

(in-package #:cathode-ray-tube.vt)

(defparameter +vterm-rect-dffi+
  '(:struct (:m :int) (:m :int) (:m :int) (:m :int))
  "VTermRect, as the dynamic FFI describes a structure: member types in order.
The names are not read by anything.")

(defparameter +vterm-pos-dffi+ '(:struct (:m :int) (:m :int))
  "VTermPos.")

(defun %symbol-address (name)
  (or (cffi:foreign-symbol-pointer name)
      (error "Could not find ~A." name)))

;;; Outbound -----------------------------------------------------------------
;;;
;;; A structure argument is passed as a POINTER TO ITS BYTES -- libffi copies it
;;; out of there -- and not as a list of members.  The error when you get that
;;; wrong says so, which is better than most.

(defun %screen-get-cell (screen row col cell)
  "vterm_screen_get_cell(screen, (VTermPos){row, col}, cell)."
  (let ((pos (ffi:allocate-foreign-object '(:array :int 2))))
    (unwind-protect
         (progn
           (setf (ffi:deref-array pos '(:array :int 2) 0) row
                 (ffi:deref-array pos '(:array :int 2) 1) col)
           (si:call-cfun (%symbol-address "vterm_screen_get_cell")
                         :int
                         (list :pointer-void +vterm-pos-dffi+ :pointer-void)
                         (list screen pos cell)))
      (ffi:free-foreign-object pos))))

(defun %screen-get-text (screen str len start-row end-row start-col end-col)
  "vterm_screen_get_text(screen, str, len, (VTermRect){...})."
  (let ((rect (ffi:allocate-foreign-object '(:array :int 4))))
    (unwind-protect
         (progn
           (setf (ffi:deref-array rect '(:array :int 4) 0) start-row
                 (ffi:deref-array rect '(:array :int 4) 1) end-row
                 (ffi:deref-array rect '(:array :int 4) 2) start-col
                 (ffi:deref-array rect '(:array :int 4) 3) end-col)
           (si:call-cfun (%symbol-address "vterm_screen_get_text")
                         :unsigned-long
                         (list :pointer-void :pointer-void :unsigned-long
                               +vterm-rect-dffi+)
                         (list screen str len rect)))
      (ffi:free-foreign-object rect))))

;;; Inbound ------------------------------------------------------------------
;;;
;;; A structure ARRIVES the same way it is sent: as a pointer to its bytes.
;;; ECL keeps what a closure needs alive on the name's plist, and the names here
;;; are interned, so the addresses stay valid for the life of the image -- which
;;; is what libvterm requires of a callback table it keeps.

(defvar *screen-trampolines* nil)

(defun %screen-trampolines ()
  "(values DAMAGE MOVERECT MOVECURSOR) as C function pointers."
  (unless *screen-trampolines*
    (setf *screen-trampolines*
          (list
           (si::make-dynamic-callback
            (lambda (rect user)
              (call-damage (ffi:deref-array rect '(:array :int 4) 0)
                           (ffi:deref-array rect '(:array :int 4) 1)
                           (ffi:deref-array rect '(:array :int 4) 2)
                           (ffi:deref-array rect '(:array :int 4) 3)
                           user))
            'crt-damage-tramp :int (list +vterm-rect-dffi+ :pointer-void))
           (si::make-dynamic-callback
            (lambda (dest src user)
              (call-moverect (ffi:deref-array dest '(:array :int 4) 0)
                             (ffi:deref-array dest '(:array :int 4) 1)
                             (ffi:deref-array dest '(:array :int 4) 2)
                             (ffi:deref-array dest '(:array :int 4) 3)
                             (ffi:deref-array src '(:array :int 4) 0)
                             (ffi:deref-array src '(:array :int 4) 1)
                             (ffi:deref-array src '(:array :int 4) 2)
                             (ffi:deref-array src '(:array :int 4) 3)
                             user))
            'crt-moverect-tramp :int
            (list +vterm-rect-dffi+ +vterm-rect-dffi+ :pointer-void))
           (si::make-dynamic-callback
            (lambda (pos oldpos visible user)
              (call-movecursor (ffi:deref-array pos '(:array :int 2) 0)
                               (ffi:deref-array pos '(:array :int 2) 1)
                               (ffi:deref-array oldpos '(:array :int 2) 0)
                               (ffi:deref-array oldpos '(:array :int 2) 1)
                               visible user))
            'crt-movecursor-tramp :int
            (list +vterm-pos-dffi+ +vterm-pos-dffi+ :int :pointer-void)))))
  (values-list *screen-trampolines*))
