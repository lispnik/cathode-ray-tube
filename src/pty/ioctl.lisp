;;;; src/pty/ioctl.lisp -- TIOCSWINSZ, without the shim.
;;;;
;;;; The first piece of vendor/shim to be replaced by Lisp, and it is first
;;;; because it is the smallest complete example of the shape: one variadic C
;;;; call, reached two ways, with everything else portable.
;;;;
;;;; ioctl is VARIADIC -- int ioctl(int, unsigned long, ...) -- and on Darwin
;;;; arm64 a variadic argument goes on the STACK while a fixed one goes in a
;;;; register.  A fixed-arity declaration therefore compiles, links, runs, and
;;;; silently tells the kernel nothing: measured, it returns -1 and the child's
;;;; `stty size' stays at whatever it was.  CFFI cannot express the variadic
;;;; form at all -- it only ever calls ffi_prep_cif, never ffi_prep_cif_var --
;;;; which is why this went through C to begin with.
;;;;
;;;; Both implementations CAN express it, differently, so the call itself is the
;;;; only thing that varies:
;;;;
;;;;   SBCL   %IOCTL-POINTER in ioctl-sbcl.lisp, splicing &optional into the
;;;;          alien signature ahead of the variadic argument.
;;;;   ECL    ioctl-ecl.lisp, si:call-cfun with a trailing :DEFAULT n-fixed.
;;;;          Needs lispnik/ecl -- see the README.
;;;;
;;;; Everything else lives here and is shared.

(in-package #:cathode-ray-tube.pty)

;;; The request number, derived rather than remembered ------------------------
;;;
;;; <sys/ioccom.h> builds these out of a direction, a group letter, a number and
;;; the size of the argument:
;;;
;;;   #define _IOC(inout,group,num,len) \
;;;     (inout | ((len & IOCPARM_MASK) << 16) | ((group) << 8) | (num))
;;;   #define _IOW(g,n,t)  _IOC(IOC_IN,  (g), (n), sizeof(t))
;;;   #define _IOR(g,n,t)  _IOC(IOC_OUT, (g), (n), sizeof(t))
;;;
;;; Doing the same arithmetic here is not the same as writing the number down.
;;; A typed constant is unfalsifiable -- 2148037735 is right or wrong and
;;; nothing about it says which -- while this is the derivation the header
;;; performs, with each input named.  Both values are asserted against a live
;;; pty in the suite, which is the only oracle that cannot drift.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ioc-in+ #x80000000
    "IOC_IN: the argument is copied IN to the kernel.")
  (defconstant +ioc-out+ #x40000000
    "IOC_OUT: the argument is copied back OUT.")
  (defconstant +iocparm-mask+ #x1fff))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ioc (direction group number length)
    "<sys/ioccom.h>'s _IOC, with its arguments named.

EVAL-WHEN because the constants below are computed with it AT COMPILE TIME, and
a plain DEFUN does not exist until load time -- which is a compile-time error
naming IOC and not the DEFCONSTANT that called it."
    (logior direction
            (ash (logand length +iocparm-mask+) 16)
            (ash (char-code group) 8)
            number)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +sizeof-winsize+ 8
    "struct winsize is four unsigned shorts and has been since 4.3BSD.

Asserted rather than trusted: the request number encodes this size, so a struct
that grew would make every ioctl here fail with EINVAL rather than misbehave --
and WINSIZE-ROUND-TRIPS would catch it on the first run."))

(defconstant +tiocswinsz+ (ioc +ioc-in+ #\t 103 +sizeof-winsize+))
(defconstant +tiocgwinsz+ (ioc +ioc-out+ #\t 104 +sizeof-winsize+))

(cffi:defcstruct winsize
  (row :unsigned-short)
  (col :unsigned-short)
  (xpixel :unsigned-short)
  (ypixel :unsigned-short))

;;; The two calls -------------------------------------------------------------

(defun set-winsize (fd rows cols)
  "Tell the kernel the pty is ROWS by COLS, which sends the child SIGWINCH."
  (when (and fd (>= fd 0) (plusp rows) (plusp cols))
    (cffi:with-foreign-object (ws '(:struct winsize))
      (setf (cffi:foreign-slot-value ws '(:struct winsize) 'row) rows
            (cffi:foreign-slot-value ws '(:struct winsize) 'col) cols
            (cffi:foreign-slot-value ws '(:struct winsize) 'xpixel) 0
            (cffi:foreign-slot-value ws '(:struct winsize) 'ypixel) 0)
      (zerop (%ioctl-pointer fd +tiocswinsz+ ws)))))

(defun get-winsize (fd)
  "(values ROWS COLS) as the kernel has them, or NIL."
  (when (and fd (>= fd 0))
    (cffi:with-foreign-object (ws '(:struct winsize))
      (when (zerop (%ioctl-pointer fd +tiocgwinsz+ ws))
        (values (cffi:foreign-slot-value ws '(:struct winsize) 'row)
                (cffi:foreign-slot-value ws '(:struct winsize) 'col))))))
