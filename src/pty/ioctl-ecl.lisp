;;;; src/pty/ioctl-ecl.lisp -- the variadic ioctl, on ECL.
;;;;
;;;; The counterpart to ioctl-sbcl.lisp.  Same one-function contract, different
;;;; machinery: ECL's DYNAMIC FFI, which is libffi, reached through SI:CALL-CFUN.
;;;;
;;;; NEEDS lispnik/ecl.  Variadic calls through the dynamic interface are a fix
;;;; that stock ECL does not carry -- on stock this signals rather than
;;;; misbehaving, which is the right way round.  ci-ecl.yml builds the fork and
;;;; proves it is the fork before running anything; the README says why.

(in-package #:cathode-ray-tube.pty)

(defvar *ioctl-address* nil
  "ioctl's entry point, looked up once.")

(defun %ioctl-address ()
  (or *ioctl-address*
      (setf *ioctl-address*
            (or (cffi:foreign-symbol-pointer "ioctl")
                (error "Could not find ioctl.")))))

(defun %ioctl-pointer (fd request pointer)
  "ioctl(fd, request, pointer), as a genuine VARIADIC call.

The trailing :DEFAULT 2 is the whole of it: :DEFAULT names the calling
convention and 2 is the number of FIXED arguments, so libffi is told through
ffi_prep_cif_var where the variadic part begins.  Without it the pointer goes in
a register, which on Darwin arm64 is not where a variadic argument belongs.

objc/src/abi-ecl.lisp's %DYNAMIC-TRAMPOLINE passes the same pair."
  (si:call-cfun (%ioctl-address)
                :int
                '(:int :unsigned-long :pointer-void)
                (list fd request pointer)
                :default 2))
