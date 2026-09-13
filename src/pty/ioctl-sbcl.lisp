;;;; src/pty/ioctl-sbcl.lisp -- the variadic ioctl, on SBCL.
;;;;
;;;; ONE of two files in the portable half allowed to name an implementation,
;;;; and tests/seam-tests.lisp asserts that it is one of exactly two.  See
;;;; ioctl.lisp for what this is and ioctl-ecl.lisp for the other half.

(in-package #:cathode-ray-tube.pty)

(defun %ioctl-pointer (fd request pointer)
  "ioctl(fd, request, pointer), as a genuine VARIADIC call.

&optional ahead of the third argument is the whole of it.  Without it SBCL
generates a fixed-arity call and passes the pointer in a register; Darwin arm64
expects a variadic argument on the stack, so the kernel reads whatever was there
and the call fails.  Measured against a live pty, both ways: fixed arity returns
-1 with the size unchanged, this returns 0 with the size set.

objc/src/abi.lisp does the same thing in BUILD-TRAMPOLINE, where its N-FIXED
argument marks where the splice goes."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "ioctl"
                          (function sb-alien:int
                                    sb-alien:int
                                    sb-alien:unsigned-long
                                    &optional sb-sys:system-area-pointer))
   fd request (sb-sys:int-sap (cffi:pointer-address pointer))))
