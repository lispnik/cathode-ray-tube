;;;; src/pty/errno.lisp -- errno, and the three values worth telling apart.
;;;;
;;;; GENERATED table below, by tools/errno-constants.sh.  Regenerate with
;;;; `make errno-constants'; `make check-errno-constants' re-reads the SDK and
;;;; asserts nothing moved, skipping where Xcode is absent.
;;;;
;;;; Why generate three small integers that have not changed since 4.2BSD?  The
;;;; same reason tools/metal-constants.sh exists: a number typed from memory is
;;;; a number nobody can check.  EINTR being 4 is right or wrong and nothing
;;;; about the digit says which, and the consequence of wrong here is not a
;;;; crash but a reader loop that treats a real error as a retry, or a retry as
;;;; the end of a child.
;;;;
;;;; errno ITSELF is a function call on Darwin.  It is not a global: <errno.h>
;;;; defines it as (*__error()), because a thread needs its own.  Reading a
;;;; symbol called "errno" finds nothing, and CFFI can call __error on both
;;;; implementations, so this needs no seam.

(in-package #:cathode-ray-tube.pty)

(defmacro define-errno-constants (&body pairs)
  `(progn
     ,@(loop for (name . value) in pairs
             collect `(defconstant ,(intern (format nil "+~A+" name)) ,value))
     (defparameter *errno-constants* ',pairs
       "The generated table, so the test can re-read the SDK and compare.")))

(define-errno-constants
  (eintr . 4)
  (eagain . 35)
  (eio . 5))

(cffi:defcfun ("__error" %errno-location) :pointer)

(defun errno ()
  "The calling thread's errno."
  (cffi:mem-ref (%errno-location) :int))

(defun interrupted-p ()
  "True when the call that just failed was INTERRUPTED rather than broken.

EINTR, or EAGAIN on a descriptor someone has made non-blocking.  Neither says
anything about the child, and both are routine: SBCL's collector stops the world
by signalling every other thread, and a thread sitting in a blocking poll() or
read() comes back -1/EINTR when that happens.

A reader loop that treated a negative return as `the child is gone' therefore
ended whenever a collection landed on it, leaving a terminal that had died with
its child still running -- pid valid, master open, reader thread alive.  It
depends on when the collector runs, so it never happened on a desk and happened
regularly on a CI runner."
  (let ((e (errno)))
    (or (= e +eintr+) (= e +eagain+))))
