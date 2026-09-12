;;;; src/util/foreign.lisp -- where libcathode.dylib is found.
;;;;
;;;; In a BUNDLE this is already handled: asdf-macos-app copies every dylib CFFI
;;;; has open into Contents/Frameworks, rewrites their own dependencies to
;;;; @loader_path, and pushes that directory onto CFFI:*FOREIGN-LIBRARY-DIRECTORIES*
;;;; in the dumped image.  From a CHECKOUT nothing has done that, so this does.
;;;;
;;;; It lives in the system rather than in the Makefile because everything that
;;;; loads the system needs it -- the test suite, the REPL, a script, the
;;;; application -- and a Makefile can only help the things it launches.  It
;;;; also cannot be done from a Makefile --eval before the system is loaded,
;;;; since CFFI does not exist yet at that point.

(in-package #:cathode-ray-tube.util)

(defun vendor-library-directory ()
  "vendor/lib/ in a source checkout, or NIL when there isn't one."
  (let ((directory (ignore-errors
                    (asdf:system-relative-pathname :cathode-ray-tube "vendor/lib/"))))
    (when (and directory (probe-file directory))
      (truename directory))))

(defun register-vendor-library-directory ()
  "Make libcathode.dylib findable when running from a checkout.

PUSHNEW, and by TRUENAME, so that repeated loads do not grow the list."
  (let ((directory (vendor-library-directory)))
    (when directory
      (pushnew directory cffi:*foreign-library-directories* :test #'equal))
    directory))

(register-vendor-library-directory)

;;; A dumped image has to do it again: the list is rebuilt at startup, and a
;;; bundle's Frameworks directory is not where this checkout's vendor/lib was.
(uiop:register-image-restore-hook 'register-vendor-library-directory nil)
