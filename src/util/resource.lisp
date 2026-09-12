;;;; src/util/resource.lisp -- where the shaders, fonts and textures live.

(in-package #:cathode-ray-tube.util)

(defvar *resource-directory* nil
  "Overrides resource lookup entirely.  Set by the tests, which run against the
source tree from wherever ASDF happens to have put them.")

(defun resource (relative)
  "RELATIVE under Contents/Resources in a bundle, under res/ in a checkout.

Every path in the program goes through here, so that nothing else has to know
whether it is running from a .app or from a git working copy.

MACOS-APP is reached by FIND-PACKAGE and UIOP:SYMBOL-CALL rather than by a
direct reference: asdf-macos-app is a build-time dependency of the bundle system
only, and the library must load without it -- a property CI asserts in its own
step, because the failure is invisible to anyone who has both installed."
  (or (when *resource-directory*
        (merge-pathnames relative (pathname *resource-directory*)))
      (let ((override (uiop:getenv "CRT_RESOURCE_DIR")))
        (when (and override (plusp (length override)))
          (merge-pathnames relative
                           (uiop:ensure-directory-pathname
                            (uiop:parse-native-namestring override)))))
      (let ((package (find-package "MACOS-APP")))
        (when (and package (uiop:symbol-call :macos-app :running-in-bundle-p))
          (uiop:symbol-call :macos-app :bundle-resource relative)))
      (asdf:system-relative-pathname
       :cathode-ray-tube (concatenate 'string "res/" relative))))
