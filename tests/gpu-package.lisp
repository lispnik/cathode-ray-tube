;;;; tests/gpu-package.lisp -- the suites that need hardware.
;;;;
;;;; Loaded only by #:cathode-ray-tube/tests, never by the portable system, so
;;;; that the ECL leg does not have to know these exist.

(in-package #:cathode-ray-tube/tests)

(def-suite metal :in all :description "Metal. Skips without a GPU.")
(def-suite text :in all :description "Glyphs. Skips without a GPU.")
(def-suite effects :in all :description "The CRT chain. Skips without a GPU.")
(def-suite ui :in all :description "Windows. Skips without a window server.")

(defun gpu-or-skip ()
  "True when there is a Metal device; otherwise SKIP and return NIL.

A virtualised runner may have no GPU at all, and that is a fact about the
machine rather than about this code."
  (if (crt.metal:metal-available-p)
      t
      (progn (skip "no Metal device on this machine") nil)))
