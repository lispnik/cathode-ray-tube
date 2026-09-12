;;;; tests/package.lisp -- the suite, and how it decides what to skip.
;;;;
;;;; Three tiers, and the difference matters for what a green run MEANS.
;;;;
;;;;   Tier 1 is arithmetic and data: colours, metrics, profiles, VT behaviour.
;;;;     It needs no GPU and no window server, and it runs on ECL as well as
;;;;     SBCL.  Most of the fidelity of this port lives here.
;;;;
;;;;   Tier 2 needs a Metal device.  It renders offscreen and asserts on
;;;;     individual pixels.  Where there is no GPU it SKIPS -- a virtualised
;;;;     runner without one is a fact about the machine, not a failure of the
;;;;     code, and that is the discipline objc's own suite follows.
;;;;
;;;;   Tier 3 needs a window server.  GitHub's macOS runners have one.
;;;;
;;;; The skip counts are printed, so a runner that quietly loses its GPU shows
;;;; up as fewer checks rather than as a mystery.

(defpackage #:cathode-ray-tube/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:util #:cathode-ray-tube.util)
                    (#:metal #:cathode-ray-tube.metal)
                    (#:vt #:cathode-ray-tube.vt)
                    (#:ui #:cathode-ray-tube.ui))
  (:export #:run-tests #:all))

(in-package #:cathode-ray-tube/tests)

(def-suite all :description "Everything.")

(def-suite math :in all :description "utils.js and fontmanager.cpp arithmetic.")
(def-suite color :in all :description "Colour, including the /256.")
(def-suite vt :in all :description "The terminal core. No GPU, no window.")
(def-suite metal :in all :description "Metal. Skips without a GPU.")
(def-suite ui :in all :description "Windows. Skips without a window server.")

(defun gpu-or-skip ()
  "True when there is a Metal device; otherwise SKIP and return NIL."
  (if (metal:metal-available-p)
      t
      (progn (skip "no Metal device on this machine") nil)))

(defun run-tests ()
  "Run everything and return T when nothing FAILED.  Skips are not failures.

Returns a BOOLEAN, which FIVEAM:RUN! does not -- it prints and answers NIL --
and ASDF discards what a TEST-OP returns.  Between the two, a suite with failing
tests goes green unless the exit status comes from here.

The skip count is PRINTED, and that is the point of printing it: a runner that
quietly loses its GPU would otherwise look identical to one that ran everything,
and the difference would only surface as a bug reaching a release."
  (let ((results (run 'all)))
    (explain! results)
    (multiple-value-bind (ok failed skipped) (results-status results)
      (format t "~&~%~D check~:P: ~D failed, ~D skipped.~%"
              (length results) (length failed) (length skipped))
      ;; The names and reasons are not dug out of the result objects: those
      ;; accessors are FiveAM internals, and EXPLAIN! above has already printed
      ;; both.  The count is here because it is the number a CI log should be
      ;; grepped for.
      ok)))
