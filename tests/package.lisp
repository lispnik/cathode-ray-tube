;;;; tests/package.lisp -- the suites, and what a green run actually means.
;;;;
;;;; Three tiers, and the difference matters.
;;;;
;;;;   TIER 1 is arithmetic and data: colours, the fourteen profiles, the
;;;;     terminal core, the pty.  It needs no GPU and no window server, it runs
;;;;     on ECL as well as SBCL, and most of the FIDELITY of this port lives
;;;;     here -- the numbers, not the pixels.
;;;;
;;;;   TIER 2 needs a Metal device.  It renders offscreen and asserts on
;;;;     individual pixels.  Where there is no GPU it SKIPS: a virtualised
;;;;     runner without one is a fact about the machine, not a failure of the
;;;;     code, and that is the discipline objc's own suite follows.
;;;;
;;;;   TIER 3 needs a window server.  GitHub's macOS runners have one.
;;;;
;;;; Tier 1 is #:cathode-ray-tube/portable-tests and is all the ECL leg runs.
;;;; The skip count is PRINTED, so a runner that quietly loses its GPU looks
;;;; different from one that ran everything -- otherwise the two are
;;;; indistinguishable in a log, and the difference is a release.
;;;;
;;;; Package prefixes here are the GLOBAL nicknames (CRT.METAL and friends)
;;;; rather than local ones.  A local nickname belongs to the package that
;;;; declares it, and this package is defined once but used by two systems --
;;;; one of which has never heard of Metal.

(defpackage #:cathode-ray-tube/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests #:run-portable-tests #:all #:portable))

(in-package #:cathode-ray-tube/tests)

(def-suite all :description "Everything.")
(def-suite portable :in all
  :description "No GPU, no window, no Objective-C. The ECL leg.")

(def-suite math :in portable :description "utils.js and fontmanager.cpp arithmetic.")
(def-suite color :in portable :description "Colour, including the /256.")
(def-suite profile :in portable
  :description "The fourteen profiles and their derivations.")
(def-suite vt :in portable :description "The terminal core.")
(def-suite pty :in portable :description "Real child processes on real ptys.")
(def-suite cli :in portable
  :description "The command line and the settings store.")
(def-suite terminal :in portable :description "A pty and a screen, end to end.")

(defun explain-results (results label)
  (explain! results)
  (multiple-value-bind (ok failed skipped) (results-status results)
    (format t "~&~%~A: ~D check~:P, ~D failed, ~D skipped.~%"
            label (length results) (length failed) (length skipped))
    ok))

(defun run-portable-tests ()
  "Tier 1 only.  Returns T when nothing failed; skips are not failures."
  (explain-results (run 'portable) "portable"))

(defun run-tests ()
  "Everything.  Returns T when nothing failed; skips are not failures.

Returns a BOOLEAN, which FIVEAM:RUN! does not -- it prints and answers NIL --
and ASDF discards what a TEST-OP returns.  Between the two, a suite with failing
tests goes green unless the exit status comes from somewhere deliberate."
  (explain-results (run 'all) "all"))
