;;;; tests/math-tests.lisp

(in-package #:cathode-ray-tube/tests)
(in-suite math)

(test clamp
  (is (= 5 (crt.util:clamp 5 0 10)))
  (is (= 0 (crt.util:clamp -1 0 10)))
  (is (= 10 (crt.util:clamp 11 0 10)))
  (is (= 0 (crt.util:clamp 0 0 10)))
  (is (= 10 (crt.util:clamp 10 0 10))))

(test lint
  (is (= 0 (crt.util:lint 0 10 0)))
  (is (= 10 (crt.util:lint 0 10 1)))
  (is (= 5 (crt.util:lint 0 10 1/2)))
  ;; The derivations that actually use it, from ApplicationSettings.qml.
  (is (= 0.16d0 (crt.util:lint 0.16d0 1.6d0 0d0)))          ; burnIn = 0
  (is (< 0.51d0 (crt.util:lint 0.16d0 1.6d0 0.25d0) 0.53d0)) ; burnIn = 0.25 default
  (is (= 4d0 (crt.util:lint 4d0 120d0 0d0)))                 ; screenRadius = 0
  (is (= 27.2d0 (crt.util:lint 4d0 120d0 0.2d0))))           ; _screenRadius = 0.2

(test smoothstep
  (is (= 0 (crt.util:smoothstep 2 4 1)))
  (is (= 0 (crt.util:smoothstep 2 4 2)))
  (is (= 1 (crt.util:smoothstep 2 4 4)))
  (is (= 1 (crt.util:smoothstep 2 4 9)))
  (is (= 1/2 (crt.util:smoothstep 2 4 3)))
  ;; Degenerate range must not divide by zero.
  (finishes (crt.util:smoothstep 2 2 3)))

(test qround-is-not-cl-round
  "Qt rounds half away from zero; Common Lisp rounds half to even.

This is the whole reason QROUND exists.  fontmanager.cpp uses qRound for
lineSpacing and nativeLineHeight, both of which land on a half-integer whenever
lineSpacing is 0.1 and the pixel height is odd -- so CL:ROUND would put every
glyph in the window a pixel out of place, on some settings and not others."
  (is (= 3 (crt.util:qround 2.5d0)))
  (is (= 2 (round 2.5d0)) "CL:ROUND rounds half to even -- the trap this avoids")
  (is (= 4 (crt.util:qround 3.5d0)))
  (is (= 4 (round 3.5d0)))
  (is (= 3 (crt.util:qround 3.2d0)))
  (is (= 3 (crt.util:qround 2.6d0)))
  (is (= -3 (crt.util:qround -2.5d0)) "away from zero on the negative side too")
  (is (= 0 (crt.util:qround 0.4d0)))
  (is (= 1 (crt.util:qround 0.5d0))))
