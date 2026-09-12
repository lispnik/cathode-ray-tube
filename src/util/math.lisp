;;;; src/util/math.lisp -- the arithmetic cool-retro-term does, reproduced exactly.
;;;;
;;;; These are ports of app/qml/utils.js and of the derivations in
;;;; app/fontmanager.cpp.  "Exactly" is the whole point: every one of them feeds
;;;; a shader uniform or a glyph position, and a port that is merely close is a
;;;; port that looks subtly wrong in a way nobody can localise.  Where the
;;;; original is surprising, the surprise is preserved and commented rather than
;;;; corrected.

(in-package #:cathode-ray-tube.util)

(declaim (inline clamp lint smoothstep))

(defun clamp (x min max)
  "X confined to [MIN, MAX].  utils.js `clamp'."
  (cond ((<= x min) min)
        ((>= x max) max)
        (t x)))

(defun lint (a b tt)
  "Linear interpolation: A at TT=0, B at TT=1.  utils.js `lint'.

Written as `(1-t)a + tb' rather than `a + t(b-a)' because that is what the
original computes, and the two differ in the last bits."
  (+ (* (- 1 tt) a) (* tt b)))

(defun smoothstep (min max value)
  "The Hermite ramp, clamped.  utils.js `smoothstep'.

Drives `rasterizationIntensity' from the oversampling ratio, which is what fades
scanlines out on a display too coarse to show them without moire."
  (let ((x (max 0 (min 1 (if (= max min) 0 (/ (- value min) (- max min)))))))
    (* x x (- 3 (* 2 x)))))

(defun qround (x)
  "Qt's `qRound': round half AWAY FROM ZERO.

NOT `cl:round', which rounds half to EVEN -- (round 2.5) is 2 and qRound(2.5) is
3.  fontmanager.cpp uses qRound for `lineSpacing' and for `nativeLineHeight',
both of which land on a half-integer whenever lineSpacing is 0.1 and the pixel
height is odd, so using the wrong one puts every glyph in the window a pixel out
of place.  This is the single most boring bug in the project and would have been
among the most annoying to find."
  (if (minusp x)
      (- (floor (+ (- x) 1/2)))
      (floor (+ x 1/2))))
