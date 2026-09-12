;;;; src/util/color.lisp -- colours, and the /256 that has to be preserved.

(in-package #:cathode-ray-tube.util)

(defstruct (rgba (:constructor make-rgba (r g b &optional (a 1.0d0))))
  "A colour in [0,1] per channel, as cool-retro-term's QML `color' behaves."
  (r 0d0 :type double-float)
  (g 0d0 :type double-float)
  (b 0d0 :type double-float)
  (a 1d0 :type double-float))

(defun str-to-color (s)
  "\"#rrggbb\" as an RGBA, dividing each byte by **256**.

Two hundred and fifty-SIX, not 255, and this is not a bug being carried over
uncritically -- it is utils.js `strToColor', it is what every one of the fourteen
profiles was authored against, and it is what their colours mean.  \"#ffffff\"
is 0.99609375 here, not 1.0.  Correcting it would shift every profile by up to
one part in 256, uniformly, in the direction of brighter -- which is exactly the
kind of drift that is invisible in isolation and obvious side by side."
  (flet ((byte-at (i) (/ (parse-integer s :start i :end (+ i 2) :radix 16) 256d0)))
    (make-rgba (byte-at 1) (byte-at 3) (byte-at 5))))

(defun color-to-str (c)
  "An RGBA back to \"#rrggbb\", inverting STR-TO-COLOR.

Multiplies by 256 and clamps to 255, which round-trips every value STR-TO-COLOR
can produce."
  (flet ((byte-of (v) (min 255 (max 0 (floor (* v 256))))))
    (format nil "#~2,'0x~2,'0x~2,'0x"
            (byte-of (rgba-r c)) (byte-of (rgba-g c)) (byte-of (rgba-b c)))))

(defun mix-color (c1 c2 alpha)
  "C1 at ALPHA=0, C2 at ALPHA=1, per channel.  utils.js `mix'."
  (flet ((m (a b) (+ (* a (- 1 alpha)) (* b alpha))))
    (make-rgba (m (rgba-r c1) (rgba-r c2))
               (m (rgba-g c1) (rgba-g c2))
               (m (rgba-b c1) (rgba-b c2))
               (m (rgba-a c1) (rgba-a c2)))))

(defun sum-color (c1 c2)
  "Channel-wise addition, clamped.  utils.js `sum'."
  (flet ((s (a b) (clamp (+ a b) 0d0 1d0)))
    (make-rgba (s (rgba-r c1) (rgba-r c2))
               (s (rgba-g c1) (rgba-g c2))
               (s (rgba-b c1) (rgba-b c2))
               (s (rgba-a c1) (rgba-a c2)))))

(defun scale-color (c value)
  "C scaled by VALUE, clamped, ALPHA left alone.  utils.js `scaleColor'."
  (flet ((s (a) (clamp (* a value) 0d0 1d0)))
    (make-rgba (s (rgba-r c)) (s (rgba-g c)) (s (rgba-b c))
               (clamp (rgba-a c) 0d0 1d0))))

(defun rgb2grey (c)
  "Luminance, with the weights the shaders use: dot(v, vec3(0.21, 0.72, 0.04)).

Not Rec. 709 (0.2126, 0.7152, 0.0722) and not Rec. 601 -- these are the rounded
numbers in terminal_dynamic.frag and burn_in.frag, and the burn-in mask compares
two of these against each other, so the rounding has to match."
  (+ (* 0.21d0 (rgba-r c)) (* 0.72d0 (rgba-g c)) (* 0.04d0 (rgba-b c))))
