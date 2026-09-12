;;;; tests/color-tests.lisp

(in-package #:cathode-ray-tube/tests)
(in-suite color)

(defun ~= (a b &optional (tol 1d-9)) (< (abs (- a b)) tol))

(test str-to-color-divides-by-256
  "utils.js divides each hex byte by 256, not 255.

Not a bug being carried over uncritically: it is what every one of the fourteen
profiles was authored against.  #ffffff is 0.99609375, and a port that
'corrects' it shifts every profile uniformly brighter by one part in 256 --
invisible alone, obvious side by side with the original."
  (let ((white (util:str-to-color "#ffffff")))
    (is (~= 255/256 (util:rgba-r white)))
    (is (~= 255/256 (util:rgba-g white)))
    (is (~= 255/256 (util:rgba-b white)))
    (is (not (~= 1d0 (util:rgba-r white))) "and it is NOT 1.0"))
  (let ((black (util:str-to-color "#000000")))
    (is (~= 0d0 (util:rgba-r black)))
    (is (~= 1d0 (util:rgba-a black))))
  ;; Default Amber's font colour, the one most of these profiles are judged by.
  (let ((amber (util:str-to-color "#ff8100")))
    (is (~= (/ 255d0 256) (util:rgba-r amber)))
    (is (~= (/ 129d0 256) (util:rgba-g amber)))
    (is (~= 0d0 (util:rgba-b amber)))))

(test color-round-trip
  (dolist (s '("#000000" "#ffffff" "#ff8100" "#0ccc68" "#7fb4ff" "#3b3b8f"
               "#a9a7ff" "#4dff6b" "#8ed6ff" "#c0c0c0" "#52f7ff" "#f2f2ec"))
    (is (string-equal s (util:color-to-str (util:str-to-color s)))
        "~A did not survive the round trip" s)))

(test mix-color
  (let ((a (util:make-rgba 0d0 0d0 0d0 1d0))
        (b (util:make-rgba 1d0 1d0 1d0 1d0)))
    (is (~= 0d0 (util:rgba-r (util:mix-color a b 0d0))))
    (is (~= 1d0 (util:rgba-r (util:mix-color a b 1d0))))
    (is (~= 0.5d0 (util:rgba-r (util:mix-color a b 0.5d0))))))

(test rgb2grey-uses-the-shaders-weights
  "0.21/0.72/0.04, which is what terminal_dynamic.frag says -- not Rec. 709."
  (is (~= 0.21d0 (util:rgb2grey (util:make-rgba 1d0 0d0 0d0))))
  (is (~= 0.72d0 (util:rgb2grey (util:make-rgba 0d0 1d0 0d0))))
  (is (~= 0.04d0 (util:rgb2grey (util:make-rgba 0d0 0d0 1d0))))
  (is (~= 0.97d0 (util:rgb2grey (util:make-rgba 1d0 1d0 1d0)))
      "they sum to 0.97, not 1.0 -- upstream's rounding, preserved"))

(test scale-and-sum
  (let ((c (util:make-rgba 0.5d0 0.5d0 0.5d0 1d0)))
    (is (~= 1d0 (util:rgba-r (util:scale-color c 4d0))) "clamps at 1")
    (is (~= 0.25d0 (util:rgba-r (util:scale-color c 0.5d0))))
    (is (~= 1d0 (util:rgba-a (util:scale-color c 0.5d0))) "alpha is left alone"))
  (is (~= 1d0 (util:rgba-r (util:sum-color (util:make-rgba 0.8d0 0d0 0d0)
                                           (util:make-rgba 0.8d0 0d0 0d0))))))
