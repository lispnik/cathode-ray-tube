;;;; tests/metal-tests.lisp -- Tier 2: renders offscreen and asserts on pixels.
;;;;
;;;; The discipline here is the one objc/examples/shader.lisp's TEST-SHADER
;;;; follows: assert that a SPECIFIC PIXEL has a SPECIFIC VALUE, not that an
;;;; image was produced.  A pass that ran on transposed coordinates, or a row
;;;; out, or with its texture bound to the wrong index, still produces an image.

(in-package #:cathode-ray-tube/tests)
(in-suite metal)

(defun near (a b &optional (tol 2))
  (<= (abs (- a b)) tol))

(test metal-constants-match-the-sdk
  "The generated table still agrees with the headers on this machine.

Skips where Xcode is absent.  Apple has never renumbered one of these; the point
is that a typo in a hand-copied table does not raise an error -- it gets you a
pipeline that builds, a texture that allocates, and a black window."
  (let ((script (asdf:system-relative-pathname
                 :cathode-ray-tube "tools/metal-constants.sh")))
    (if (not (and (probe-file script)
                  (zerop (nth-value 2 (uiop:run-program '("xcrun" "--show-sdk-path")
                                                        :ignore-error-status t
                                                        :output nil :error-output nil)))))
        (skip "no Xcode command line tools, so no headers to compare against")
        ;; Compared as (NAME-STRING . VALUE): the script's output is READ into
        ;; whatever package this file is in, and the table was interned into
        ;; CRT.METAL, so the symbols are never EQ and comparing them directly
        ;; fails on a table that is perfectly correct.
        (flet ((normalise (table)
                 (sort (mapcar (lambda (pair)
                                 (cons (string (car pair)) (cdr pair)))
                               table)
                       #'string< :key #'car)))
          (let ((fresh (with-input-from-string
                           (s (uiop:run-program (list (namestring script))
                                                :output :string))
                         (read s))))
            (is (equal (normalise fresh) (normalise crt.metal:*metal-constants*))
                "src/metal/constants.lisp disagrees with the SDK.~%~
                 Regenerate it with `make constants'.~%~
                 SDK says: ~S~%file says: ~S"
                (normalise fresh) (normalise crt.metal:*metal-constants*)))))))

(test device-and-library
  (when (gpu-or-skip)
    (is (stringp (crt.metal:device-name)))
    (is (not (crt.metal:null-object-p (crt.metal:command-queue))))
    ;; Compiling res/shaders/crt.metal is a real check: a syntax error in the
    ;; shader is otherwise only discovered by opening a window.
    (finishes (crt.metal:default-library :reload t))))

(test offscreen-render-and-readback
  "Draw the gradient into a texture and check individual pixels.

uv.x runs 0..1 left to right and uv.y 0..1 BOTTOM to top, as in GLSL; Metal's
texture origin is top-left, so row 0 of the readback is uv.y = 1.  Hence 'top'
here is the high-green end."
  (when (gpu-or-skip)
    (let* ((width 64) (height 32)
           (target (crt.metal:make-texture :width width :height height
                                       :label "test-target"))
           (pipeline (crt.metal:pipeline :fragment "gradient_fragment"
                                     :constants (list 3 t) ; CRT_CHROMA
                                     :label "test-gradient")))
      (cffi:with-foreign-object (uniforms :float 2)
        (setf (cffi:mem-aref uniforms :float 0) 0.0   ; time
              (cffi:mem-aref uniforms :float 1) 2.0)  ; aspect
        (crt.metal:with-render-pass (encoder target :clear '(0d0 0d0 0d0 1d0))
          (crt.metal:use-pipeline encoder pipeline)
          (crt.metal:bind-fragment-bytes encoder uniforms 8 0)
          (crt.metal:draw-quad encoder)))
      (let* ((pixels (crt.metal:texture-bytes target))
             (top-left (crt.metal:texture-pixel pixels target 1 1))
             (top-right (crt.metal:texture-pixel pixels target (- width 2) 1))
             (bottom-left (crt.metal:texture-pixel pixels target 1 (- height 2))))
        (is (= (* width height 4) (length pixels)))
        (is (near (first top-left) 0 8)
            "red tracks uv.x: left edge should be ~0, got ~S" top-left)
        (is (near (first top-right) 255 8)
            "red tracks uv.x: right edge should be ~255, got ~S" top-right)
        (is (> (second top-left) (second bottom-left))
            "green tracks uv.y: row 0 is uv.y=1, so it should exceed the last row~%~
             top ~S bottom ~S" top-left bottom-left)
        (is (= 255 (fourth top-left)) "alpha is opaque"))
      (crt.metal:release-texture target))))

(test function-constants-really-specialise
  "Same source, constant unset, must be flat blue.

If this fails the whole scheme is an illusion: the 56 baked .qsb variants
upstream ships would be replaced by one shader that branches at run time, which
is slower and -- much worse -- would mean the constants are not doing what the
rest of the graph assumes."
  (when (gpu-or-skip)
    (let* ((target (crt.metal:make-texture :width 16 :height 16))
           (pipeline (crt.metal:pipeline :fragment "gradient_fragment"
                                     :constants (list 3 nil)
                                     :label "test-unspecialised")))
      (cffi:with-foreign-object (uniforms :float 2)
        (setf (cffi:mem-aref uniforms :float 0) 0.0
              (cffi:mem-aref uniforms :float 1) 1.0)
        (crt.metal:with-render-pass (encoder target)
          (crt.metal:use-pipeline encoder pipeline)
          (crt.metal:bind-fragment-bytes encoder uniforms 8 0)
          (crt.metal:draw-quad encoder)))
      (let ((centre (crt.metal:texture-pixel (crt.metal:texture-bytes target) target 8 8)))
        (is (and (near (first centre) 0) (near (second centre) 0)
                 (near (third centre) 255))
            "expected flat blue, got ~S" centre))
      (crt.metal:release-texture target))))

(test clear-color-crosses-by-value
  "MTLClearColor is four doubles -- a 32-byte homogeneous float aggregate the
ABI passes in v0-v3 -- and still cannot use the #(...) shorthand, because the
runtime reports it as an ANONYMOUS structure and the bridge converts only the
four Cocoa ones by name.  This asserts the buffer path works."
  (when (gpu-or-skip)
    (let ((target (crt.metal:make-texture :width 8 :height 8)))
      ;; A pass that only clears: no pipeline, no draw.  The clear colour IS
      ;; the thing under test.
      (crt.metal:with-render-pass (encoder target :clear '(0.25d0 0.5d0 0.75d0 1d0)))
      (let ((p (crt.metal:texture-pixel (crt.metal:texture-bytes target) target 4 4)))
        (is (and (near (first p) 64) (near (second p) 128)
                 (near (third p) 191) (= 255 (fourth p)))
            "cleared to ~S, wanted (64 128 191 255)" p))
      (crt.metal:release-texture target))))

(test pipelines-are-memoised
  "A profile switch pays one compile; a frame pays none."
  (when (gpu-or-skip)
    (crt.metal:clear-pipeline-cache)
    (let ((a (crt.metal:pipeline :fragment "gradient_fragment" :constants (list 3 t)))
          (b (crt.metal:pipeline :fragment "gradient_fragment" :constants (list 3 t)))
          (c (crt.metal:pipeline :fragment "gradient_fragment" :constants (list 3 nil))))
      (is (eq a b) "the same key must return the same pipeline object")
      (is (not (eq a c)) "a different constant must build a different pipeline"))))
