;;;; src/metal/library.lisp -- the shader library, and specialised pipelines.
;;;;
;;;; cool-retro-term ships 56 precompiled .qsb files: its two big fragment
;;;; shaders are ubershaders, baked once per combination of four #ifdefs, and
;;;; chosen at run time by concatenating a filename.  Metal does that job with
;;;; [[function_constant(n)]]: one source, specialised when the function is
;;;; created, constant-folded and dead-stripped by the compiler.  The 56 files
;;;; become five programs and one table of booleans, and the variant set stops
;;;; being a build artifact.
;;;;
;;;; Specialising costs a compile -- tens of milliseconds -- so pipelines are
;;;; memoised on everything that distinguishes them.  A profile switch pays once.

(in-package #:cathode-ray-tube.metal)

(defvar *library* nil)

(defun error-message (error-pointer)
  (if (cffi:null-pointer-p error-pointer)
      "no reason given"
      (objc:invoke-into 'string error-pointer "localizedDescription")))

(defun compile-library (source)
  (let ((device (or (default-device) (error "No Metal device on this machine."))))
    ;; NSError**: allocate a slot, null it, pass it, and read it back only when
    ;; the result is null.  objc/examples/metal.lisp's BUILD-PIPELINE is the
    ;; pattern; nothing about an out-parameter needs special support.
    (cffi:with-foreign-object (error-out :pointer)
      (setf (cffi:mem-ref error-out :pointer) (cffi:null-pointer))
      (let ((library (objc:invoke device "newLibraryWithSource:options:error:"
                                  source (cffi:null-pointer) error-out)))
        (when (null-object-p library)
          (error "Metal could not compile the shader library:~%~A"
                 (error-message (cffi:mem-ref error-out :pointer))))
        library))))

(defun default-library (&key reload)
  "The compiled shader library, from res/shaders/crt.metal.

Compiled from source at startup rather than from a prebuilt .metallib.  It costs
a few tens of milliseconds once, and it means editing a shader and restarting is
the whole edit cycle -- no build step between a change and seeing it.  M5 can
ship a .metallib and fall back to this."
  (when reload (setf *library* nil))
  (or *library*
      (setf *library*
            (with-metal
              (compile-library
               (uiop:read-file-string (util:resource "shaders/crt.metal")))))))

;;; Function constants ---------------------------------------------------------

(defun constant-values (constants)
  "An MTLFunctionConstantValues from a plist of (INDEX . VALUE) pairs.

VALUE may be T/NIL for a bool or an integer for an int; those are the only two
kinds the effect shaders use.  The caller releases the result."
  (let ((values (objc:invoke (objc:invoke "MTLFunctionConstantValues" "alloc") "init")))
    (loop for (index value) on constants by #'cddr
          do (etypecase value
               (boolean
                (cffi:with-foreign-object (b :uint8)
                  (setf (cffi:mem-ref b :uint8) (if value 1 0))
                  (objc:invoke values "setConstantValue:type:atIndex:"
                               b +data-type-bool+ index)))
               (integer
                (cffi:with-foreign-object (b :int32)
                  (setf (cffi:mem-ref b :int32) value)
                  (objc:invoke values "setConstantValue:type:atIndex:"
                               b +data-type-int+ index)))))
    values))

(defun make-function (library name &optional constants)
  (cffi:with-foreign-object (error-out :pointer)
    (setf (cffi:mem-ref error-out :pointer) (cffi:null-pointer))
    (let* ((values (when constants (constant-values constants)))
           (function (if values
                         (objc:invoke library "newFunctionWithName:constantValues:error:"
                                      name values error-out)
                         (objc:invoke library "newFunctionWithName:" name))))
      (when values (objc:release values))
      (when (null-object-p function)
        (error "No function ~S in the shader library~@[: ~A~]." name
               (when constants (error-message (cffi:mem-ref error-out :pointer)))))
      function)))

;;; Pipelines ------------------------------------------------------------------

(defvar *pipelines* (make-hash-table :test 'equal)
  "(VERTEX FRAGMENT CONSTANTS PIXEL-FORMAT BLENDING) -> MTLRenderPipelineState.")

(defun clear-pipeline-cache ()
  (clrhash *pipelines*))

(defun pipeline (&key (vertex "fullscreen_vertex") fragment constants
                      (pixel-format +pixel-format-rgba8unorm+) blending label)
  "A render pipeline state, specialised by CONSTANTS and memoised on everything
that distinguishes it.

CONSTANTS is a plist of function-constant index to value:

    (pipeline :fragment \"dynamic_fragment\"
              :constants (list +k-raster-mode+ 2 +k-burn-in+ t))"
  (let ((key (list vertex fragment constants pixel-format blending)))
    (or (gethash key *pipelines*)
        (setf (gethash key *pipelines*)
              (with-metal
                (let* ((library (default-library))
                       (vfn (make-function library vertex constants))
                       (ffn (make-function library fragment constants))
                       (descriptor (objc:invoke
                                    (objc:invoke "MTLRenderPipelineDescriptor" "alloc")
                                    "init")))
                  (objc:invoke descriptor "setVertexFunction:" vfn)
                  (objc:invoke descriptor "setFragmentFunction:" ffn)
                  (when label (objc:invoke descriptor "setLabel:" label))
                  (let ((attachment (objc:invoke (objc:invoke descriptor "colorAttachments")
                                                 "objectAtIndexedSubscript:" 0)))
                    (objc:invoke attachment "setPixelFormat:" pixel-format)
                    (when blending
                      (objc:invoke attachment "setBlendingEnabled:" t)))
                  (cffi:with-foreign-object (error-out :pointer)
                    (setf (cffi:mem-ref error-out :pointer) (cffi:null-pointer))
                    (let ((state (objc:invoke
                                  (default-device)
                                  "newRenderPipelineStateWithDescriptor:error:"
                                  descriptor error-out)))
                      (objc:release descriptor)
                      (objc:release vfn)
                      (objc:release ffn)
                      (when (null-object-p state)
                        (error "Could not build the ~A pipeline:~%~A"
                               (or label fragment)
                               (error-message (cffi:mem-ref error-out :pointer))))
                      state))))))))
