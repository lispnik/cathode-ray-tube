;;;; src/metal/pass.lisp -- encoding one render pass.
;;;;
;;;; The whole graph is fullscreen quads over offscreen targets, so this is
;;;; deliberately small: a pass, a pipeline, some textures, a uniform block, a
;;;; draw.  Two decisions worth stating.
;;;;
;;;; NO VERTEX BUFFER.  The quad is synthesised in the vertex shader from
;;;; [[vertex_id]], which removes buffer allocation, binding and lifetime from
;;;; every pass in the program.
;;;;
;;;; NO COMPLETION HANDLERS.  A frame ends with -commit and -waitUntilCompleted
;;;; on the calling thread.  A block would put Lisp on a libdispatch worker,
;;;; which on a stock SBCL is the one thing that takes the image down without a
;;;; condition or a backtrace: a collection stops the world by signalling every
;;;; other thread, and Darwin refuses to signal a libdispatch worker at all.
;;;; The cost is some CPU/GPU overlap; the benefit is that burn-in's ping-pong
;;;; ordering is correct by construction and there is no fencing to get wrong.

(in-package #:cathode-ray-tube.metal)

(defun command-buffer ()
  (objc:invoke (command-queue) "commandBuffer"))

(defmacro with-clear-color ((var r g b a) &body body)
  "MTLClearColor as a filled buffer.

Four doubles, 32 bytes: a homogeneous float aggregate that the ABI passes in
v0-v3, exactly as it passes an NSRect.  It still cannot use the #(...)
shorthand, because the runtime reports it as an ANONYMOUS structure and the
bridge converts only the four it knows by name."
  `(cffi:with-foreign-object (,var :double 4)
     (setf (cffi:mem-aref ,var :double 0) (float ,r 1d0)
           (cffi:mem-aref ,var :double 1) (float ,g 1d0)
           (cffi:mem-aref ,var :double 2) (float ,b 1d0)
           (cffi:mem-aref ,var :double 3) (float ,a 1d0))
     ,@body))

(defmacro with-viewport ((var x y width height) &body body)
  "MTLViewport: originX, originY, width, height, znear, zfar.  48 bytes."
  `(cffi:with-foreign-object (,var :double 6)
     (setf (cffi:mem-aref ,var :double 0) (float ,x 1d0)
           (cffi:mem-aref ,var :double 1) (float ,y 1d0)
           (cffi:mem-aref ,var :double 2) (float ,width 1d0)
           (cffi:mem-aref ,var :double 3) (float ,height 1d0)
           (cffi:mem-aref ,var :double 4) 0d0
           (cffi:mem-aref ,var :double 5) 1d0)
     ,@body))

(defun render-pass-descriptor (target &key (load +load-action-clear+)
                                           (store +store-action-store+)
                                           (clear '(0d0 0d0 0d0 1d0)))
  "A single-colour-attachment pass writing into TARGET.

TARGET is a TEXTURE or a raw drawable texture handle, so the same code path
serves an offscreen pass and the one that reaches the screen."
  (let* ((descriptor (objc:invoke "MTLRenderPassDescriptor" "renderPassDescriptor"))
         (attachment (objc:invoke (objc:invoke descriptor "colorAttachments")
                                  "objectAtIndexedSubscript:" 0)))
    (objc:invoke attachment "setTexture:"
                 (if (texture-p target) (texture-handle target) target))
    (objc:invoke attachment "setLoadAction:" load)
    (objc:invoke attachment "setStoreAction:" store)
    (when (= load +load-action-clear+)
      (destructuring-bind (r g b a) clear
        (with-clear-color (cc r g b a)
          (objc:invoke attachment "setClearColor:" cc))))
    descriptor))

(defmacro with-render-pass ((encoder target &key (buffer '(command-buffer))
                                                 (load '+load-action-clear+)
                                                 (clear ''(0d0 0d0 0d0 1d0))
                                                 viewport label)
                            &body body)
  "Encode a pass into TARGET, binding ENCODER for BODY.

The command buffer is committed and WAITED ON when BODY returns -- see the
header for why there is no completion handler.  Pass :BUFFER to encode several
passes into one buffer, in which case the caller commits."
  (let ((cb (gensym "CB")) (own (gensym "OWN")) (tgt (gensym "TARGET")))
    `(with-metal
       (let* ((,tgt ,target)
              (,own ,(if (eq buffer :caller) nil t))
              (,cb ,buffer)
              (,encoder (objc:invoke ,cb "renderCommandEncoderWithDescriptor:"
                                     (render-pass-descriptor ,tgt :load ,load
                                                                  :clear ,clear))))
         ;; Declared here so a pass that only CLEARS -- which is a real thing a
         ;; render graph does -- does not have to open its body with a
         ;; declaration, and cannot, since the body is spliced into a PROGN.
         (declare (ignorable ,encoder))
         ,@(when label `((objc:invoke ,encoder "setLabel:" ,label)))
         ,(if viewport
              `(destructuring-bind (vx vy vw vh) ,viewport
                 (with-viewport (vp vx vy vw vh)
                   (objc:invoke ,encoder "setViewport:" vp)))
              `(when (texture-p ,tgt)
                 (with-viewport (vp 0 0 (texture-width ,tgt) (texture-height ,tgt))
                   (objc:invoke ,encoder "setViewport:" vp))))
         (unwind-protect (progn ,@body)
           (objc:invoke ,encoder "endEncoding"))
         (when ,own
           (objc:invoke ,cb "commit")
           (objc:invoke ,cb "waitUntilCompleted"))
         ,cb))))

;;; Binding --------------------------------------------------------------------

(defun use-pipeline (encoder pipeline)
  (objc:invoke encoder "setRenderPipelineState:" pipeline))

(defun bind-fragment-texture (encoder texture index)
  (objc:invoke encoder "setFragmentTexture:atIndex:"
               (if (texture-p texture) (texture-handle texture) texture) index))

(defun bind-fragment-sampler (encoder sampler index)
  (objc:invoke encoder "setFragmentSamplerState:atIndex:" sampler index))

(defun bind-fragment-bytes (encoder pointer length index)
  "Uniforms straight from memory, no MTLBuffer.

setFragmentBytes: takes anything up to 4 KiB, and every uniform block in this
program is well under that -- so there is no buffer to allocate, no triple
buffering, and no frame-to-frame synchronisation to get wrong."
  (objc:invoke encoder "setFragmentBytes:length:atIndex:" pointer length index))

(defun bind-vertex-bytes (encoder pointer length index)
  (objc:invoke encoder "setVertexBytes:length:atIndex:" pointer length index))

(defun draw-quad (encoder)
  "The fullscreen quad: four vertices, a triangle strip, no vertex buffer."
  (objc:invoke encoder "drawPrimitives:vertexStart:vertexCount:instanceCount:"
               +primitive-triangle-strip+ 0 4 1))

(defun draw-instances (encoder count)
  "COUNT instances of the unit quad -- the cell grid, one instance per cell."
  (when (plusp count)
    (objc:invoke encoder "drawPrimitives:vertexStart:vertexCount:instanceCount:"
                 +primitive-triangle-strip+ 0 4 count)))
