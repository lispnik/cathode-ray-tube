;;;; tools/probe.lisp -- M0: does the hard stuff actually work?
;;;;
;;;; No product code.  This file asks the questions that decide whether the
;;;; design is buildable, and answers them by MEASURING rather than reasoning.
;;;;
;;;;   1. Can `objc' dispatch the Metal selectors the render graph needs?  The
;;;;      risk is not the ABI -- src/abi.lisp handles homogeneous float
;;;;      aggregates and indirect returns and has CGRectInset in its tests.  The
;;;;      risk is ENCODING RESOLUTION: a selector whose runtime type names a
;;;;      structure WITHOUT its layout cannot have a trampoline built.
;;;;
;;;;   2. Does a Metal RENDER pipeline work from Lisp at all?  `objc' ships
;;;;      tested COMPUTE examples and nothing else.  So: build a pipeline
;;;;      specialised by a function constant, draw a quad into an offscreen
;;;;      texture, read the pixels back, and check them.
;;;;
;;;;   3. Does libvterm drive correctly through the shim -- including the two
;;;;      crossings CFFI cannot make unaided: a structure by value INTO a
;;;;      callback, and the variadic ioctl.
;;;;
;;;; WHAT THE FIRST RUN TAUGHT US, and it is the reason this file introspects
;;;; instances rather than class names.  `-[MTLTextureDescriptor setUsage:]'
;;;; reports NOT FOUND, and so do most of Metal's descriptor selectors, because
;;;; the public classes are abstract facades: `+alloc' hands back an instance of
;;;; a private `...Internal' SUBCLASS, and that is where the methods live.
;;;; Looking up a method on the facade searches the facade and its SUPERclasses,
;;;; never down into the subclass.  This costs `objc:invoke' nothing -- it
;;;; resolves against the receiver's real class at call time -- but it means
;;;; static introspection by name is useless here, and any future probe of a
;;;; Metal selector has to hold an object first.

(defpackage #:crt-probe
  (:use #:cl)
  (:export #:run))

(in-package #:crt-probe)

;;; `objc' compiles one sb-alien trampoline per distinct method signature, and
;;; each prints a "doing SAP to pointer coercion" note.  They are a property of
;;; the bridge, not of this file, and they bury the report.
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(defvar *failures* 0)
(defvar *checks* 0)

(defun report (ok fmt &rest args)
  (incf *checks*)
  (unless ok (incf *failures*))
  (format t "~&  [~:[FAIL~; ok ~]] ~?~%" ok fmt args)
  ok)

(defun note (fmt &rest args) (format t "~&        ~?~%" fmt args))
(defun heading (fmt &rest args)
  (format t "~&~%~?~%~v,,,'-<~>~%" fmt args 72))

(defmacro with-metal (&body body)
  "Metal violates the float traps SBCL runs with; message sends are masked by
the bridge but plain C calls are not."
  `(float-features:with-float-traps-masked (:invalid :divide-by-zero :overflow)
     ,@body))

(defun nullp (x)
  (cffi:null-pointer-p (if (cffi:pointerp x) x (objc:objc-object-pointer x))))

;;; Metal constants, read out of the SDK headers rather than remembered.
;;; tools/metal-constants.lisp will generate these for real; the handful the
;;; probe needs are inline.
(defconstant +pixel-format-rgba8unorm+ 70)
(defconstant +texture-type-2d+          2)
(defconstant +usage-shader-read+        1)
(defconstant +usage-render-target+      4)
(defconstant +storage-mode-shared+      0)
(defconstant +load-action-clear+        2)
(defconstant +store-action-store+       1)
(defconstant +primitive-triangle-strip+ 4)
(defconstant +data-type-bool+          53)

(cffi:defcfun ("MTLCreateSystemDefaultDevice" %create-device) :pointer)

;;; 0. The environment ---------------------------------------------------------

(defun probe-environment ()
  (heading "0. The environment")
  (note "~A ~A on ~A" (lisp-implementation-type) (lisp-implementation-version)
        (machine-type))
  ;; Core compression decides whether a notarised bundle runs on a Mac without
  ;; Homebrew: it links libzstd, which Apple notarises happily and dyld then
  ;; fails to find on someone else's disk.
  (report (not (find :sb-core-compression *features*))
          "this SBCL is built WITHOUT core compression")
  (let* ((runtime (namestring sb-ext:*runtime-pathname*))
         (out (with-output-to-string (s)
                (uiop:run-program (list "otool" "-L" runtime)
                                  :output s :ignore-error-status t)))
         (libs (remove-if (lambda (l) (or (zerop (length l)) (search runtime l)))
                          (mapcar (lambda (l) (string-trim '(#\Space #\Tab) l))
                                  (rest (uiop:split-string out :separator '(#\Newline))))))
         (foreign (remove-if (lambda (l) (or (eql 0 (search "/usr/lib/" l))
                                             (eql 0 (search "/System/" l))))
                             libs)))
    (note "runtime ~A" runtime)
    (dolist (l libs) (note "  ~A" l))
    (report (null foreign)
            "the runtime links nothing outside /usr/lib and /System")))

;;; 1 & 2. Metal ---------------------------------------------------------------

(defparameter +probe-shader+
  ;; A fullscreen quad synthesized from vertex_id -- no vertex buffer, which is
  ;; how every pass in the real graph will draw.  The fragment colour is chosen
  ;; by a FUNCTION CONSTANT, which is the mechanism that replaces
  ;; cool-retro-term's 56 precompiled .qsb variants, so the probe exercises it.
  "#include <metal_stdlib>
using namespace metal;

constant bool PROBE_FLAG [[function_constant(0)]];

struct VOut { float4 position [[position]]; float2 uv; };

vertex VOut probe_vertex(uint vid [[vertex_id]])
{
  // (0,0) (1,0) (0,1) (1,1) -- a triangle strip covering clip space.
  float2 uv = float2((vid & 1), (vid >> 1));
  VOut out;
  out.position = float4(uv * 2.0 - 1.0, 0.0, 1.0);
  out.uv = uv;
  return out;
}

fragment float4 probe_fragment(VOut in [[stage_in]])
{
  // With the flag set the quad is red on the left, green on the right, so a
  // readback can tell orientation from colour.  Unset, it is solid blue --
  // which is how we prove the constant actually specialised anything.
  if (PROBE_FLAG)
    return float4(in.uv.x, in.uv.y, 0.0, 1.0);
  return float4(0.0, 0.0, 1.0, 1.0);
}")

(defun compile-library (device source)
  (cffi:with-foreign-object (err :pointer)
    (setf (cffi:mem-ref err :pointer) (cffi:null-pointer))
    (let ((lib (objc:invoke device "newLibraryWithSource:options:error:"
                            source (cffi:null-pointer) err)))
      (when (nullp lib)
        (error "Metal could not compile the probe shader:~%~A"
               (let ((e (cffi:mem-ref err :pointer)))
                 (if (cffi:null-pointer-p e) "no reason given"
                     (objc:invoke-into 'string e "localizedDescription")))))
      lib)))

(defun specialised-function (library name flag)
  "NAME from LIBRARY with function constant 0 set to FLAG."
  (let ((values (objc:invoke (objc:invoke "MTLFunctionConstantValues" "alloc") "init")))
    (cffi:with-foreign-object (b :uint8)
      (setf (cffi:mem-ref b :uint8) (if flag 1 0))
      (objc:invoke values "setConstantValue:type:atIndex:" b +data-type-bool+ 0))
    (cffi:with-foreign-object (err :pointer)
      (setf (cffi:mem-ref err :pointer) (cffi:null-pointer))
      (let ((fn (objc:invoke library "newFunctionWithName:constantValues:error:"
                             name values err)))
        (objc:release values)
        (when (nullp fn)
          (error "no function ~S: ~A" name
                 (let ((e (cffi:mem-ref err :pointer)))
                   (if (cffi:null-pointer-p e) "no reason given"
                       (objc:invoke-into 'string e "localizedDescription")))))
        fn))))

(defun make-render-pipeline (device vertex fragment)
  (let ((desc (objc:invoke (objc:invoke "MTLRenderPipelineDescriptor" "alloc") "init")))
    (objc:invoke desc "setVertexFunction:" vertex)
    (objc:invoke desc "setFragmentFunction:" fragment)
    (let ((attachment (objc:invoke (objc:invoke desc "colorAttachments")
                                   "objectAtIndexedSubscript:" 0)))
      (objc:invoke attachment "setPixelFormat:" +pixel-format-rgba8unorm+))
    (cffi:with-foreign-object (err :pointer)
      (setf (cffi:mem-ref err :pointer) (cffi:null-pointer))
      (let ((pipeline (objc:invoke device "newRenderPipelineStateWithDescriptor:error:"
                                   desc err)))
        (objc:release desc)
        (when (nullp pipeline)
          (error "could not build a render pipeline: ~A"
                 (let ((e (cffi:mem-ref err :pointer)))
                   (if (cffi:null-pointer-p e) "no reason given"
                       (objc:invoke-into 'string e "localizedDescription")))))
        pipeline))))

(defun make-target (device width height)
  (let ((desc (objc:invoke "MTLTextureDescriptor"
                           "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
                           +pixel-format-rgba8unorm+ width height nil)))
    (objc:invoke desc "setUsage:" (logior +usage-render-target+ +usage-shader-read+))
    (objc:invoke desc "setStorageMode:" +storage-mode-shared+)
    (objc:invoke desc "setTextureType:" +texture-type-2d+)
    (objc:invoke device "newTextureWithDescriptor:" desc)))

(defun mtl-viewport (x y w h znear zfar)
  "Six doubles, 48 bytes -- too big for an HFA, so it goes indirectly.  A filled
buffer, like MTLSize; the #(...) shorthand is only for the four Cocoa structs."
  (let ((b (cffi:foreign-alloc :double :count 6)))
    (setf (cffi:mem-aref b :double 0) (float x 1d0)
          (cffi:mem-aref b :double 1) (float y 1d0)
          (cffi:mem-aref b :double 2) (float w 1d0)
          (cffi:mem-aref b :double 3) (float h 1d0)
          (cffi:mem-aref b :double 4) (float znear 1d0)
          (cffi:mem-aref b :double 5) (float zfar 1d0))
    b))

(defun mtl-clear-color (r g b a)
  "MTLClearColor: four doubles, 32 bytes.

A BUFFER, not the #(...) shorthand -- and that is the one thing the first run of
this probe got wrong.  The runtime reports the argument as an ANONYMOUS
structure, `(:STRUCT NIL (:DOUBLE :DOUBLE :DOUBLE :DOUBLE))', and
OBJC::MARSHAL-ARGUMENT accepts a vector only for the four structures the bridge
converts by name -- NSRect, NSPoint, NSSize, NSRange.  Anything else, however
convenient its shape, signals `Cannot pass #(...) as a structure argument.'
The ABI is not the problem: sb-alien still sees four doubles and still passes
them in v0-v3 as the homogeneous float aggregate they are."
  (let ((buf (cffi:foreign-alloc :double :count 4)))
    (setf (cffi:mem-aref buf :double 0) (float r 1d0)
          (cffi:mem-aref buf :double 1) (float g 1d0)
          (cffi:mem-aref buf :double 2) (float b 1d0)
          (cffi:mem-aref buf :double 3) (float a 1d0))
    buf))

(defun mtl-region (x y w h)
  "MTLRegion: origin and size, three NSUIntegers each, 48 bytes."
  (let ((b (cffi:foreign-alloc :uint64 :count 6)))
    (setf (cffi:mem-aref b :uint64 0) x (cffi:mem-aref b :uint64 1) y
          (cffi:mem-aref b :uint64 2) 0
          (cffi:mem-aref b :uint64 3) w (cffi:mem-aref b :uint64 4) h
          (cffi:mem-aref b :uint64 5) 1)
    b))

(defun render (device queue pipeline target width height clear)
  "Draw the quad into TARGET and return its pixels as a (W*H*4) octet vector."
  (let* ((rpd (objc:invoke "MTLRenderPassDescriptor" "renderPassDescriptor"))
         (att (objc:invoke (objc:invoke rpd "colorAttachments")
                           "objectAtIndexedSubscript:" 0)))
    (objc:invoke att "setTexture:" target)
    (objc:invoke att "setLoadAction:" +load-action-clear+)
    (objc:invoke att "setStoreAction:" +store-action-store+)
    ;; MTLClearColor: four doubles, a 32-byte homogeneous float aggregate passed
    ;; in v0-v3.  Same shape as NSRect, which src/abi.lisp measures as working.
    (let ((cc (apply #'mtl-clear-color clear)))
      (unwind-protect (objc:invoke att "setClearColor:" cc)
        (cffi:foreign-free cc)))
    (let* ((cb (objc:invoke queue "commandBuffer"))
           (enc (objc:invoke cb "renderCommandEncoderWithDescriptor:" rpd))
           (vp (mtl-viewport 0 0 width height 0d0 1d0)))
      (unwind-protect
           (progn
             (objc:invoke enc "setRenderPipelineState:" pipeline)
             (objc:invoke enc "setViewport:" vp)
             (objc:invoke enc "drawPrimitives:vertexStart:vertexCount:instanceCount:"
                          +primitive-triangle-strip+ 0 4 1)
             (objc:invoke enc "endEncoding"))
        (cffi:foreign-free vp))
      ;; No completion handler: commit and wait, on this thread.  A block would
      ;; put Lisp on a libdispatch worker, which is the one thing the threading
      ;; design forbids.
      (objc:invoke cb "commit")
      (objc:invoke cb "waitUntilCompleted")
      (let* ((n (* width height 4))
             (out (make-array n :element-type '(unsigned-byte 8)))
             (buf (cffi:foreign-alloc :uint8 :count n))
             (region (mtl-region 0 0 width height)))
        (unwind-protect
             (progn
               (objc:invoke target "getBytes:bytesPerRow:fromRegion:mipmapLevel:"
                            buf (* width 4) region 0)
               (dotimes (i n) (setf (aref out i) (cffi:mem-aref buf :uint8 i)))
               out)
          (cffi:foreign-free buf)
          (cffi:foreign-free region))))))

(defun px (pixels width x y)
  (let ((i (* 4 (+ x (* y width)))))
    (list (aref pixels i) (aref pixels (+ i 1))
          (aref pixels (+ i 2)) (aref pixels (+ i 3)))))

(defun near (a b &optional (tol 2)) (<= (abs (- a b)) tol))

(defun probe-metal ()
  (heading "1. Metal: a real render pipeline")
  (let ((device (with-metal (let ((d (%create-device)))
                              (and (not (cffi:null-pointer-p d)) d)))))
    (unless (report device "a Metal device exists")
      (note "no GPU -- the rest of this section cannot run")
      (return-from probe-metal nil))
    (note "device ~A" (with-metal (objc:invoke-into 'string device "name")))
    (with-metal
      (let* ((queue (objc:invoke device "newCommandQueue"))
             (library (compile-library device +probe-shader+))
             (width 64) (height 32))
        (report (not (nullp queue)) "-[device newCommandQueue]")
        (report (not (nullp library)) "runtime MSL compilation")

        ;; The specialised pipeline: PROBE_FLAG true.
        (let* ((vfn (specialised-function library "probe_vertex" t))
               (ffn (specialised-function library "probe_fragment" t))
               (pipeline (make-render-pipeline device vfn ffn))
               (target (make-target device width height)))
          (report (not (nullp pipeline))
                  "newRenderPipelineStateWithDescriptor:error: (NSError** out-param)")
          (report (not (nullp target)) "an offscreen RGBA8 render target")
          (let ((pixels (render device queue pipeline target width height
                                '(0d0 0d0 0d0 1d0))))
            (report t "setViewport: (MTLViewport, 48 bytes, indirect)")
            (report t "getBytes:...fromRegion:... (MTLRegion, 48 bytes, indirect)")
            ;; uv.x runs 0..1 left to right, uv.y 0..1 bottom to top, and Metal's
            ;; texture origin is top-left -- so row 0 of the readback is uv.y=1.
            (let ((tl (px pixels width 1 1))
                  (tr (px pixels width (- width 2) 1))
                  (bl (px pixels width 1 (- height 2))))
              (note "top-left ~S  top-right ~S  bottom-left ~S" tl tr bl)
              (report (and (near (first tl) 0 8) (near (first tr) 255 8))
                      "red channel tracks uv.x across the quad")
              (report (> (second tl) (second bl))
                      "green channel tracks uv.y down the quad")
              (report (every (lambda (p) (= 255 p))
                             (list (fourth tl) (fourth tr) (fourth bl)))
                      "alpha is opaque where the quad covers"))
            )

          ;; The same source with the constant UNSET must produce solid blue.
          ;; If it does not, the specialisation did nothing and the whole
          ;; function-constant scheme is an illusion.
          (let* ((vfn2 (specialised-function library "probe_vertex" nil))
                 (ffn2 (specialised-function library "probe_fragment" nil))
                 (pipeline2 (make-render-pipeline device vfn2 ffn2))
                 (target2 (make-target device width height))
                 (pixels (render device queue pipeline2 target2 width height
                                 '(0d0 0d0 0d0 1d0)))
                 (centre (px pixels width (floor width 2) (floor height 2))))
            (note "centre with the constant unset: ~S" centre)
            (report (and (near (first centre) 0) (near (second centre) 0)
                         (near (third centre) 255))
                    "a function constant genuinely specialises the pipeline"))

          ;; And the clear colour, which is the HFA path.
          (let* ((target3 (make-target device width height))
                 (rpd (objc:invoke "MTLRenderPassDescriptor" "renderPassDescriptor"))
                 (att (objc:invoke (objc:invoke rpd "colorAttachments")
                                   "objectAtIndexedSubscript:" 0)))
            (objc:invoke att "setTexture:" target3)
            (objc:invoke att "setLoadAction:" +load-action-clear+)
            (objc:invoke att "setStoreAction:" +store-action-store+)
            (let ((cc (mtl-clear-color 0.25d0 0.5d0 0.75d0 1d0)))
              (unwind-protect (objc:invoke att "setClearColor:" cc)
                (cffi:foreign-free cc)))
            (let ((cb (objc:invoke queue "commandBuffer")))
              (objc:invoke (objc:invoke cb "renderCommandEncoderWithDescriptor:" rpd)
                           "endEncoding")
              (objc:invoke cb "commit")
              (objc:invoke cb "waitUntilCompleted"))
            (let* ((n (* width height 4))
                   (buf (cffi:foreign-alloc :uint8 :count n))
                   (region (mtl-region 0 0 width height)))
              (unwind-protect
                   (progn
                     (objc:invoke target3 "getBytes:bytesPerRow:fromRegion:mipmapLevel:"
                                  buf (* width 4) region 0)
                     (let ((got (list (cffi:mem-aref buf :uint8 0)
                                      (cffi:mem-aref buf :uint8 1)
                                      (cffi:mem-aref buf :uint8 2)
                                      (cffi:mem-aref buf :uint8 3))))
                       (note "cleared to ~S, wanted (64 128 191 255)" got)
                       (report (and (near (first got) 64) (near (second got) 128)
                                    (near (third got) 191) (= 255 (fourth got)))
                               "setClearColor: (MTLClearColor, a 32-byte HFA in v0-v3)")))
                (cffi:foreign-free buf)
                (cffi:foreign-free region)))))))
    t))

;;; 3. libvterm through the shim ----------------------------------------------

(cffi:define-foreign-library libcathode
  (t (:default "libcathode")))

(cffi:defcfun ("vterm_new" %vterm-new) :pointer (rows :int) (cols :int))
(cffi:defcfun ("vterm_free" %vterm-free) :void (vt :pointer))
(cffi:defcfun ("vterm_set_utf8" %vterm-set-utf8) :void (vt :pointer) (utf8 :int))
(cffi:defcfun ("vterm_obtain_screen" %vterm-obtain-screen) :pointer (vt :pointer))
(cffi:defcfun ("vterm_screen_reset" %vterm-screen-reset) :void
  (screen :pointer) (hard :int))
(cffi:defcfun ("vterm_input_write" %vterm-input-write) :unsigned-long
  (vt :pointer) (bytes :pointer) (len :unsigned-long))

;;; The shim's flattened entry points.
(cffi:defcfun ("crt_screen_set_callbacks" %crt-set-callbacks) :pointer
  (screen :pointer) (cbs :pointer) (user :pointer))
(cffi:defcfun ("crt_screen_context_free" %crt-context-free) :void (ctx :pointer))
(cffi:defcfun ("crt_screen_get_row" %crt-get-row) :int
  (screen :pointer) (row :int) (cols :int) (cells :pointer))
(cffi:defcfun ("crt_set_winsize" %crt-set-winsize) :int
  (fd :int) (rows :int) (cols :int))

;;; VTermScreenCell, measured with a C program rather than guessed: 40 bytes,
;;; and `width' sits at 24 but the next member starts at 28 because the bitfield
;;; that follows realigns.  CFFI reads a structure THROUGH A POINTER without
;;; difficulty -- only by-value crossings need the shim -- so this is a plain
;;; defcstruct.
(cffi:defcstruct (vterm-screen-cell :size 40)
  (chars :uint32 :count 6)
  (width :char   :offset 24)
  (attrs :uint32 :offset 28)
  (fg    :uint32 :offset 32)
  (bg    :uint32 :offset 36))

;;; The flat callback table the shim expects.  Nine function pointers; we fill
;;; in the one that matters and leave the rest null, which the shim reads as
;;; "not handled".
(cffi:defcstruct crt-screen-callbacks
  (damage :pointer) (moverect :pointer) (movecursor :pointer)
  (settermprop :pointer) (bell :pointer) (resize :pointer)
  (sb-pushline :pointer) (sb-popline :pointer) (sb-clear :pointer))

(defvar *damage* nil "Rectangles the damage callback was handed.")
(defvar *cursor* nil)

;;; THE CROSSING CFFI CANNOT MAKE UNAIDED.  libvterm hands `damage' a VTermRect
;;; BY VALUE -- four ints, 16 bytes -- and `cffi:defcallback' signals
;;; CASE-FAILURE when asked to receive a structure that way.  The shim's
;;; trampoline unpacks it first, so what arrives here is four plain ints.
(cffi:defcallback probe-damage :int
    ((start-row :int) (end-row :int) (start-col :int) (end-col :int)
     (user :pointer))
  (declare (ignore user))
  (push (list start-row end-row start-col end-col) *damage*)
  1)

(cffi:defcallback probe-movecursor :int
    ((row :int) (col :int) (old-row :int) (old-col :int) (visible :int)
     (user :pointer))
  (declare (ignore old-row old-col user))
  (setf *cursor* (list row col visible))
  1)

(defun cell-text (screen row cols)
  "The first codepoint of each cell of ROW, as a string, right-trimmed."
  (cffi:with-foreign-object (cells '(:struct vterm-screen-cell) cols)
    (let ((n (%crt-get-row screen row cols cells)))
      (string-right-trim
       " "
       (with-output-to-string (out)
         (dotimes (i n)
           (let* ((cell (cffi:mem-aptr cells '(:struct vterm-screen-cell) i))
                  (cp (cffi:mem-aref (cffi:foreign-slot-pointer
                                      cell '(:struct vterm-screen-cell) 'chars)
                                     :uint32 0)))
             (write-char (if (zerop cp) #\Space (code-char cp)) out))))))))

(defun cell-at (screen row col)
  "(values codepoint width fg bg) for one cell."
  (cffi:with-foreign-object (cells '(:struct vterm-screen-cell) (1+ col))
    (%crt-get-row screen row (1+ col) cells)
    (let ((cell (cffi:mem-aptr cells '(:struct vterm-screen-cell) col)))
      (values (cffi:mem-aref (cffi:foreign-slot-pointer
                              cell '(:struct vterm-screen-cell) 'chars)
                             :uint32 0)
              (cffi:foreign-slot-value cell '(:struct vterm-screen-cell) 'width)
              (cffi:foreign-slot-value cell '(:struct vterm-screen-cell) 'fg)
              (cffi:foreign-slot-value cell '(:struct vterm-screen-cell) 'bg)))))

(defun write-vt (vt string)
  (let ((octets (babel:string-to-octets string :encoding :utf-8)))
    (cffi:with-foreign-object (buf :uint8 (length octets))
      (dotimes (i (length octets))
        (setf (cffi:mem-aref buf :uint8 i) (aref octets i)))
      (%vterm-input-write vt buf (length octets)))))

(defun probe-vterm ()
  (heading "2. libvterm through the shim")
  (handler-case (cffi:use-foreign-library libcathode)
    (error (e)
      (report nil "libcathode.dylib loads: ~A" e)
      (return-from probe-vterm nil)))
  (report t "libcathode.dylib loads")

  (let* ((rows 24) (cols 80)
         (vt (%vterm-new rows cols)))
    (report (not (cffi:null-pointer-p vt)) "vterm_new(~D, ~D)" rows cols)
    (%vterm-set-utf8 vt 1)
    (let ((screen (%vterm-obtain-screen vt)))
      (report (not (cffi:null-pointer-p screen)) "vterm_obtain_screen")
      ;; Callbacks must be installed BEFORE the reset, or the reset's own damage
      ;; is dropped and the first frame looks clean when it is not.
      (let ((ctx (cffi:with-foreign-object (cbs '(:struct crt-screen-callbacks))
                   (cffi:foreign-funcall "memset" :pointer cbs :int 0
                                         :unsigned-long
                                         (cffi:foreign-type-size
                                          '(:struct crt-screen-callbacks))
                                         :pointer)
                   (setf (cffi:foreign-slot-value cbs '(:struct crt-screen-callbacks)
                                                  'damage)
                         (cffi:callback probe-damage)
                         (cffi:foreign-slot-value cbs '(:struct crt-screen-callbacks)
                                                  'movecursor)
                         (cffi:callback probe-movecursor))
                   (%crt-set-callbacks screen cbs (cffi:null-pointer)))))
        (report (not (cffi:null-pointer-p ctx)) "crt_screen_set_callbacks")
        (%vterm-screen-reset screen 1)

        (setf *damage* nil *cursor* nil)
        (write-vt vt "hello")
        (report *damage*
                "a VTermRect reached a Lisp callback BY VALUE, flattened by the shim")
        (note "damage rects: ~S" (reverse *damage*))
        (note "cursor: ~S" *cursor*)
        (report (string= "hello" (cell-text screen 0 cols))
                "plain text lands in row 0: ~S" (cell-text screen 0 cols))
        (report (equal (list 0 5) (subseq (or *cursor* '(nil nil)) 0 2))
                "the cursor advanced to (0 5)")

        ;; SGR.  VTermColor is a tagged 4-byte union: the low byte is the type,
        ;; and for an indexed colour the next byte is the palette index.
        (write-vt vt (format nil "~C[31mR" #\Escape))
        (multiple-value-bind (cp width fg) (cell-at screen 0 5)
          (declare (ignore width))
          (let ((type (ldb (byte 8 0) fg)) (idx (ldb (byte 8 8) fg)))
            (note "cell 5: codepoint ~D fg=#x~8,'0X type=~D index=~D" cp fg type idx)
            (report (= cp (char-code #\R)) "SGR text lands")
            (report (and (= 1 (logand type 1)) (= idx 1))
                    "ESC[31m sets an indexed foreground of 1 (red)")))

        ;; Erase, then a wide character.
        (write-vt vt (format nil "~C[2J~C[H" #\Escape #\Escape))
        (report (string= "" (cell-text screen 0 cols)) "ESC[2J clears the screen")

        (write-vt vt "漢x")
        (multiple-value-bind (cp width) (cell-at screen 0 0)
          (note "wide cell: codepoint ~D width ~D" cp width)
          (report (= cp #x6F22) "a CJK codepoint survives UTF-8 decoding")
          (report (= width 2) "and occupies two cells"))

        (%crt-context-free ctx)))
    (%vterm-free vt))
  t)

;;; 4. The pty, and the variadic ioctl -----------------------------------------

(cffi:defcfun ("forkpty" %forkpty) :int
  (amaster :pointer) (name :pointer) (termios :pointer) (winsize :pointer))

(defun probe-pty ()
  (heading "3. The pty, and the variadic ioctl")
  ;; `stty size' in the child reports the kernel's idea of the window, which is
  ;; the only witness that TIOCSWINSZ actually arrived.  The child blocks on a
  ;; read first, so the parent's resize is guaranteed to land before stty runs:
  ;; no sleep, no race.
  (let* ((argv-list (list "/bin/sh" "-c" "read x; stty size"))
         (argv (cffi:foreign-alloc :pointer :count (1+ (length argv-list)))))
    (loop for s in argv-list for i from 0
          do (setf (cffi:mem-aref argv :pointer i) (cffi:foreign-string-alloc s)))
    (setf (cffi:mem-aref argv :pointer (length argv-list)) (cffi:null-pointer))
    (cffi:with-foreign-object (amaster :int)
      (let ((pid (%forkpty amaster (cffi:null-pointer) (cffi:null-pointer)
                           (cffi:null-pointer))))
        (cond
          ((zerop pid)
           ;; Child.  Allocate nothing between fork and exec.
           (cffi:foreign-funcall "execv" :pointer (cffi:mem-aref argv :pointer 0)
                                         :pointer argv :int)
           (cffi:foreign-funcall "_exit" :int 127 :void))
          ((minusp pid) (report nil "forkpty"))
          (t
           (report t "forkpty -> pid ~D" pid)
           (let ((fd (cffi:mem-ref amaster :int)))
             (report (zerop (%crt-set-winsize fd 30 100))
                     "crt_set_winsize(fd, 30, 100)")
             ;; Release the child, then read what stty saw.
             (cffi:with-foreign-string (nl (format nil "~%"))
               (cffi:foreign-funcall "write" :int fd :pointer nl
                                             :unsigned-long 1 :long))
             (let ((text (with-output-to-string (out)
                           (cffi:with-foreign-object (buf :uint8 4096)
                             (loop repeat 20
                                   for n = (cffi:foreign-funcall
                                            "read" :int fd :pointer buf
                                            :unsigned-long 4096 :long)
                                   while (plusp n)
                                   do (dotimes (i n)
                                        (write-char (code-char
                                                     (cffi:mem-aref buf :uint8 i))
                                                    out))
                                      (when (search "30 100" (get-output-stream-string
                                                              (make-string-output-stream)))
                                        (return)))))))
               (note "child said: ~S" (string-trim '(#\Space #\Newline #\Return) text))
               (report (search "30 100" text)
                       "the child's `stty size' reports 30 rows by 100 columns")
               (note "measured against the same call made with a plain")
               (note "cffi:foreign-funcall: ioctl returns -1 and the child")
               (note "says \"0 0\".  ioctl is variadic, and on Apple arm64 a")
               (note "variadic argument goes on the stack, not in a register.")))
           (cffi:foreign-funcall "close" :int (cffi:mem-ref amaster :int) :int)
           (cffi:foreign-funcall "waitpid" :int pid :pointer (cffi:null-pointer)
                                           :int 0 :int)))))
    (loop for i from 0
          for p = (cffi:mem-aref argv :pointer i)
          until (cffi:null-pointer-p p)
          do (cffi:foreign-string-free p))
    (cffi:foreign-free argv))
  t)

;;; Entry point ----------------------------------------------------------------

(defun run ()
  (format t "~&cathode-ray-tube :: M0 probe~%")
  (let ((*failures* 0) (*checks* 0))
    (probe-environment)
    (objc:ensure-objc-initialized
     :modules '("/System/Library/Frameworks/AppKit.framework/AppKit"
                "/System/Library/Frameworks/Metal.framework/Metal"
                "/System/Library/Frameworks/QuartzCore.framework/QuartzCore"
                "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics"
                "/System/Library/Frameworks/CoreText.framework/CoreText"))
    (probe-metal)
    (probe-vterm)
    (probe-pty)
    (heading "Summary")
    (format t "~&~D checks, ~D failure~:P~%" *checks* *failures*)
    (zerop *failures*)))
