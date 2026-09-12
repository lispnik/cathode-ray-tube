;;;; src/metal/resources.lisp -- textures, samplers, and the structures Metal
;;;; insists on passing by value.
;;;;
;;;; EVERY Metal structure argument is a filled buffer, without exception.
;;;; OBJC::MARSHAL-ARGUMENT accepts the #(...) vector shorthand only for the four
;;;; structures the bridge converts by name -- NSRect, NSPoint, NSSize and
;;;; NSRange -- and everything else, however convenient its shape, signals
;;;; `Cannot pass #(...) as a structure argument.'  MTLClearColor is four
;;;; doubles and looks exactly like an NSRect, and is still refused, because the
;;;; runtime reports it as an ANONYMOUS structure.  The ABI is not the problem:
;;;; sb-alien sees four doubles and passes them in v0-v3 as the homogeneous
;;;; float aggregate they are.  Measured in tools/probe.lisp.
;;;;
;;;; So the buffers are wrapped AT THE CALL SITE and no caller ever holds one.

(in-package #:cathode-ray-tube.metal)

(defstruct (texture (:constructor %make-texture (handle width height pixel-format)))
  "An MTLTexture and the three things about it we ask for constantly.

The dimensions are cached rather than read back with -width/-height because the
render graph consults them once per pass per frame, and a message send to learn
a number we chose ourselves is a silly way to spend a frame."
  handle
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (pixel-format 0 :type fixnum))

(defun make-texture (&key width height (pixel-format +pixel-format-rgba8unorm+)
                          (usage (logior +usage-render-target+ +usage-shader-read+))
                          (storage +storage-mode-shared+)
                          label)
  "An offscreen texture.

STORAGE defaults to shared, which is what makes TEXTURE-BYTES possible; a target
nothing reads back should be private."
  (let ((device (or (default-device) (error "No Metal device on this machine."))))
    (with-metal
      (let ((descriptor
              (objc:invoke "MTLTextureDescriptor"
                           "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
                           pixel-format (max 1 width) (max 1 height) nil)))
        (objc:invoke descriptor "setUsage:" usage)
        (objc:invoke descriptor "setStorageMode:" storage)
        (objc:invoke descriptor "setTextureType:" +texture-type-2d+)
        (let ((handle (objc:invoke device "newTextureWithDescriptor:" descriptor)))
          (when (null-object-p handle)
            (error "Metal refused a ~Dx~D texture in format ~D." width height
                   pixel-format))
          (when label (objc:invoke handle "setLabel:" label))
          (%make-texture handle (max 1 width) (max 1 height) pixel-format))))))

(defun release-texture (texture)
  (when (and texture (texture-handle texture))
    (objc:release (texture-handle texture))
    (setf (texture-handle texture) nil)))

(defmacro with-mtl-region ((var x y width height) &body body)
  "MTLRegion: origin and size, three NSUIntegers each, 48 bytes.  Not an HFA, so
it crosses indirectly -- which is what every structure but the four Cocoa ones
does."
  `(cffi:with-foreign-object (,var :uint64 6)
     (setf (cffi:mem-aref ,var :uint64 0) ,x
           (cffi:mem-aref ,var :uint64 1) ,y
           (cffi:mem-aref ,var :uint64 2) 0
           (cffi:mem-aref ,var :uint64 3) ,width
           (cffi:mem-aref ,var :uint64 4) ,height
           (cffi:mem-aref ,var :uint64 5) 1)
     ,@body))

(defun bytes-per-pixel (pixel-format)
  (cond ((= pixel-format +pixel-format-r8unorm+) 1)
        ((= pixel-format +pixel-format-rgba16float+) 8)
        (t 4)))

(defun texture-bytes (texture)
  "TEXTURE's pixels as an octet vector, row-major from the top left.

Only for a shared or managed texture, and only for tests and tools -- reading a
render target back stalls the GPU, which is exactly what a per-pixel assertion
wants and exactly what a frame does not."
  (with-metal
    (let* ((width (texture-width texture))
           (height (texture-height texture))
           (stride (* width (bytes-per-pixel (texture-pixel-format texture))))
           (count (* stride height))
           (out (make-array count :element-type '(unsigned-byte 8))))
      (cffi:with-foreign-object (buffer :uint8 count)
        (with-mtl-region (region 0 0 width height)
          (objc:invoke (texture-handle texture)
                       "getBytes:bytesPerRow:fromRegion:mipmapLevel:"
                       buffer stride region 0))
        (dotimes (i count out)
          (setf (aref out i) (cffi:mem-aref buffer :uint8 i)))))))

(defun texture-pixel (pixels texture x y)
  "The (R G B A) of PIXELS -- a TEXTURE-BYTES result -- at X, Y."
  (let ((i (* (bytes-per-pixel (texture-pixel-format texture))
              (+ x (* y (texture-width texture))))))
    (list (aref pixels i) (aref pixels (+ i 1))
          (aref pixels (+ i 2)) (aref pixels (+ i 3)))))

(defun make-sampler (&key (min +filter-linear+) (mag +filter-linear+)
                          (address +address-clamp-to-edge+) label)
  "A sampler state.

MIN and MAG are nearest for the low-resolution fonts, whose whole point is that
their pixels stay square under magnification, and linear for everything else."
  (let ((device (or (default-device) (error "No Metal device on this machine."))))
    (with-metal
      (let ((descriptor (objc:invoke (objc:invoke "MTLSamplerDescriptor" "alloc") "init")))
        (objc:invoke descriptor "setMinFilter:" min)
        (objc:invoke descriptor "setMagFilter:" mag)
        (objc:invoke descriptor "setSAddressMode:" address)
        (objc:invoke descriptor "setTAddressMode:" address)
        (when label (objc:invoke descriptor "setLabel:" label))
        (let ((sampler (objc:invoke device "newSamplerStateWithDescriptor:" descriptor)))
          (objc:release descriptor)
          sampler)))))
