;;;; tools/snapshot-png.lisp -- write a render target to a PNG.
;;;;
;;;; For looking at things, and the basis of the fidelity comparison in M4: the
;;;; effect chain is judged by pictures, and a picture you can diff is worth
;;;; more than a description of one.
;;;;
;;;; NSBitmapImageRep rather than a PNG encoder in Lisp -- the encoder is already
;;;; on the machine.  The trap, which objc's own shader example documents, is
;;;; that -bitmapData returns `unsigned char *', which ENCODES exactly as a C
;;;; string does; plain INVOKE converts it to a Lisp string and, since the buffer
;;;; starts with a zero byte, hands back "" rather than an error.  INVOKE-INTO
;;;; with :POINTER is what that disposition exists for.

(defpackage #:crt-snapshot
  (:use #:cl)
  (:export #:texture-to-png #:write-texture-png))

(in-package #:crt-snapshot)

(defun texture-to-png (texture)
  "TEXTURE's pixels as PNG bytes."
  (crt.metal:with-metal
    (let* ((width (crt.metal:texture-width texture))
           (height (crt.metal:texture-height texture))
           (pixels (crt.metal:texture-bytes texture)))
      (objc:with-autorelease-pool ()
        (let ((rep (objc:invoke
                    (objc:invoke "NSBitmapImageRep" "alloc")
                    (concatenate 'string
                                 "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:"
                                 "bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:"
                                 "colorSpaceName:bytesPerRow:bitsPerPixel:")
                    (cffi:null-pointer) width height 8 4 t nil
                    "NSCalibratedRGBColorSpace" (* width 4) 32)))
          (when (cffi:null-pointer-p (objc:objc-object-pointer rep))
            (error "Could not make a ~Dx~D bitmap." width height))
          (let ((destination (objc:invoke-into :pointer rep "bitmapData")))
            (dotimes (i (length pixels))
              (setf (cffi:mem-aref destination :uint8 i) (aref pixels i))))
          ;; NSBitmapImageFileTypePNG is 4.
          (let ((data (objc:invoke rep "representationUsingType:properties:"
                                   4 (objc:invoke "NSDictionary" "dictionary"))))
            (when (cffi:null-pointer-p (objc:objc-object-pointer data))
              (error "Could not encode the texture as a PNG."))
            (let* ((length (objc:invoke-into 'integer data "length"))
                   (bytes (objc:invoke-into :pointer data "bytes"))
                   (out (make-array length :element-type '(unsigned-byte 8))))
              (dotimes (i length out)
                (setf (aref out i) (cffi:mem-aref bytes :uint8 i))))))))))

(defun write-texture-png (texture path)
  (let ((bytes (texture-to-png texture)))
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence bytes out))
    path))
