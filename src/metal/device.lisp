;;;; src/metal/device.lisp -- the device, the queue, and the float traps.

(in-package #:cathode-ray-tube.metal)

(defparameter +frameworks+
  '("/System/Library/Frameworks/Metal.framework/Metal"
    "/System/Library/Frameworks/QuartzCore.framework/QuartzCore"
    "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics")
  "Loaded before anything else touches Metal.  OBJC:DEFINE-OBJC-CLASS queues
class registration until initialisation, so every framework whose classes we
subclass or instantiate must be named by then -- lem-cocoa/main.lisp states the
rule and this is the same discipline.")

(defun ensure-metal ()
  (objc:ensure-objc-initialized :modules +frameworks+))

(cffi:defcfun ("MTLCreateSystemDefaultDevice" %create-system-default-device) :pointer)

(defmacro with-metal (&body body)
  "Run BODY with the floating-point traps Metal violates masked.

Not defensive.  MTLCreateSystemDefaultDevice signals FLOATING-POINT-OVERFLOW on
the way to answering and SBCL runs with that trap enabled.  The bridge masks
traps around every message send and every Lisp-implemented method, so ordinary
OBJC:INVOKE is covered -- but a CFFI:DEFCFUN is not a message send and nothing
masks it for you.  Every plain C entry point in this program is wrapped."
  `(float-features:with-float-traps-masked (:invalid :divide-by-zero :overflow)
     ,@body))

(defvar *device* nil "The MTLDevice, once someone has asked for one.")
(defvar *queue* nil "Its command queue.  One is enough; they are free to make.")

(defun default-device ()
  "The system's default Metal device, or NIL where there is not one."
  (ensure-metal)
  (or *device*
      (setf *device*
            (with-metal
              (let ((device (%create-system-default-device)))
                (unless (cffi:null-pointer-p device) device))))))

(defun metal-available-p ()
  "True when this machine has a GPU Metal will talk to.

A virtualised runner may not, and that is a fact about the machine rather than
about this code -- so the GPU tests SKIP on it, which is the discipline objc's
own suite follows."
  (handler-case (and (default-device) t)
    (error () nil)))

(defun device-name ()
  (let ((device (default-device)))
    (when device (with-metal (objc:invoke-into 'string device "name")))))

(defun command-queue ()
  (or *queue*
      (setf *queue*
            (let ((device (or (default-device)
                              (error "No Metal device on this machine."))))
              (with-metal (objc:invoke device "newCommandQueue"))))))

(defun reset-device ()
  "Forget the device and queue.  For tests, and for an image that was dumped."
  (setf *device* nil *queue* nil))

(defun null-object-p (x)
  "True for a null pointer or an OBJC object wrapping one.

-[... new...] returns nil rather than signalling when it fails, and a nil that
is allowed to travel shows up several calls later as something inexplicable."
  (cffi:null-pointer-p (if (cffi:pointerp x) x (objc:objc-object-pointer x))))
