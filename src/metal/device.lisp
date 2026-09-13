;;;; src/metal/device.lisp -- the device, the queue, and the float traps.

(in-package #:cathode-ray-tube.metal)

(defparameter +frameworks+
  '("/System/Library/Frameworks/AppKit.framework/Versions/C/AppKit"
    "/System/Library/Frameworks/Metal.framework/Metal"
    "/System/Library/Frameworks/QuartzCore.framework/QuartzCore"
    "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics"
    "/System/Library/Frameworks/CoreText.framework/CoreText")
  "EVERY framework this program uses, named in ONE place.

AppKit is in a list that belongs to the Metal layer, which looks wrong and is
not.  OBJC:ENSURE-OBJC-INITIALIZED is process-global and happens ONCE: the first
call flushes the queue of classes DEFINE-OBJC-CLASS has been accumulating, and a
class whose superclass is not loaded by that moment has no superclass, for the
rest of the process.  A later call with more modules does not go back and fix it.

So a partial list is not merely incomplete, it is a trap -- and it was a real
one.  With Metal alone here, the first thing to ask for a device initialised
objc, the queue flushed, CathodeRayTubeView could not find NSView, and the error
surfaced as `no Metal device on this machine' from METAL-AVAILABLE-P, whose
handler-case was swallowing it, and separately as `Attempting to make an
instance of a class which does not exist' from the window test.  One cause, two
symptoms, neither of them naming the real problem.

lem-cocoa/main.lisp states the rule; this is what ignoring it looks like.")

(defun ensure-frameworks ()
  "Bring the Objective-C runtime up with every framework loaded.  Idempotent."
  (objc:ensure-objc-initialized :modules +frameworks+))

(defun ensure-metal ()
  (ensure-frameworks))

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
own suite follows.

Initialisation happens OUTSIDE the handler-case, deliberately.  A framework that
will not load, or a class registration that failed, is a bug in this program and
must not be reported as `there is no GPU here' -- which is exactly what it did
report, once, and it cost an hour."
  (ensure-frameworks)
  (handler-case (and (default-device) t)
    (error () nil)))

(defvar *texture-storage-mode* nil
  "Cached: the storage mode a texture this program reads back must be made with.")

(defun apple-gpu-p ()
  "True on an Apple GPU -- which is to say, on Apple silicon.

-supportsFamily: with MTLGPUFamilyApple1 answers YES on every Apple GPU and NO
on every Intel and AMD one, which is exactly the line this program cares about."
  (let ((device (default-device)))
    (and device
         (with-metal (objc:invoke-bool device "supportsFamily:" +gpu-family-apple1+)))))

(defun texture-storage-mode ()
  "SHARED on Apple silicon, MANAGED everywhere else.

This is the fix for twenty-six green-on-arm64, zero-on-Intel assertions.  On a
discrete or Intel GPU the CPU and the GPU hold SEPARATE copies of a texture's
memory, so a render target the GPU has just written reads back on the CPU as the
zeros it was allocated with -- no error, no warning, just a black picture and a
suite full of `got (0 0 0 0)'.  Managed storage plus a blit synchronize before
reading is what makes the two copies agree; see SYNCHRONIZE-TEXTURE.

hasUnifiedMemory is the obvious thing to ask and is the WRONG question: an Intel
integrated GPU shares physical memory with the CPU and answers YES, and still
keeps a separate cached copy that only a blit reconciles.  Ask what kind of GPU
it is instead."
  (or *texture-storage-mode*
      (setf *texture-storage-mode*
            (if (apple-gpu-p) +storage-mode-shared+ +storage-mode-managed+))))

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
  (setf *device* nil *queue* nil *texture-storage-mode* nil))

(defun null-object-p (x)
  "True for a null pointer or an OBJC object wrapping one.

-[... new...] returns nil rather than signalling when it fails, and a nil that
is allowed to travel shows up several calls later as something inexplicable."
  (cffi:null-pointer-p (if (cffi:pointerp x) x (objc:objc-object-pointer x))))
