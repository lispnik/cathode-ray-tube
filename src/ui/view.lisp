;;;; src/ui/view.lisp -- the NSView, its CAMetalLayer, and the frame clock.
;;;;
;;;; LAYER-HOSTED, not layer-backed: we hand the view a CAMetalLayer we made and
;;;; own, rather than letting AppKit make one.  -setLayer: must come BEFORE
;;;; -setWantsLayer:; the other order gets you AppKit's own layer and silently
;;;; ignores yours.
;;;;
;;;; NOT AN MTKView.  MetalKit would work and would be less code, but it hides
;;;; exactly the things this program has opinions about -- displaySyncEnabled,
;;;; maximumDrawableCount, drawableSize against the backing scale, the
;;;; colorspace, and `opaque', which is what the windowOpacity setting needs.
;;;; CAMetalLayer is thirty more lines and no mysteries.
;;;;
;;;; THE FRAME CLOCK IS A CADisplayLink ON THE MAIN RUN LOOP, and that choice is
;;;; load-bearing rather than stylistic.  A CVDisplayLink calls back on a
;;;; CoreVideo-owned pthread; on a stock SBCL, running Lisp on a thread Lisp
;;;; never created is the documented way to end the process with
;;;; `cannot suspend thread: 45, Operation not supported' and no condition --
;;;; a collection stops the world by signalling every other thread, and Darwin
;;;; refuses to signal a foreign worker.  -[NSView displayLinkWithTarget:selector:]
;;;; (macOS 14+) delivers on the MAIN thread, so no foreign thread ever enters
;;;; Lisp and --with-sb-safepoint is not required.  CVDisplayLink is also
;;;; deprecated as of the same release.

(in-package #:cathode-ray-tube.ui)

(objc:define-objc-class crt-view ()
  ((layer      :initform nil :accessor view-layer)
   (link       :initform nil :accessor view-link)
   ;; The frame-skipped effects clock.  TimeManager.qml advances `time' only
   ;; every effectsFrameSkip'th frame, so at 60Hz the effects animate at ~20Hz
   ;; while the quad still redraws at 60.  That quantisation is part of the look
   ;; -- flicker reads as chunky rather than smooth -- so it is reproduced here
   ;; rather than replaced with a continuous clock.
   (frame-skip :initform 3   :accessor view-frame-skip)
   (tick       :initform 0   :accessor view-tick)
   (started-at :initform nil :accessor view-started-at)
   (effect-time :initform 0d0 :accessor view-effect-time)
   (frames     :initform 0   :accessor view-frames)
   (drawable-size :initform '(0 0) :accessor view-drawable-size)
   (draw-function :initform nil :accessor view-draw-function)
   ;; Set when the view changes size, acted on at the top of the next frame.
   ;; Doing the work here would reallocate the render target, the cell grid and
   ;; the instance buffer once per mouse-move event while a window edge is being
   ;; dragged; doing it once per frame is the same result and a fraction of the
   ;; work.
   (resized :initform nil :accessor view-resized-p)
   (key-handler :initform nil :accessor view-key-handler))
  (:objc-class-name "CathodeRayTubeView")
  (:objc-superclass-name "NSView"))

;;; Metal draws bottom-up in its own clip space and the layer composites the
;;; result; the view's flippedness only affects AppKit's own coordinate
;;; conventions for events, and top-left origin is easier to reason about for a
;;; character grid.
(objc:define-objc-method ("isFlipped" objc:objc-bool) ((self crt-view))
  t)

(objc:define-objc-method ("acceptsFirstResponder" objc:objc-bool) ((self crt-view))
  t)

(objc:define-objc-method ("wantsUpdateLayer" objc:objc-bool) ((self crt-view))
  t)

;;; Backing scale changes when the window moves between a Retina display and an
;;; external one, and the drawable has to follow or everything is half size.
(objc:define-objc-method ("viewDidChangeBackingProperties" :void) ((self crt-view))
  (handling-errors ("viewDidChangeBackingProperties")
    (update-drawable-size self)))

(objc:define-objc-method ("setFrameSize:" :void)
    ((self crt-view) (size cocoa:ns-size))
  (handling-errors ("setFrameSize:")
    (objc:invoke (objc:current-super) "setFrameSize:" size)
    (update-drawable-size self)
    (setf (view-resized-p self) t)))

(objc:define-objc-method ("keyDown:" :void)
    ((self crt-view) (event objc:objc-object-pointer))
  (handling-errors ("keyDown:")
    (let ((handler (view-key-handler self)))
      (when handler
        (let ((bytes (event-key-string event)))
          (when bytes (funcall handler bytes)))))))

;;; Swallowed, not passed on.  AppKit's default -keyUp: and -flagsChanged: do
;;; nothing we want, and -doCommandBySelector: would turn a bare Return into
;;; -insertNewline: and beep at anything it did not recognise.
(objc:define-objc-method ("keyUp:" :void)
    ((self crt-view) (event objc:objc-object-pointer))
  (declare (ignore event))
  nil)

;;; The display link's target.  Named with a prefix because a selector is a
;;; process-global name and `stepFrame:' is the sort of thing another framework
;;; might also define on NSView.
(objc:define-objc-method ("crtStepFrame:" :void)
    ((self crt-view) (link objc:objc-object-pointer))
  (declare (ignore link))
  (handling-errors ("crtStepFrame:")
    (step-frame self)))

;;; Geometry -------------------------------------------------------------------

(defun backing-scale (view)
  (let ((window (objc:invoke (objc:objc-object-pointer view) "window")))
    (if (crt.metal:null-object-p window)
        1d0
        (objc:invoke-into 'double-float window "backingScaleFactor"))))

(defun update-drawable-size (view)
  "Match the layer's drawable to the view's size in DEVICE pixels."
  (let* ((pointer (objc:objc-object-pointer view))
         (bounds (objc:invoke-into 'vector pointer "bounds"))
         (scale (backing-scale view))
         (width (max 1 (round (* (aref bounds 2) scale))))
         (height (max 1 (round (* (aref bounds 3) scale))))
         (layer (view-layer view)))
    (when layer
      (objc:invoke layer "setContentsScale:" scale)
      ;; CGSize is one of the four structures the bridge converts by name, so
      ;; the #(...) shorthand works here -- and only here.  Everything else
      ;; Metal takes by value is a filled buffer.
      (objc:invoke layer "setDrawableSize:"
                   (vector (float width 1d0) (float height 1d0))))
    (setf (view-drawable-size view) (list width height))))

;;; The layer ------------------------------------------------------------------

(defun attach-metal-layer (view)
  "Give VIEW a CAMetalLayer it owns."
  (let ((pointer (objc:objc-object-pointer view))
        (layer (objc:invoke "CAMetalLayer" "layer"))
        (device (or (crt.metal:default-device)
                    (error "No Metal device on this machine."))))
    (objc:invoke layer "setDevice:" device)
    ;; BGRA8Unorm rather than RGBA: it is what CAMetalLayer wants natively, and
    ;; using anything else makes the window server convert every frame.
    (objc:invoke layer "setPixelFormat:" crt.metal:+pixel-format-bgra8unorm+)
    ;; We never sample the drawable, only write it.
    (objc:invoke layer "setFramebufferOnly:" t)
    (objc:invoke layer "setOpaque:" t)
    ;; Vsync.  The effects clock quantises on top of this; turning it off would
    ;; burn a core to draw frames nobody sees.
    (objc:invoke layer "setDisplaySyncEnabled:" t)
    (setf (view-layer view) layer)
    ;; ORDER MATTERS: setLayer: then setWantsLayer:.  The reverse gets AppKit's
    ;; own layer and ignores this one.
    (objc:invoke pointer "setLayer:" layer)
    (objc:invoke pointer "setWantsLayer:" t)
    (update-drawable-size view)
    layer))

;;; The clock ------------------------------------------------------------------

(defun start-display-link (view)
  "Begin receiving frames on the MAIN thread."
  (let* ((pointer (objc:objc-object-pointer view))
         (link (objc:invoke pointer "displayLinkWithTarget:selector:"
                            pointer (objc:coerce-to-selector "crtStepFrame:"))))
    (when (crt.metal:null-object-p link)
      (error "This macOS has no -[NSView displayLinkWithTarget:selector:]; ~
              cathode-ray-tube needs macOS 14 or later."))
    ;; One mode at a time, and never kCFRunLoopCommonModes -- see the note in
    ;; frameworks.lisp, where the measurement is.  mainRunLoop rather than
    ;; currentRunLoop: they are the same object here, and saying which one we
    ;; mean costs nothing and survives being called from somewhere else.
    (let ((run-loop (objc:invoke "NSRunLoop" "mainRunLoop")))
      (dolist (mode +run-loop-modes+)
        (objc:invoke link "addToRunLoop:forMode:" run-loop mode)))
    (setf (view-link view) link
          (view-started-at view) (get-internal-real-time))
    link))

(defun stop-display-link (view)
  (let ((link (view-link view)))
    (when link
      (objc:invoke link "invalidate")
      (setf (view-link view) nil))))

(defun tick-effect-clock (view)
  "Advance the effects clock every FRAME-SKIP'th frame.

Upstream counts FrameAnimation triggers and publishes elapsedTime every
effectsFrameSkip'th one; counting display-link ticks is the same quantisation on
the same clock.  Feeding a smooth CACurrentMediaTime() here would be wrong in a
way that is hard to name and easy to see."
  (incf (view-tick view))
  (when (>= (view-tick view) (max 1 (view-frame-skip view)))
    (setf (view-tick view) 0
          (view-effect-time view)
          (/ (float (- (get-internal-real-time) (view-started-at view)) 1d0)
             internal-time-units-per-second)))
  (view-effect-time view))

;;; The frame ------------------------------------------------------------------

(defun step-frame (view)
  "One frame: acquire a drawable, let the draw function fill it, present."
  (crt.metal:with-metal
    (let* ((layer (view-layer view))
           (drawable (and layer (objc:invoke layer "nextDrawable"))))
      ;; nextDrawable returns nil when every drawable is still in flight, or
      ;; when the layer has no area.  A dropped frame is correct behaviour here;
      ;; blocking for one is not.
      (unless (or (null drawable) (crt.metal:null-object-p drawable))
        (let ((time (tick-effect-clock view))
              (texture (objc:invoke drawable "texture"))
              (function (view-draw-function view)))
          (incf (view-frames view))
          (when function
            (funcall function view texture drawable time)))))))
