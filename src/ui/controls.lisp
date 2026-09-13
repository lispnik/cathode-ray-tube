;;;; src/ui/controls.lisp -- AppKit controls, and the one object they all talk to.
;;;;
;;;; Cocoa's target/action wants an object with a method per action.  The
;;;; behaviour here lives in Lisp closures made while the settings window is
;;;; being built, so there is ONE target, ONE selector, and a table from the
;;;; sender's address to the closure that belongs to it -- the same shape
;;;; actions.lisp uses for the menus, narrowed from "find the key session" to
;;;; "find this control".
;;;;
;;;; Every control is made with a CONVENIENCE CONSTRUCTOR -- +checkboxWithTitle:,
;;;; +sliderWithValue:, +labelWithString: -- rather than +alloc plus setters.
;;;; That is not brevity.  -setButtonType: takes NSButtonTypeSwitch, -setBezelStyle:
;;;; takes NSBezelStyleRounded, and AppKit's enumerations are not in the SDK
;;;; headers this project generates constants from, so every one would be a
;;;; number remembered rather than read -- which is the class of bug
;;;; tools/metal-constants.sh exists to make impossible.  The constructors take
;;;; no enumerations at all.

(in-package #:cathode-ray-tube.ui)

(objc:define-objc-class control-actions ()
  ()
  (:objc-class-name "CathodeRayTubeControlActions"))

(defvar *control-target* nil)
(defvar *control-handlers* (make-hash-table :test 'eql)
  "Sender address -> a function of one argument, the control.

Entries are never removed, because a control outlives nothing here: the settings
window is built once and kept.  A window that was rebuilt per opening would want
this cleared with it, and would say so.")

(defun control-target ()
  (or *control-target* (setf *control-target* (make-instance 'control-actions))))

(objc:define-objc-method ("crtControlChanged:" :void)
    ((self control-actions) (sender objc:objc-object-pointer))
  (handling-errors ("crtControlChanged:")
    (let ((handler (gethash (cffi:pointer-address sender) *control-handlers*)))
      (when handler (funcall handler sender)))))

(defun set-control-handler (control handler)
  "Make HANDLER run when CONTROL changes, and point the control at our target."
  (let ((pointer (objc:objc-object-pointer control)))
    (setf (gethash (cffi:pointer-address pointer) *control-handlers*) handler)
    (objc:invoke pointer "setTarget:" (objc:objc-object-pointer (control-target)))
    (objc:invoke pointer "setAction:" (objc:coerce-to-selector "crtControlChanged:"))
    control))

;;; Making them ----------------------------------------------------------------

(defun make-label (text &key (alignment :left))
  "A non-editable NSTextField.  ALIGNMENT is :LEFT or :RIGHT."
  (let ((label (objc:invoke "NSTextField" "labelWithString:" text)))
    ;; NSTextAlignmentRight is 1 and left is 0, and those two ARE worth writing
    ;; down: they are the only AppKit enumerators in this file, they have not
    ;; moved since NeXT, and the alternative is a control with no way to right
    ;; align a value.
    (objc:invoke label "setAlignment:" (ecase alignment (:left 0) (:right 1)))
    label))

(defun make-checkbox (title checked handler)
  "A checkbox.  HANDLER is called with T or NIL."
  (let ((button (objc:invoke "NSButton" "checkboxWithTitle:target:action:"
                             title (cffi:null-pointer) (cffi:null-pointer))))
    (objc:invoke button "setState:" (if checked 1 0))
    (set-control-handler
     button (lambda (sender)
              (funcall handler (plusp (objc:invoke-into 'integer sender "state")))))
    button))

(defun make-push-button (title handler)
  (let ((button (objc:invoke "NSButton" "buttonWithTitle:target:action:"
                             title (cffi:null-pointer) (cffi:null-pointer))))
    (set-control-handler button (lambda (sender) (declare (ignore sender))
                                  (funcall handler)))
    button))

(defun make-slider (value minimum maximum handler)
  "A continuous slider.  HANDLER is called with a double."
  (let ((slider (objc:invoke "NSSlider" "sliderWithValue:minValue:maxValue:target:action:"
                             (float value 1d0) (float minimum 1d0) (float maximum 1d0)
                             (cffi:null-pointer) (cffi:null-pointer))))
    ;; Continuous, so the terminal behind the window follows the thumb.  A
    ;; setting you can only see the effect of after letting go is one you tune by
    ;; guessing.
    (objc:invoke slider "setContinuous:" t)
    (set-control-handler
     slider (lambda (sender)
              (funcall handler (objc:invoke-into 'double-float sender "doubleValue"))))
    slider))

(defun make-popup (titles selected handler)
  "A pop-up menu of TITLES.  HANDLER is called with the index and the title."
  (let ((popup (objc:invoke (objc:invoke "NSPopUpButton" "alloc")
                            "initWithFrame:pullsDown:"
                            (vector 0d0 0d0 200d0 25d0) nil)))
    (dolist (title titles)
      (objc:invoke popup "addItemWithTitle:" title))
    (when (and selected (< -1 selected (length titles)))
      (objc:invoke popup "selectItemAtIndex:" selected))
    (set-control-handler
     popup (lambda (sender)
             (let ((index (objc:invoke-into 'integer sender "indexOfSelectedItem")))
               (funcall handler index (nth index titles)))))
    popup))

(defun make-text-field (text handler)
  (let ((field (objc:invoke "NSTextField" "textFieldWithString:" text)))
    (set-control-handler
     field (lambda (sender)
             (funcall handler (or (objc:invoke-into 'string sender "stringValue") ""))))
    field))

(defun make-color-well (hex handler)
  "An NSColorWell showing HEX, calling HANDLER with a new #rrggbb string.

The colour panel it opens is shared and modeless, which is Cocoa's answer to
upstream's ColorButton and its own colour dialog."
  (let ((well (objc:invoke (objc:invoke "NSColorWell" "alloc")
                           "initWithFrame:" (vector 0d0 0d0 44d0 24d0))))
    (objc:invoke well "setColor:" (ns-color-from-hex hex))
    (set-control-handler
     well (lambda (sender) (funcall handler (hex-from-ns-color
                                             (objc:invoke sender "color")))))
    well))

;;; Colours --------------------------------------------------------------------

(defun ns-color-from-hex (hex)
  "#rrggbb -> an NSColor.

Through UTIL:STR-TO-COLOR, so that what the well shows is what the shader gets:
that function divides by 256 rather than 255, faithfully to utils.js, and a
colour well that used the other divisor would disagree with the terminal behind
it by one part in 256 -- invisible, and enough to make a round trip through this
window drift a profile every time it was opened."
  (let ((rgba (util:str-to-color hex)))
    (objc:invoke "NSColor" "colorWithSRGBRed:green:blue:alpha:"
                 (float (util:rgba-r rgba) 1d0)
                 (float (util:rgba-g rgba) 1d0)
                 (float (util:rgba-b rgba) 1d0)
                 1d0)))

(defun hex-from-ns-color (color)
  "An NSColor -> #rrggbb, converted to sRGB first.

-redComponent on a colour in any other space raises rather than converting, and
the colour panel hands back whatever space the user picked in."
  (let ((srgb (objc:invoke color "colorUsingColorSpace:"
                           (objc:invoke "NSColorSpace" "sRGBColorSpace"))))
    (if (crt.metal:null-object-p srgb)
        "#000000"
        (util:color-to-str
         (util:make-rgba (objc:invoke-into 'double-float srgb "redComponent")
                         (objc:invoke-into 'double-float srgb "greenComponent")
                         (objc:invoke-into 'double-float srgb "blueComponent"))))))

;;; Laying them out ------------------------------------------------------------

(defconstant +row-height+ 28)
(defconstant +form-margin+ 16)
(defconstant +label-width+ 130)

(defun set-frame (view x y width height)
  (objc:invoke (objc:objc-object-pointer view) "setFrame:"
               (vector (float x 1d0) (float y 1d0)
                       (float width 1d0) (float height 1d0)))
  view)

(defun add-subview (parent child)
  (objc:invoke (objc:objc-object-pointer parent) "addSubview:"
               (objc:objc-object-pointer child))
  child)

(defun make-form (width rows)
  "A view holding ROWS, each (LABEL-OR-NIL CONTROL &optional TRAILING).

Laid out by hand, top down, in fixed rows.  Auto Layout would mean building
NSLayoutConstraint objects three at a time through a message-send bridge to
arrange a form whose shape is known in advance and never changes; hand-placed
frames are shorter, and the flipped-coordinate arithmetic is in one place."
  (let* ((height (+ (* +row-height+ (length rows)) (* 2 +form-margin+)))
         (view (objc:invoke (objc:invoke "NSView" "alloc") "initWithFrame:"
                            (vector 0d0 0d0 (float width 1d0) (float height 1d0)))))
    (loop for row in rows
          for index from 0
          ;; AppKit's origin is BOTTOM left, so the first row is the highest y.
          for y = (- height +form-margin+ (* +row-height+ (1+ index)))
          do (destructuring-bind (label control &optional trailing) row
               (when label
                 (add-subview view (set-frame (make-label label :alignment :right)
                                              +form-margin+ (+ y 4)
                                              +label-width+ 18)))
               (let* ((x (if label (+ +form-margin+ +label-width+ 8) +form-margin+))
                      (right (- width +form-margin+))
                      (trailing-width (if trailing 56 0))
                      (control-width (- right x trailing-width (if trailing 8 0))))
                 (add-subview view (set-frame control x (+ y 2)
                                              (max 40 control-width) 22))
                 (when trailing
                   (add-subview view (set-frame trailing (- right trailing-width)
                                                (+ y 4) trailing-width 18))))))
    view))
