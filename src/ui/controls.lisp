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
;;;
;;; A FLIPPED view, and that is the fix for the thing that was actually wrong.
;;; AppKit's origin is bottom left, and an NSScrollView whose document view is
;;; shorter than the clip view pins it to the BOTTOM -- so a tab with eight rows
;;; in a four-hundred-pixel scroller drew them in the lower half with a band of
;;; empty grey above, which looks like a rendering fault rather than a layout
;;; one.  Measured before believing it: the Effects form came out 340 tall in a
;;; 400 tall scroller, and the first screenshot showed the sliders sitting at
;;; the bottom.
;;;
;;; Flipping it puts the origin at the top left, which is where a form starts,
;;; and the arithmetic below reads downward like the form does.

(objc:define-objc-class form-view ()
  ()
  (:objc-class-name "CathodeRayTubeFormView")
  (:objc-superclass-name "NSView"))

(objc:define-objc-method ("isFlipped" objc:objc-bool) ((self form-view))
  t)

;;; The metrics, in one place.  Loosely macOS's own: a 22-point control on a
;;; 30-point pitch leaves eight points of air, which is what stops a column of
;;; sliders reading as a wall.
(defconstant +row-height+ 30 "Baseline to baseline for an ordinary row.")
(defconstant +control-height+ 22)
(defconstant +label-height+ 18)
(defconstant +form-margin+ 20)
(defconstant +label-width+ 140 "The right-aligned label column.")
(defconstant +gutter+ 12 "Between the label column and its control.")
(defconstant +readout-width+ 52 "The value shown beside a slider.")
(defconstant +section-lead+ 18 "Air ABOVE a section heading.")
(defconstant +section-trail+ 8 "And below it, before its first row.")
(defconstant +group-gap+ 10 "Between buttons sharing a row.")
(defconstant +well-width+ 64
  "A colour well.  Wide enough to read the colour, and nowhere near the width of
the window -- a well stretched across a form looks like a progress bar.")

(defun set-frame (view x y width height)
  (objc:invoke (objc:objc-object-pointer view) "setFrame:"
               (vector (float x 1d0) (float y 1d0)
                       (float width 1d0) (float height 1d0)))
  view)

(defun add-subview (parent child)
  (objc:invoke (objc:objc-object-pointer parent) "addSubview:"
               (objc:objc-object-pointer child))
  child)

(defun make-section-label (text)
  "A heading.  Small, bold, and the only thing in this window that is either."
  (let ((label (make-label text)))
    (objc:invoke label "setFont:"
                 (objc:invoke "NSFont" "boldSystemFontOfSize:" 11d0))
    (objc:invoke label "setTextColor:" (objc:invoke "NSColor" "secondaryLabelColor"))
    label))

(defun row-height-of (row &key first)
  "How much vertical space ROW needs, heading and gap rows included.

FIRST suppresses a heading's lead-in.  The form margin is already above it, and
a heading that adds its own air on top of that sits too far from the top edge to
look deliberate."
  (case (first row)
    (:section (+ (if first 0 +section-lead+) +label-height+ +section-trail+))
    (:gap (floor +row-height+ 2))
    (t +row-height+)))

(defun lay-out-row (view row width y)
  "Place ROW's controls at Y.  Returns nothing; the caller advances."
  (let ((right (- width +form-margin+)))
    (case (first row)
      (:section
       (add-subview view (set-frame (make-section-label (second row))
                                    +form-margin+ y
                                    (- right +form-margin+) +label-height+)))
      (:gap)
      (:group
       ;; Buttons side by side, each as wide as it needs to be.  They used to be
       ;; one per row at the full width of the window, which is how a Save
       ;; button ends up four hundred and sixty points wide.
       (let ((x +form-margin+))
         (dolist (control (rest row))
           (objc:invoke (objc:objc-object-pointer control) "sizeToFit")
           (let* ((frame (objc:invoke-into (vector 0d0 0d0 0d0 0d0) control "frame"))
                  (w (max 84 (+ 20 (aref frame 2)))))
             (add-subview view (set-frame control x y w +control-height+))
             (incf x (+ w +group-gap+))))))
      (t
       (destructuring-bind (label control &optional trailing fixed-width) row
         (when label
           (add-subview view (set-frame (make-label label :alignment :right)
                                        +form-margin+ (+ y 2)
                                        +label-width+ +label-height+)))
         (let* ((x (if label (+ +form-margin+ +label-width+ +gutter+) +form-margin+))
                (trailing-width (if trailing (+ +readout-width+ +group-gap+) 0))
                ;; FIXED-WIDTH for a control with a natural size.  A slider or a
                ;; pop-up wants the whole column; a colour well does not, and
                ;; stretching one across the form makes it look like a progress
                ;; bar rather than a swatch.
                (control-width (or fixed-width
                                   (max 60 (- right x trailing-width)))))
           (add-subview view (set-frame control x y control-width +control-height+))
           (when trailing
             (add-subview view (set-frame trailing (- right +readout-width+) (+ y 2)
                                          +readout-width+ +label-height+)))))))))

(defun make-form (width rows &key (minimum-height 0))
  "A view holding ROWS, laid out top down.

A row is (LABEL CONTROL &optional TRAILING), or one of:

  (:section TEXT)        a small bold heading with air around it
  (:group c1 c2 ...)     controls side by side, each sized to its title

and a fourth element on an ordinary row fixes the control's width, for the ones
with a natural size.
  (:gap)                  half a row of nothing

MINIMUM-HEIGHT keeps a short form filling its scroller, so that the background
is uniform whether a tab has six rows or eleven.

Hand-placed rather than Auto Layout: building NSLayoutConstraint objects three
at a time through a message-send bridge, to arrange a form whose shape is known
in advance and never changes, is more code than placing frames -- and this way
the coordinate arithmetic is in one function instead of forty."
  (let* ((heights (loop for row in rows
                        for index from 0
                        collect (row-height-of row :first (zerop index))))
         (height (max minimum-height
                      (+ (reduce #'+ heights :initial-value 0) (* 2 +form-margin+))))
         (view (make-instance 'form-view)))
    (set-frame view 0 0 width height)
    (let ((y +form-margin+))
      (loop for row in rows
            for step in heights
            for index from 0
            do (lay-out-row view row width
                            ;; A heading's own air is above it, so its label sits
                            ;; at the BOTTOM of the space the row reserves.
                            (if (eq (first row) :section)
                                (+ y (if (zerop index) 0 +section-lead+))
                                y))
               (incf y step)))
    view))
