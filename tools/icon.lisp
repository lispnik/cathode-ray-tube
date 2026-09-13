;;;; tools/icon.lisp -- the application icon, drawn rather than checked in.
;;;;
;;;; Using the same Objective-C bindings the application is built on, so there
;;;; is no artwork in the repository and no asset pipeline: changing the icon is
;;;; an edit to this file.  utc-status-app does the same and for the same
;;;; reason.
;;;;
;;;;     make icon      -> res/icon.png
;;;;
;;;; A rounded amber screen on a dark bezel, with scanlines and a prompt: the
;;;; picture the application makes, at 1024 pixels.

(defpackage #:crt-icon
  (:use #:cl)
  (:export #:render-icon))

(in-package #:crt-icon)

(defun render-icon (path &key (size 1024))
  (crt.metal:ensure-frameworks)
  (float-features:with-float-traps-masked (:invalid :divide-by-zero :overflow)
    (objc:with-autorelease-pool ()
      (let* ((image (objc:invoke (objc:invoke "NSImage" "alloc") "initWithSize:"
                                 (vector (float size 1d0) (float size 1d0)))))
        (objc:invoke image "lockFocus")
        (draw-icon size)
        (objc:invoke image "unlockFocus")
        (let* ((tiff (objc:invoke image "TIFFRepresentation"))
               (rep (objc:invoke "NSBitmapImageRep" "imageRepWithData:" tiff))
               (png (objc:invoke rep "representationUsingType:properties:"
                                 4 (objc:invoke "NSDictionary" "dictionary")))
               (length (objc:invoke-into 'integer png "length"))
               (bytes (objc:invoke-into :pointer png "bytes")))
          (ensure-directories-exist path)
          (with-open-file (out path :direction :output
                                    :element-type '(unsigned-byte 8)
                                    :if-exists :supersede)
            (dotimes (i length)
              (write-byte (cffi:mem-aref bytes :uint8 i) out)))
          path)))))

(defun colour (r g b &optional (a 1.0))
  (objc:invoke "NSColor" "colorWithCalibratedRed:green:blue:alpha:"
               (float r 1d0) (float g 1d0) (float b 1d0) (float a 1d0)))

(defun rounded-rect (x y w h radius)
  (objc:invoke "NSBezierPath" "bezierPathWithRoundedRect:xRadius:yRadius:"
               (vector (float x 1d0) (float y 1d0) (float w 1d0) (float h 1d0))
               (float radius 1d0) (float radius 1d0)))

(defun draw-icon (size)
  "A dark bezel, an amber screen, a prompt.

Deliberately NOT a picture of the effect chain.  An icon is looked at 32 pixels
wide in a Dock and 16 in a menu; scanlines and bloom at that size are noise, and
the first attempt -- which had them -- came out as a grey barcode.  What reads
small is the SHAPE: a rounded tube behind a heavy bezel, and one bright amber
mark on it."
  (let* ((s (float size 1d0))
         (inset (* s 0.055d0))
         (bezel-radius (* s 0.22d0))
         (screen-inset (* s 0.155d0))
         (screen-radius (* s 0.13d0)))
    ;; The bezel.
    (let ((bezel (rounded-rect inset inset (- s (* 2 inset)) (- s (* 2 inset))
                               bezel-radius)))
      (objc:invoke (colour 0.11 0.11 0.12) "setFill")
      (objc:invoke bezel "fill")
      ;; A highlight along the top edge, which is what makes it read as moulded
      ;; plastic rather than a flat square at small sizes.  CLIPPED to the bezel:
      ;; a plain rectangle here has square corners and pokes out past the
      ;; bezel's much larger radius, which at icon size looks like a rendering
      ;; bug rather than a highlight.
      (objc:invoke "NSGraphicsContext" "saveGraphicsState")
      (objc:invoke bezel "addClip")
      (objc:invoke (colour 0.30 0.30 0.32 0.55) "setFill")
      (objc:invoke (rounded-rect inset (- s inset (* s 0.045d0))
                                 (- s (* 2 inset)) (* s 0.045d0)
                                 (* s 0.02d0))
                   "fill")
      (objc:invoke "NSGraphicsContext" "restoreGraphicsState"))
    ;; The screen.
    (let ((screen (rounded-rect screen-inset screen-inset
                                (- s (* 2 screen-inset)) (- s (* 2 screen-inset))
                                screen-radius)))
      (objc:invoke (colour 0.05 0.032 0.012) "setFill")
      (objc:invoke screen "fill")
      (objc:invoke "NSGraphicsContext" "saveGraphicsState")
      (objc:invoke screen "addClip")
      ;; The phosphor glow behind the prompt, as a few widening translucent
      ;; rounded rectangles -- a gradient without needing NSGradient.
      (loop for i from 6 downto 1
            for spread = (* s 0.012d0 i)
            do (objc:invoke (colour 1.0 0.51 0.0 0.045) "setFill")
               (objc:invoke (rounded-rect (- (* s 0.285d0) spread)
                                          (- (* s 0.435d0) spread)
                                          (+ (* s 0.34d0) (* 2 spread))
                                          (+ (* s 0.105d0) (* 2 spread))
                                          (+ (* s 0.02d0) spread))
                            "fill"))
      ;; The prompt: a chevron and a cursor block.
      (objc:invoke (colour 1.0 0.55 0.05 1.0) "setStroke")
      (let ((chevron (objc:invoke "NSBezierPath" "bezierPath"))
            (x (* s 0.30d0)) (y (* s 0.49d0)) (w (* s 0.075d0)) (h (* s 0.075d0)))
        (objc:invoke chevron "setLineWidth:" (* s 0.035d0))
        (objc:invoke chevron "setLineCapStyle:" 1)    ; round
        (objc:invoke chevron "setLineJoinStyle:" 1)
        (objc:invoke chevron "moveToPoint:" (vector x (+ y h)))
        (objc:invoke chevron "lineToPoint:" (vector (+ x w) y))
        (objc:invoke chevron "lineToPoint:" (vector x (- y h)))
        (objc:invoke chevron "stroke"))
      (objc:invoke (colour 1.0 0.62 0.15 1.0) "setFill")
      (objc:invoke (rounded-rect (* s 0.45d0) (* s 0.425d0)
                                 (* s 0.20d0) (* s 0.13d0) (* s 0.018d0))
                   "fill")
      (objc:invoke "NSGraphicsContext" "restoreGraphicsState"))))
