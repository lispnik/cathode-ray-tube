;;;; src/ui/geometry.lisp -- from a mouse click to a cell.
;;;;
;;;; The screen the user sees is BENT, so a click lands where the character
;;;; LOOKED, not where it is.  The mapping back is the interesting part, and it
;;;; is not what it first appears.
;;;;
;;;; THE TRANSFORM IS APPLIED, NOT INVERTED, and that surprised me.  The static
;;;; pass does
;;;;
;;;;     curvatureCoords = distortCoordinates(qt_TexCoord0);
;;;;     colour          = texture(source, curvatureCoords);
;;;;
;;;; -- for a point on the SCREEN it samples the texture at D(screen).  So the
;;;; texture coordinate under a screen position is D of that position, and going
;;;; from a mouse click to a cell means applying D, the same function the shader
;;;; applies, in the same direction.  Not its inverse.
;;;;
;;;; Upstream calls it `correctDistortion' (PreprocessedTerminal.qml:252), which
;;;; is right in the sense of "correct this position into texture space" and
;;;; invites exactly the misreading I made.  I wrote it as an inverse, named it
;;;; UNDISTORT, and wrote a test asserting it barely moved anything -- which
;;;; failed, because it moves a corner by 56 pixels, because it is supposed to.
;;;; The code was right and the name and the test were both wrong.

(in-package #:cathode-ray-tube.ui)

(defun distort-point (x y width height frame-size curvature)
  "Map a point in DEVICE PIXELS from screen space into texture space.

X and Y are relative to the drawable's top-left; the result is where in the
terminal texture that screen position is showing.  This is the shader's
`distortCoordinates', applied in the same direction -- see the header."
  (let* ((u (/ x (max 1d0 width)))
         (v (/ y (max 1d0 height)))
         ;; The same padding the shaders apply before bending, so that a
         ;; profile with a frame is undone in the space the frame created.
         (u (- (* u (+ 1d0 (* frame-size 2d0))) frame-size))
         (v (- (* v (+ 1d0 (* frame-size 2d0))) frame-size))
         (cx (- 0.5d0 u))
         (cy (- 0.5d0 v))
         (distortion (* (+ (* cx cx) (* cy cy)) curvature)))
    (values (* (- u (* cx (+ 1d0 distortion) distortion)) width)
            (* (- v (* cy (+ 1d0 distortion) distortion)) height))))

(defun view-point-to-cell (session point-x point-y &key clamp)
  "The cell under a point given in VIEW coordinates (points, top-left origin).

Returns (values COL ROW INSIDE-P).  INSIDE-P is false when the point is off the
grid -- over the bezel, or past the last row -- which the caller needs in order
to tell a click on the screen from a click on the plastic.

CLAMP pins the result to the grid instead, which is what a DRAG wants: once a
selection has started, dragging onto the bezel should extend to the edge rather
than stop tracking."
  (let* ((view (session-view session))
         (renderer (session-renderer session))
         (profile (session-profile session))
         (scale (screen-backing-scale)))
    (destructuring-bind (width height) (view-drawable-size view)
      (let* ((x (* point-x scale))
             (y (* point-y scale))
             (window-scale (crt.settings:normalized-window-scale width height))
             (curvature (* (crt.settings:profile-screen-curvature profile)
                           crt.settings:+screen-curvature-size+ window-scale))
             (frame-size (* (crt.settings:frame-size profile) window-scale)))
        (multiple-value-bind (ux uy)
            (distort-point x y (float width 1d0) (float height 1d0)
                           frame-size curvature)
          (let* ((margin (crt.text::text-renderer-margin renderer))
                 (cw (crt.text:text-renderer-cell-width renderer))
                 (ch (crt.text:text-renderer-cell-height renderer))
                 (col (floor (- ux margin) cw))
                 (row (floor (- uy margin) ch))
                 (cols (crt.text:text-renderer-cols renderer))
                 (rows (crt.text:text-renderer-rows renderer))
                 (inside (and (<= 0 col) (< col cols) (<= 0 row) (< row rows))))
            (if clamp
                (values (min (1- cols) (max 0 col)) (min (1- rows) (max 0 row)) inside)
                (values col row inside))))))))
