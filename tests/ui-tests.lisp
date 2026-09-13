;;;; tests/ui-tests.lisp -- Tier 3: needs a window server.
;;;;
;;;; GitHub's macOS runners have one; objc's own GUI suite runs on them and its
;;;; environment step prints `window server: T'.  Where there is none these skip,
;;;; and the skip count is printed, so a runner that changes shows up as fewer
;;;; checks rather than as a mystery.
;;;;
;;;; This is the smoke test that catches "the whole thing deadlocked": a window,
;;;; a layer, a display link, and frames actually arriving.

(in-package #:cathode-ray-tube/tests)
(in-suite ui)

(defun window-server-or-skip ()
  (cond ((not (objc.runloop:window-server-p))
         (skip "no window server") nil)
        ((not (crt.metal:metal-available-p))
         (skip "no Metal device") nil)
        (t t)))

(test window-draws-frames-at-vsync
  "Open a window, run the clock, and count frames.

The assertion is on the RATE, loosely: anything above 30 frames in two seconds
means the display link is firing on its own rather than being driven by us.  It
is deliberately not tight -- a busy runner is allowed to drop frames, and the
failure this guards against is zero."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (let* ((window (crt.ui:make-crt-window :width 320 :height 200
                                       :title "cathode-ray-tube tests"
                                       :draw-function #'crt.ui:gradient-frame))
           (view (crt.ui:crt-window-view window)))
      (unwind-protect
           (progn
             ;; The drawable is in DEVICE pixels, so on a Retina display it is
             ;; twice the view's size.  Asserting it is at least the view size
             ;; catches a backing-scale mistake without assuming the scale.
             (destructuring-bind (dw dh) (crt.ui:view-drawable-size view)
               (is (>= dw 320) "drawable width ~D is smaller than the view" dw)
               (is (>= dh 200) "drawable height ~D is smaller than the view" dh))
             (crt.ui:show-crt-window window)
             (objc.runloop:pump-events :seconds 0.02d0 :max-seconds 2.0d0)
             (let ((frames (crt.ui:view-frames view)))
               (is (> frames 30)
                   "only ~D frames in 2s -- the display link is not firing.~%~
                    If this is 0, check the run loop MODE: a CADisplayLink added ~
                    to kCFRunLoopCommonModes is accepted and never fires."
                   frames)
               (is (> (crt.ui:view-effect-time view) 0.5d0)
                   "the effects clock did not advance (~,3F)"
                   (crt.ui:view-effect-time view))))
        (crt.ui:close-crt-window window)
        (objc:invoke (crt.ui:crt-window-handle window) "orderOut:" nil)))))
