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

(defun wait-for (predicate &key (timeout 5.0) (interval 0.02))
  "Pump the event loop until PREDICATE answers, or time runs out.

PUMPING rather than sleeping: the reader thread delivers on its own, but the
session only notices during a frame, and frames only happen while the run loop
runs."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (let ((value (funcall predicate)))
        (when value (return value)))
      (when (> (get-internal-real-time) deadline) (return nil))
      (objc.runloop:pump-events :seconds interval :max-seconds interval))))

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

(defun synthesize-key (window view characters &optional (code 0))
  "Send VIEW a real NSKeyDown, the way AppKit would."
  (let ((event (objc:invoke
                "NSEvent"
                (concatenate 'string
                             "keyEventWithType:location:modifierFlags:timestamp:"
                             "windowNumber:context:characters:"
                             "charactersIgnoringModifiers:isARepeat:keyCode:")
                10                       ; NSEventTypeKeyDown
                (vector 0d0 0d0)
                0                        ; no modifiers
                0d0
                (objc:invoke-into 'integer window "windowNumber")
                (cffi:null-pointer)
                characters characters nil code)))
    (objc:invoke (objc:objc-object-pointer view) "keyDown:"
                 (objc:objc-object-pointer event))))

(test typing-reaches-the-child
  "The regression test for a terminal that could not be typed into.

VIEW-KEY-HANDLER was never assigned, so -keyDown: reached the view, found no
handler, and dropped the keystroke -- silently, because a view with nothing to
do about a key is not an error, and nothing else in the program refers to that
slot.  The window drew, the shell ran, the prompt blinked, and it was read-only.

Asserting on the CHILD rather than on the handler is the point: this passes only
if a synthesized NSEvent travels the whole way -- keyDown:, the key table, the
outbound queue, the reader thread's write, the pty, the shell, back through
libvterm and onto the screen."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (let* ((session (crt.ui:make-session
                     :width 640 :height 400
                     :title "cathode-ray-tube input test"
                     :command '("/bin/sh" "-c"
                                "read line; printf 'GOT[%s]' \"$line\"; sleep 10")))
           (window (crt.ui:crt-window-handle (crt.ui:session-window session)))
           (view (crt.ui:session-view session))
           (terminal (crt.ui:session-terminal session)))
      (unwind-protect
           (progn
             (is-true (crt.ui:view-key-handler view)
                      "the session must install a key handler")
             (objc.runloop:pump-events :seconds 0.02d0 :max-seconds 1.0d0)
             (synthesize-key window view "h" 4)
             (synthesize-key window view "i" 34)
             (synthesize-key window view (string #\Newline) 36)
             (let ((seen (wait-for (lambda ()
                                     (let ((text (crt.terminal:with-terminal-locked
                                                     (terminal)
                                                   (crt.vt:vt-text
                                                    (crt.terminal:terminal-vt terminal)
                                                    0 3))))
                                       (and (search "GOT[hi]" text) text))))))
               (is-true seen
                        "the child never saw the keystrokes; screen was ~S"
                        (crt.terminal:with-terminal-locked (terminal)
                          (crt.vt:vt-text (crt.terminal:terminal-vt terminal) 0 3)))))
        (crt.ui:end-session session)))))

(test special-keys-become-escape-sequences
  "Arrows and friends arrive as private-use codepoints in the 0xF700 block,
which is AppKit's way of not inventing an event type for them."
  (is (string= (format nil "~C[A" #\Escape)
               (crt.ui::function-key-sequence :up)))
  (is (string= (format nil "~C[D" #\Escape)
               (crt.ui::function-key-sequence :left)))
  (is (string= (format nil "~C[3~~" #\Escape)
               (crt.ui::function-key-sequence :delete)))
  (is (string= (format nil "~COP" #\Escape)
               (crt.ui::function-key-sequence :f1)))
  (is (null (crt.ui::function-key-sequence :not-a-key))))

(test a-new-window-opens-at-eighty-by-twenty-five
  "And the CHILD agrees, which is the half that matters.

A terminal that thinks it is 80x25 while the shell thinks otherwise is worse
than one that is simply the wrong size: line wrapping, curses redraws and
`clear' all go wrong in ways that look like the program's fault."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (let ((session (crt.ui:make-session
                    :title "cathode-ray-tube geometry test"
                    :command '("/bin/sh" "-c" "sleep 10"))))
      (unwind-protect
           (let ((terminal (crt.ui:session-terminal session)))
             (is (= 80 (crt.terminal:terminal-cols terminal)))
             (is (= 25 (crt.terminal:terminal-rows terminal)))
             ;; The kernel's idea of the pty, which is what the child reads.
             (multiple-value-bind (rows cols)
                 (crt.pty:get-winsize
                  (crt.pty:pty-fd (crt.terminal:terminal-pty terminal)))
               (is (= 25 rows) "the kernel thinks the pty has ~D rows" rows)
               (is (= 80 cols) "the kernel thinks the pty has ~D columns" cols)))
        (crt.ui:end-session session)))))

(test a-session-uses-the-face-its-profile-names
  "Half of what distinguishes the fourteen looks is the face.

The mapping existed and nothing called it: MAKE-SESSION took a :font defaulting
to IBM VGA and never consulted the profile, so all fourteen rendered in one
face.  That makes the port look far less faithful than its shader work is, and
it is invisible unless you compare two profiles that differ only in their font."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (dolist (name '("Default Amber" "Commodore PET" "Apple ][" "Plasma"))
      (let ((session (crt.ui:make-session
                      :profile name :title "cathode-ray-tube face test"
                      :command '("/bin/sh" "-c" "sleep 5"))))
        (unwind-protect
             (let* ((profile (crt.ui:session-profile session))
                    (wanted (crt.text:font-for-profile-name
                             (crt.settings:profile-font-name profile)))
                    (got (crt.text:font-handle (crt.ui:session-font session)))
                    (expected (crt.text:load-bundled-font wanted)))
               (unwind-protect
                    ;; Compared by CELL METRICS rather than by identity: two
                    ;; CTFontRefs for the same file are different objects, and
                    ;; the thing that actually matters is that the grid is the
                    ;; one this face implies.
                    (is (= (crt.text:font-cell-width expected)
                           (crt.text:font-cell-width (crt.ui:session-font session)))
                        "~A wants ~S; the session's face has a different cell width"
                        name wanted)
                 (crt.text:release-font expected))
               (is-true got))
          (crt.ui:end-session session))))))

(test the-margin-comes-from-the-profile
  "Text must not run to the edge of the virtual screen.

margin = lint(1, 40, _margin) + (1 - 1/sqrt(2)) * screenRadius, and the second
term is why: a rounded corner eats into the usable rectangle, so a heavily
rounded profile needs a bigger inset or its text runs under the bezel.  It was
being computed and never passed, so every profile had a margin of zero."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (let ((round (crt.ui:make-session :profile "Commodore PET"
                                      :command '("/bin/sh" "-c" "sleep 5")))
          (flat (crt.ui:make-session :profile "IBM 3278 Reborn"
                                     :command '("/bin/sh" "-c" "sleep 5"))))
      (unwind-protect
           (let ((round-margin (crt.text::text-renderer-margin
                                (crt.ui:session-renderer round)))
                 (flat-margin (crt.text::text-renderer-margin
                               (crt.ui:session-renderer flat))))
             (is (plusp flat-margin) "even a flat screen has some inset")
             (is (> round-margin flat-margin)
                 "a heavily rounded screen needs a bigger inset: PET ~,1F vs ~
                  3278 ~,1F" round-margin flat-margin))
        (crt.ui:end-session round)
        (crt.ui:end-session flat)))))
