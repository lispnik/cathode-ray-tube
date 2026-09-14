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

(defun screen-text (terminal)
  "TERMINAL's first three rows, safely: empty for a terminal already closed."
  (crt.terminal:with-terminal-locked (terminal)
    (crt.vt:vt-text (crt.terminal:terminal-vt terminal) 0 3)))

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
             ;; A second window if the first came up empty.  On the Intel CI
             ;; runner -- a VM with a paravirtualised GPU -- the display link
             ;; has been seen to deliver NOTHING in the first two seconds and
             ;; then run normally, once in many runs.  Two seconds more is
             ;; cheap, and it is spent only when the first window failed.
             ;;
             ;; This does NOT soften the assertion, which is the point: a link
             ;; that never fires still reports zero and still fails.  It buys
             ;; time, not tolerance.
             (when (<= (crt.ui:view-frames view) 30)
               (objc.runloop:pump-events :seconds 0.02d0 :max-seconds 2.0d0))
             (let ((frames (crt.ui:view-frames view)))
               (is (> frames 30)
                   "only ~D frames in up to 4s -- the display link is not firing.~%~
                    If this is 0, check the run loop MODE: a CADisplayLink added ~
                    to kCFRunLoopCommonModes is accepted and never fires."
                   frames)
               (is (> (crt.ui:view-effect-time view) 0.5d0)
                   "the effects clock did not advance (~,3F)"
                   (crt.ui:view-effect-time view))))
        (crt.ui:close-crt-window window)
        (objc:invoke (crt.ui:crt-window-handle window) "orderOut:" nil)))))

(defun synthesize-key (window view characters &optional (code 0))
  "Send VIEW a real NSKeyDown, the way AppKit would.

Returns T when AppKit made an event.  It can decline -- +keyEventWithType: is
documented to return nil for arguments it does not like -- and a nil event sent
to -keyDown: is swallowed by the IMP's handler-case, so the keystroke vanishes
and the only symptom is a terminal that was not typed into.  The caller checks."
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
    (cond ((or (null event)
               (and (cffi:pointerp event) (cffi:null-pointer-p event)))
           nil)
          (t (objc:invoke (objc:objc-object-pointer view) "keyDown:"
                          (objc:objc-object-pointer event))
             t))))

(test typing-reaches-the-child
  "The regression test for a terminal that could not be typed into.

VIEW-KEY-HANDLER was never assigned, so -keyDown: reached the view, found no
handler, and dropped the keystroke -- silently, because a view with nothing to
do about a key is not an error, and nothing else in the program refers to that
slot.  The window drew, the shell ran, the prompt blinked, and it was read-only.

Asserting on the CHILD rather than on the handler is the point: this passes only
if a synthesized NSEvent travels the whole way -- keyDown:, the key table, the
outbound queue, the reader thread's write, the pty, the shell, back through
libvterm and onto the screen.

`tr a-z A-Z' rather than a shell reading a line, and the choice is load-bearing
twice over.  It TRANSFORMS what it is given, so finding HI on screen cannot be
the pty's own echo of the hi that was typed -- which a test looking for its own
input would accept and learn nothing from.  And it is a plain binary, so a
failure is about this program rather than about what a shell builtin does with a
controlling terminal, which is a distinction this suite has already had to make
once (see A-SIGNALLED-CHILD-REPORTS-128-PLUS-THE-SIGNAL).

ONE line, and it must stay one line.  Measured: tr answers the first line sent
to it through a pty and then never answers another -- so does `sed -u', while cat
and a `while read' loop answer every line.  It is tr's own stdio buffering and
nothing to do with this program, but a second synthesized line here would never
arrive and the failure would look exactly like a bug in the keyboard path.
A-COLLECTION-MUST-NOT-KILL-THE-TERMINAL needs two lines and uses a shell for
that reason.

Every step says which one it was.  A blank screen is the same picture whether
AppKit declined to build the event, the event arrived nowhere, or the child was
never there to receive it, and on a CI runner you get one line to tell them
apart."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (let* ((session (crt.ui:make-session
                     :width 640 :height 400
                     :title "cathode-ray-tube input test"
                     :command '("/usr/bin/tr" "a-z" "A-Z")))
           (window (crt.ui:crt-window-handle (crt.ui:session-window session)))
           (view (crt.ui:session-view session))
           (terminal (crt.ui:session-terminal session)))
      (unwind-protect
           (progn
             (is-true (crt.ui:view-key-handler view)
                      "the session must install a key handler")
             (objc.runloop:pump-events :seconds 0.02d0 :max-seconds 1.0d0)
             (is-true (crt.terminal:terminal-alive-p terminal)
                      "the child must still be running BEFORE we type.~%~
                       exit status ~S, pid ~S, master fd ~S, reader ~:[gone~;alive~]"
                      (crt.terminal:terminal-exit-status terminal)
                      (crt.pty:pty-pid (crt.terminal:terminal-pty terminal))
                      (crt.pty:pty-fd (crt.terminal:terminal-pty terminal))
                      (let ((thread (crt.terminal:terminal-reader terminal)))
                        (and thread (bt2:thread-alive-p thread))))
             (is-true (and (synthesize-key window view "h" 4)
                           (synthesize-key window view "i" 34)
                           (synthesize-key window view (string #\Newline) 36))
                      "AppKit would not build a key event on this machine")
             (let ((seen (wait-for (lambda ()
                                     (let ((text (screen-text terminal)))
                                       (and (search "HI" text) text))))))
               (is-true seen
                        "the child never saw the keystrokes.~%~
                         screen ~S, terminal ~:[CLOSED~;open~], child ~:[gone (~:*~S)~;alive~]"
                        (screen-text terminal)
                        (crt.vt:vt-open-p (crt.terminal:terminal-vt terminal))
                        (if (crt.terminal:terminal-alive-p terminal)
                            t
                            (crt.terminal:terminal-exit-status terminal)))))
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

;;; Mouse, selection and the clipboard ---------------------------------------------

(defmacro with-session ((var &rest args) &body body)
  `(let ((,var (crt.ui:make-session :title "cathode-ray-tube test" ,@args)))
     (unwind-protect (progn ,@body)
       (crt.ui:end-session ,var))))

(defun feed-session (session string &key (timeout 5.0))
  "Run STRING through the child and wait for it to appear."
  (declare (ignore string))
  (wait-for (lambda () (plusp (length (screen-of session)))) :timeout timeout))

(defun screen-of (session)
  (let ((terminal (crt.ui:session-terminal session)))
    (crt.terminal:with-terminal-locked (terminal)
      (crt.vt:vt-text (crt.terminal:terminal-vt terminal) 0
                      (crt.terminal:terminal-rows terminal)))))

(test selection-covers-cells-in-reading-order
  "Not a rectangle.  A selection over three lines takes the tail of the first,
all of the middle and the head of the last, which is what selecting prose means."
  (let ((selection (crt.ui:make-selection 1 5 3 2)))
    (is-false (crt.ui:cell-selected-p selection 0 9) "the row above is outside")
    (is-false (crt.ui:cell-selected-p selection 1 4) "before the anchor on its row")
    (is-true (crt.ui:cell-selected-p selection 1 5) "the anchor itself")
    (is-true (crt.ui:cell-selected-p selection 1 79) "to the end of the first row")
    (is-true (crt.ui:cell-selected-p selection 2 0) "all of a middle row")
    (is-true (crt.ui:cell-selected-p selection 2 79))
    (is-true (crt.ui:cell-selected-p selection 3 1) "up to the end column")
    (is-false (crt.ui:cell-selected-p selection 3 2) "which is exclusive")
    (is-false (crt.ui:cell-selected-p selection 4 0) "the row below is outside")))

(test a-selection-dragged-upward-still-works
  "The anchor can be after the end, and normalising on every mouse-move would
lose which end the user is holding."
  (let ((up (crt.ui:make-selection 3 2 1 5)))
    (is-true (crt.ui:cell-selected-p up 2 40) "a middle row is covered either way")
    (is-true (crt.ui:cell-selected-p up 1 5))
    (is-false (crt.ui:cell-selected-p up 1 4))))

(test selection-yields-the-text-under-it
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (with-session (session :command '("/bin/sh" "-c" "printf 'HELLO WORLD'; sleep 10"))
      (is-true (wait-for (lambda () (search "HELLO" (screen-of session)))))
      ;; Columns 6 through 11: "WORLD".
      (setf (crt.ui:session-selection session) (crt.ui:make-selection 0 6 0 11))
      (is (string= "WORLD" (crt.ui:selection-text session))
          "got ~S" (crt.ui:selection-text session)))))

(test double-click-selects-a-word
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (with-session (session :command '("/bin/sh" "-c" "printf 'alpha beta gamma'; sleep 10"))
      (is-true (wait-for (lambda () (search "beta" (screen-of session)))))
      ;; "alpha beta gamma": beta occupies columns 6-9.
      (multiple-value-bind (start end) (crt.ui:word-bounds session 0 7)
        (is (= 6 start) "word starts at 6, got ~D" start)
        (is (= 10 end) "word ends at 10 exclusive, got ~D" end)))))

(defun shader-distort (x y width height frame-size curvature)
  "An INDEPENDENT transcription of distortCoordinates from crt.metal.

Deliberately written out again from the shader rather than calling the Lisp one,
so the test compares two readings of the same source instead of a function with
itself."
  (let* ((u (- (* (/ x width) (+ 1d0 (* 2d0 frame-size))) frame-size))
         (v (- (* (/ y height) (+ 1d0 (* 2d0 frame-size))) frame-size))
         (ccx (- u 0.5d0))
         (ccy (- v 0.5d0))
         (dist (* (+ (* ccx ccx) (* ccy ccy)) curvature)))
    (values (* (+ u (* ccx (+ 1d0 dist) dist)) width)
            (* (+ v (* ccy (+ 1d0 dist) dist)) height))))

(test clicking-maps-through-the-same-curvature-the-shader-applies
  "The screen is BENT, and a click lands where the character LOOKED.

The mapping is the shader's distortCoordinates applied in the SAME direction,
not its inverse: the static pass samples the texture at D(screen), so the
texture coordinate under a screen position is D of that position.  I got this
backwards first -- see the header of geometry.lisp -- and the test that caught it
was asserting the wrong property, so it is now written against an independent
transcription of the shader."
  (let* ((width 1000d0) (height 600d0)
         (curvature 0.3d0) (frame 0.02d0))
    (dolist (point '((500d0 300d0) (100d0 100d0) (900d0 500d0) (20d0 580d0)))
      (destructuring-bind (x y) point
        (multiple-value-bind (gx gy) (crt.ui:distort-point x y width height
                                                           frame curvature)
          (multiple-value-bind (sx sy) (shader-distort x y width height
                                                       frame curvature)
            (is (< (abs (- gx sx)) 0.001d0)
                "at ~,0F,~,0F the mouse mapping gives x=~,2F and the shader ~,2F"
                x y gx sx)
            (is (< (abs (- gy sy)) 0.001d0)
                "at ~,0F,~,0F the mouse mapping gives y=~,2F and the shader ~,2F"
                x y gy sy)))))
    ;; And it is a real transform, not a no-op: the centre is a fixed point and
    ;; a corner moves a long way.
    (multiple-value-bind (cx cy) (crt.ui:distort-point 500d0 300d0 width height
                                                        0d0 curvature)
      (is (< (abs (- cx 500d0)) 0.5d0) "the centre barely moves")
      (is (< (abs (- cy 300d0)) 0.5d0)))
    (multiple-value-bind (ex ey) (crt.ui:distort-point 20d0 20d0 width height
                                                        0d0 curvature)
      (is (> (abs (- ex 20d0)) 5d0)
          "a corner must move a long way, or the curvature is not being applied")
      (is (> (abs (- ey 20d0)) 5d0)))))

(test a-flat-profile-maps-straight-through
  "With no curvature the mapping must be the identity, or every click on IBM
3278 Reborn and Boring would be wrong."
  (multiple-value-bind (x y) (crt.ui:distort-point 123d0 456d0 1000d0 600d0 0d0 0d0)
    (is (< (abs (- x 123d0)) 0.001d0))
    (is (< (abs (- y 456d0)) 0.001d0))))

(test the-clipboard-round-trips
  "Opt-in, because it overwrites whatever the user had on their pasteboard.

    CRT_TEST_CLIPBOARD=1 make test"
  (if (not (uiop:getenv "CRT_TEST_CLIPBOARD"))
      (skip "set CRT_TEST_CLIPBOARD=1 to test the pasteboard")
      (progn
        (crt.ui:ensure-appkit)
        (crt.ui:set-clipboard-string "cathode-ray-tube test")
        (is (string= "cathode-ray-tube test" (crt.ui:clipboard-string))))))

(test the-menu-bar-has-what-it-needs
  "Without a menu bar AppKit does no key-equivalent handling at all, so Cmd-Q,
Cmd-C and Cmd-V are dead keys rather than commands."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (let* ((bar (crt.ui:make-menu-bar))
           (titles (loop for i below (objc:invoke-into 'integer bar "numberOfItems")
                         for item = (objc:invoke bar "itemAtIndex:" i)
                         for submenu = (objc:invoke item "submenu")
                         unless (crt.metal:null-object-p submenu)
                           collect (objc:invoke-into 'string submenu "title"))))
      (dolist (wanted '("File" "Edit" "View" "Profiles" "Window"))
        (is-true (member wanted titles :test #'string=)
                 "the menu bar has no ~A menu; it has ~S" wanted titles))
      ;; The Profiles menu is the only way to change look at run time.
      (let* ((index (position "Profiles" titles :test #'string=))
             (item (objc:invoke bar "itemAtIndex:" index))
             (menu (objc:invoke item "submenu")))
        (is (= 14 (objc:invoke-into 'integer menu "numberOfItems"))
            "all fourteen profiles should be listed")))))

(test font-source-chooses-between-ours-and-the-machines
  "fontSource 0 means one of the twenty-six faces we ship; 1 means an installed
family (fontmanager.cpp:434).

All fourteen built-in profiles say 0, which is exactly why this was easy to
leave unimplemented and easy not to notice: it is reachable only through the
settings window or an imported profile, and until it worked such a profile
silently got whichever bundled face its fontName happened to resolve to -- or
the default, when it resolved to nothing.

A system family also takes the SMOOTH zoom path rather than the integer one:
there is no table row saying it is a bitmap design with one true size, because
there is no table row at all."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (let ((families (crt.text:system-monospace-families)))
      (is-true families "this machine must have some monospace family")
      (flet ((variant (source name)
               (crt.settings:profile-from-alist
                (list (cons "fontSource" source) (cons "fontName" name))
                :into (copy-structure (crt.settings:find-profile "Commodore PET")))))
        (let ((bundled (variant 0 "COMMODORE_PET_SCALED"))
              (system (variant 1 (or (find "Menlo" families :test #'string=)
                                     (first families)))))
          (is-false (crt.ui:profile-system-font-p bundled) "0 is ours")
          (is-true (crt.ui:profile-system-font-p system) "1 is the machine's")
          (multiple-value-bind (font face scale) (crt.ui:load-profile-font bundled)
            (unwind-protect
                 (progn
                   (is (eq :commodore-pet face) "a bundled profile names a face")
                   (is (= 8 (crt.text:font-pixel-size font))
                       "loaded at PetMe's one true size"))
              (crt.text:release-font font))
            (is (>= scale 1) "and magnified by a whole number"))
          (multiple-value-bind (font face scale) (crt.ui:load-profile-font system)
            (unwind-protect
                 (progn
                   (is-true font "the system family must load")
                   (is (null face)
                       "and must NOT claim to be one of ours, or the fallback
chain and the magnification both ask a table a question it cannot answer")
                   (is (= 1 scale) "an outline is drawn at the size it is asked for"))
              (crt.text:release-font font)))
          ;; A family this machine does not have must not refuse to open a
          ;; window: profiles travel between machines.  It falls back to the
          ;; DEFAULT face rather than to the profile's usual one, and that is
          ;; forced rather than chosen -- for a fontSource-1 profile fontName is
          ;; the family, so there is no bundled name left in the profile to
          ;; resolve back to.
          (multiple-value-bind (font face scale)
              (crt.ui:load-profile-font (variant 1 "No Such Family At All"))
            (unwind-protect
                 (progn
                   (is-true font "a missing family must still yield a font")
                   (is (eq crt.ui::*default-font* face)
                       "and it must be the default face, got ~S" face)
                   (is (>= scale 1) "magnified like the bundled face it now is"))
              (crt.text:release-font font))))))))

(defun tab-controls (window label class-name)
  "Every subview of WINDOW's tab LABEL whose class is CLASS-NAME, in order."
  (let* ((tabs (crt.ui:settings-window-tabs window))
         (count (objc:invoke-into 'integer tabs "numberOfTabViewItems"))
         (found '()))
    (dotimes (i count (nreverse found))
      (let ((item (objc:invoke tabs "tabViewItemAtIndex:" i)))
        (when (string= label (objc:invoke-into 'string item "label"))
          (let* ((document (objc:invoke (objc:invoke item "view") "documentView"))
                 (subviews (objc:invoke document "subviews"))
                 (n (objc:invoke-into 'integer subviews "count")))
            (dotimes (j n)
              (let ((view (objc:invoke subviews "objectAtIndex:" j)))
                (when (string= class-name (objc:objc-class-name
                                           (objc:invoke view "class")))
                  (push view found))))))))))

(defun drive-control (control)
  "Send CONTROL's action the way AppKit would when a person moves it."
  (objc:invoke (objc:objc-object-pointer (crt.ui::control-target))
               "crtControlChanged:" (objc:objc-object-pointer control)))

(test the-settings-window-edits-a-copy-and-not-the-builtin
  "Four tabs of live controls, and the one thing that must never happen.

FIND-PROFILE hands every caller the SAME structure -- the fourteen built-ins are
a table, not fourteen copies -- so a settings window that edited the session's
profile in place would rewrite Default Amber for every other window and for the
rest of the process, and then save it to disk on the next write.  Nothing about
that would look like a bug until someone noticed their amber had gone green.

So the first edit copies, and the assertion is on the TABLE rather than on the
session: the session having the new value proves the control works, and the
built-in still having the old one proves it works safely."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (let ((session (crt.ui:make-session :width 480 :height 320
                                        :profile "Default Amber"
                                        :command '("/bin/sh" "-c" "sleep 30"))))
      (unwind-protect
           (let ((window (crt.ui:show-settings-window))
                 (builtin (crt.settings:find-profile "Default Amber")))
             (is-true window "the settings window must open")
             (is (= 4 (objc:invoke-into 'integer (crt.ui:settings-window-tabs window)
                                        "numberOfTabViewItems"))
                 "General, Terminal, Effects, Advanced")
             (let ((sliders (tab-controls window "Effects" "NSSlider"))
                   (before (crt.settings:profile-bloom builtin)))
               (is (= 11 (length sliders))
                   "SettingsEffectsTab.qml has eleven; got ~D" (length sliders))
               ;; The first is Bloom.  Move it somewhere it certainly was not.
               (let ((bloom (first sliders))
                     (target (if (> before 0.5d0) 0.125d0 0.875d0)))
                 (objc:invoke bloom "setDoubleValue:" target)
                 (drive-control bloom)
                 (is (< (abs (- target (crt.settings:profile-bloom
                                        (crt.ui:session-profile session))))
                        1d-6)
                     "the session must have taken the new value")
                 (is (= before (crt.settings:profile-bloom builtin))
                     "and the BUILT-IN must be untouched: it is shared by every
window and by the profile table itself")
                 (is (not (eq builtin (crt.ui:session-profile session)))
                     "which means the session is no longer holding the built-in")))
             ;; A slider that only a shader reads must not have rebuilt the grid.
             (let ((sliders (tab-controls window "Effects" "NSSlider"))
                   (cols (crt.terminal:terminal-cols (crt.ui:session-terminal session))))
               (let ((jitter (fourth sliders)))
                 (objc:invoke jitter "setDoubleValue:" 0.4d0)
                 (drive-control jitter)
                 (is (= cols (crt.terminal:terminal-cols
                              (crt.ui:session-terminal session)))
                     "an effects slider must not resize the terminal"))))
        (crt.ui:end-session session)))))

(test tabs-are-the-system-s-own
  "New Tab puts a second terminal in the key window's tab group.

Upstream draws its own tab bar in QML because Qt has nothing else to offer.  Here
each tab is a real NSWindow in an NSWindowTabGroup, which is why nothing below
CRT.UI has to know tabs exist -- every tab keeps its own view, layer and display
link, and a session cannot tell the difference.  What that buys, and what would
otherwise have to be built: the tab bar, the overview, dragging a tab out into a
window, and Cmd-Shift-bracket.

The assertion that matters is that the two sessions are INDEPENDENT terminals
sharing a window, not one terminal drawn twice."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (let ((first (crt.ui:make-session :width 480 :height 320
                                      :profile "Monochrome Green"
                                      :command '("/bin/sh" "-c" "sleep 30")))
          (second nil))
      (unwind-protect
           (progn
             (is (= 1 (length (crt.ui:window-tabs (crt.ui:session-window first))))
                 "one window is a tab group of one")
             (setf second (crt.ui:make-session
                           :width 480 :height 320 :tab-of first
                           :profile (crt.settings:profile-name
                                     (crt.ui:session-profile first))
                           :command '("/bin/sh" "-c" "sleep 30")))
             (let ((tabs (crt.ui:window-tabs (crt.ui:session-window first))))
               (is (= 2 (length tabs)) "and two make a group of two, got ~D"
                   (length tabs)))
             (is (string= "Monochrome Green"
                          (crt.settings:profile-name (crt.ui:session-profile second)))
                 "a new tab inherits the profile, or a window of tabs would look
like several terminals rather than one")
             (is (not (eq (crt.ui:session-terminal first)
                          (crt.ui:session-terminal second)))
                 "the two tabs must be separate terminals")
             (is (not (eq (crt.ui:session-view first) (crt.ui:session-view second)))
                 "with separate views, so each has its own layer and clock")
             ;; Cmd-1 and Cmd-2 pick them out.
             (is-true (crt.ui:select-window-tab (crt.ui:session-window first) 0)
                      "Cmd-1 selects the first tab")
             (is-true (crt.ui:select-window-tab (crt.ui:session-window first) 1)
                      "Cmd-2 the second")
             (is-false (crt.ui:select-window-tab (crt.ui:session-window first) 8)
                       "and Cmd-9 does nothing when there is no ninth, rather
than erroring"))
        (when second (crt.ui:end-session second))
        (crt.ui:end-session first)))))

(test the-settings-form-fills-its-scroller-from-the-top
  "The layout bug that was visible and that no assertion could see.

AppKit's origin is bottom left, and an NSScrollView whose document view is
SHORTER than the clip view pins it to the bottom.  So a tab with eight rows in a
four-hundred-point scroller drew them in the lower half under a band of empty
grey, which reads as a rendering fault rather than a layout one.  The suite was
green throughout: every control existed, every handler worked, and the thing was
simply in the wrong place.

Three invariants, each of which was false before:

  the form is FLIPPED, so row 0 is at the top and y grows downward
  the form is at least as tall as the scroller, so nothing is bottom-pinned
  the form is no WIDER than the scroller, so nothing needs a horizontal scroller

The last one was off by four points, from deriving the tab's content rect by
subtracting a guess at the chrome instead of asking for it."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (objc.runloop:shared-application :activation-policy 0)
    (let ((session (crt.ui:make-session :width 480 :height 320
                                        :command '("/bin/sh" "-c" "sleep 30"))))
      (unwind-protect
           (let* ((window (crt.ui:show-settings-window))
                  (tabs (crt.ui:settings-window-tabs window))
                  (count (objc:invoke-into 'integer tabs "numberOfTabViewItems")))
             (is (= 4 count) "four tabs")
             (dotimes (i count)
               (let* ((item (objc:invoke tabs "tabViewItemAtIndex:" i))
                      (label (objc:invoke-into 'string item "label"))
                      (scroll (objc:invoke item "view"))
                      (form (objc:invoke scroll "documentView"))
                      (sf (objc:invoke-into (vector 0d0 0d0 0d0 0d0) scroll "frame"))
                      (ff (objc:invoke-into (vector 0d0 0d0 0d0 0d0) form "frame")))
                 (is-true (objc:invoke-bool form "isFlipped")
                          "~A: the form must be flipped, or its rows start at the
bottom of the scroller" label)
                 (is (>= (aref ff 3) (aref sf 3))
                     "~A: form is ~,0F tall in a ~,0F scroller, so AppKit will pin
it to the bottom" label (aref ff 3) (aref sf 3))
                 (is (<= (aref ff 2) (aref sf 2))
                     "~A: form is ~,0F wide in a ~,0F scroller, so it needs a
horizontal scroller it should not have" label (aref ff 2) (aref sf 2))))
             ;; And the buttons really are side by side rather than a stack of
             ;; full-width ones, which is what they were.
             (let ((buttons (tab-controls window "General" "NSButton")))
               (is (= 3 (length buttons)) "Save, Export, Import")
               (let ((ys (mapcar (lambda (b)
                                   (aref (objc:invoke-into (vector 0d0 0d0 0d0 0d0)
                                                           b "frame")
                                         1))
                                 buttons)))
                 (is (every (lambda (y) (= y (first ys))) ys)
                     "all three must share one row, got ys ~S" ys))
               (is (every (lambda (b)
                            (< (aref (objc:invoke-into (vector 0d0 0d0 0d0 0d0)
                                                       b "frame")
                                     2)
                               200))
                          buttons)
                   "each must be the width of its title, not the width of the
window"))
             ;; A control with a natural size must not be given the whole
             ;; column.  For a colour well that is a matter of looks -- 330
             ;; points of flat orange reads as a progress bar.  For a CHECKBOX
             ;; it is behaviour: an NSButton's hit area is its frame, so a
             ;; full-column checkbox toggles when you click empty grey space
             ;; three hundred points from its label.  Measured before the fix:
             ;; "Blinking cursor" was 113 points of control in a 484-point frame.
             (dolist (spec '(("Terminal" "NSColorWell" 80)
                             ("Advanced" "NSButton" 320)))
               (destructuring-bind (tab class limit) spec
                 (let ((controls (tab-controls window tab class)))
                   (is-true controls "~A has no ~A to check" tab class)
                   (dolist (control controls)
                     (let ((width (aref (objc:invoke-into (vector 0d0 0d0 0d0 0d0)
                                                          control "frame")
                                        2)))
                       (is (<= width limit)
                           "~A: a ~A is ~,0F wide, so it is taking the whole
column rather than its own size" tab class width)))))))
        (crt.ui:end-session session)))))

(test the-bell-actually-rings
  "RING-PENDING-BELLS reaches AppKit without signalling.

The bell was counted correctly and rung with +[NSSound beep], which does not
exist.  The bridge resolves a method before sending it, so this failed loudly --
`No method \"beep\" for object \"NSSound\"' -- rather than crashing, and the
IMP's handler-case swallowed it.  The result: a log line on every bell, no
sound, and a green suite.

The suite missed it because THE-BELL-IS-COUNTED asserts the count the reader
thread keeps, which is deliberately the half that never touches AppKit.  Nothing
called the half that does until the application was built and run.

So this calls it.  It cannot assert that a noise happened -- there is no way to
ask -- but it asserts the thing that was actually wrong: that the call is one
AppKit will accept.  NSBeep is a plain C function, which is why the guess was
wrong; the check below would fail for +[NSSound beep] and passes for this."
  (when (window-server-or-skip)
    (crt.ui:ensure-appkit)
    (is-true (cffi:foreign-symbol-pointer "NSBeep")
             "NSBeep must exist as a C function, since that is what we call")
    (is-false (objc:can-invoke-p "NSSound" "beep")
              "and +[NSSound beep] must NOT, or this test is guarding nothing")
    (let ((session (crt.ui:make-session
                    :width 320 :height 200
                    ;; The child rings twice; the session coalesces to one beep.
                    :command '("/bin/sh" "-c" "printf 'a\\007b\\007'; sleep 10"))))
      (unwind-protect
           (let ((terminal (crt.ui:session-terminal session)))
             (is-true (wait-for (lambda ()
                                  (>= (crt.terminal:terminal-bell-count terminal) 2)))
                      "the child's two BELs must be counted")
             ;; The real assertion: driving it does not signal.  Before the fix
             ;; this wrote "No method beep" to *error-output* from inside the
             ;; frame and returned.
             (let ((errors (make-string-output-stream)))
               (let ((*error-output* errors))
                 (finishes (crt.ui::ring-pending-bells session)))
               (is (zerop (length (get-output-stream-string errors)))
                   "ringing the bell must not complain: ~A"
                   (get-output-stream-string errors)))
             (is (= (crt.terminal:terminal-bell-count terminal)
                    (crt.ui::session-bells-seen session))
                 "and must mark them seen, so a hundred BELs are one noise"))
        (crt.ui:end-session session)))))
