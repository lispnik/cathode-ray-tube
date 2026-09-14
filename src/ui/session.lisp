;;;; src/ui/session.lisp -- a window, a terminal, and the frame that joins them.
;;;;
;;;; The draw path, once per display-link tick, all on the main thread:
;;;;
;;;;   take a snapshot under the lock  ->  build instances  ->  upload new glyphs
;;;;   ->  text pass  ->  blit to the drawable  ->  present
;;;;
;;;; The lock is held only for the snapshot, which copies the dirty rows into
;;;; structures it already owns.  Everything after it -- CoreText, Metal, the
;;;; drawable -- happens with the lock released, because holding a lock across a
;;;; GPU submission would make the pty reader wait on the display.

(in-package #:cathode-ray-tube.ui)

(defstruct (session (:constructor %make-session))
  window view terminal renderer font
  graph profile
  (sampler nil)
  (margin 8.0 :type single-float)
  (title nil)
  ;; The bell count last seen on the main thread.  VT-BELL-COUNT is incremented
  ;; by the reader thread, and the difference is how many bells arrived since
  ;; the last frame.
  (bells-seen 0 :type unsigned-byte)
  ;; The transient COLUMNSxROWS readout, drawn OVER the finished frame.
  (overlay nil)
  (selection nil)
  (dragging nil)
  (scale 1 :type (integer 1 16))
  ;; Zoom, as a multiplier on whatever the face would otherwise be drawn at.
  ;; Upstream's fontScaling runs 0.25 to 2.50 in steps of 0.05; the steps here
  ;; are coarser for the bitmap faces because their magnification is an integer
  ;; -- see APPLY-FONT-SCALING.
  (font-scaling 1.0d0 :type double-float)
  ;; With no effects the text target is blitted straight to the drawable.  Not
  ;; scaffolding: it is the honest "effects off" path, and it is what the
  ;; Boring profile amounts to.
  (effects t))

(defvar *sessions* '())

(defun session-for-view (view)
  (find view *sessions* :key #'session-view))

;;; Geometry --------------------------------------------------------------------

(defun fit-terminal-to-view (session)
  "Rebuild the text renderer for the view's size and tell the terminal.

Both halves matter and they fail differently: the renderer's grid is ours and
cannot fail, while TIOCSWINSZ is the kernel's and can, so TERMINAL-RESIZE does
them in that order and separately."
  (let* ((view (session-view session))
         (renderer (session-renderer session)))
    (destructuring-bind (width height) (view-drawable-size view)
      (multiple-value-bind (cols rows)
          (crt.text:resize-text-renderer renderer width height)
        (crt.terminal:terminal-resize (session-terminal session) rows cols)
        (when (session-graph session)
          (crt.effects:resize-graph (session-graph session) width height))
        ;; The readout starts its second here, and only on a grid that actually
        ;; changed -- OVERLAY-NOTE-SIZE decides that, so dragging a window edge
        ;; three pixels without crossing a cell boundary does not keep the
        ;; overlay alive for as long as the mouse is down.
        (let ((overlay (session-overlay session)))
          (when overlay (crt.text:overlay-note-size overlay cols rows)))
        (values cols rows)))))

;;; The frame -------------------------------------------------------------------

(defun draw-session (view texture drawable time)
  "One frame.  Called on the main thread by the display link."
  (declare (ignore time))
  (let ((session (session-for-view view)))
    (when session
      ;; A resize is acted on here rather than in -setFrameSize:, so that the
      ;; expensive part -- reallocating the target, the grid and the instance
      ;; buffer -- happens once per frame at most however fast the user drags a
      ;; window edge.
      (when (view-resized-p view)
        (setf (view-resized-p view) nil)
        (fit-terminal-to-view session))
      ;; The title is POLLED rather than pushed.  It changes rarely, comparing
      ;; two strings once a frame costs nothing, and it keeps the reader thread
      ;; from having to reach AppKit at all -- which is one fewer thing that can
      ;; be wrong about threads.
      (update-session-title session)
      (ring-pending-bells session)
      (let* ((terminal (session-terminal session))
             (snapshot (crt.terminal:terminal-snapshot terminal))
             (renderer (session-renderer session))
             ;; The terminal's own colours, which the effect chain then recolours
             ;; through convertWithChroma.  White on black is deliberate: the
             ;; profile's foreground and background are applied in the DYNAMIC
             ;; pass, and painting them here as well would apply them twice.
             (clock (view-effect-time view))
             (target (crt.text:render-text
                      renderer snapshot
                      :default-fg '(255 255 255)
                      :default-bg '(0 0 0)
                      :blink-on (blink-phase clock)
                      ;; A cursor that does not blink is always on.  The profile
                      ;; decides, and thirteen of the fourteen say no -- a
                      ;; steady block is what a phosphor tube looks like.
                      :cursor-on (or (not (crt.settings:profile-blinking-cursor
                                           (session-profile session)))
                                     (blink-phase clock))
                      :selected-p (let ((selection (session-selection session)))
                                    (when selection
                                      (lambda (row col)
                                        (cell-selected-p selection row col)))))))
        (multiple-value-bind (vw vh) (crt.text:text-renderer-virtual-size renderer)
          ;; The overlay is drawn ON TOP of the finished frame, so whichever pass
          ;; would have handed the drawable over must not: the last pass to touch
          ;; it presents it, and that is the overlay when there is one.
          (let* ((overlay (session-overlay session))
                 (overlaid (and overlay (crt.text:overlay-visible-p overlay)))
                 (present (unless overlaid drawable)))
            (if (and (session-effects session) (session-graph session))
                (crt.effects:render-effects
                 (session-graph session) target texture
                 :drawable drawable :present present
                 :time (crt.ui:view-effect-time view)
                 :painted (crt.terminal:snapshot-painted snapshot)
                 ;; The terminal's grid in NATIVE font pixels -- not the
                 ;; drawable's size, and not the magnified cell either.  See
                 ;; TEXT-RENDERER-VIRTUAL-SIZE for what passing the magnified one
                 ;; does, which is to switch rasterisation off everywhere.
                 :virtual-width vw :virtual-height vh)
                (blit-to-drawable session target texture drawable :present present))
            (when overlaid
              (destructuring-bind (dw dh) (view-drawable-size view)
                (crt.text:render-overlay overlay renderer texture dw dh
                                         :drawable drawable)))))))))

(defconstant +blink-period+ 1.0d0
  "Seconds for a full blink cycle: half on, half off.

Driven by the EFFECT clock, which is frame-skipped to about 20Hz, so the
transition lands on an effect tick rather than on a frame -- the same
quantisation everything else animated in this program uses.")

(defun blink-phase (time)
  "True during the lit half of the blink cycle."
  (< (mod time +blink-period+) (/ +blink-period+ 2)))

(defun update-session-title (session)
  (let ((title (crt.terminal:terminal-title (session-terminal session))))
    (when (and title (not (equal title (session-title session))))
      (setf (session-title session) title)
      (objc:invoke (crt-window-handle (session-window session)) "setTitle:" title))))

(defun ring-pending-bells (session)
  "Sound the bells the child asked for since the last frame.

POLLED on the main thread rather than rung from the callback, for the same
reason the title is: the bell callback runs on the reader thread, and NSBeep is
AppKit.

Coalesced to at most one beep per frame on purpose.  A program that emits a
hundred BELs -- a `cat' over a binary is the classic -- would otherwise queue a
hundred system sounds and go on making noise long after it finished.  Counting
them and beeping once is what a terminal is expected to do.

An audible bell rather than a visual flash because upstream's is Konsole's, in
qmltermwidget, which is not in this tree to read; a beep is the behaviour
everything else on this machine has.

NSBeep is a plain C FUNCTION, not a method on anything.  The obvious guess --
+[NSSound beep] -- does not exist, and the bridge says so rather than crashing:
`No method \"beep\" for object \"NSSound\"'.  That was the guess, it shipped, and
it fired on the first bell of the first run of the installed app; the suite
missed it because the bell test asserts the COUNT the reader thread keeps and
never reaches AppKit."
  (let* ((terminal (session-terminal session))
         (count (and terminal (crt.terminal:terminal-bell-count terminal))))
    (when (and count (> count (session-bells-seen session)))
      (setf (session-bells-seen session) count)
      (cffi:foreign-funcall "NSBeep" :void))))

(defun blit-to-drawable (session source texture drawable &key (present drawable))
  "Copy the text target to the window.

Until the effect chain lands this is the whole of the output; afterwards it
remains the honest `effects off' path."
  (let ((pipeline (crt.metal:pipeline
                   :fragment "blit_fragment"
                   :pixel-format crt.metal:+pixel-format-bgra8unorm+
                   :label "blit"))
        (sampler (or (session-sampler session)
                     (setf (session-sampler session)
                           (crt.metal:make-sampler
                            :min crt.metal:+filter-nearest+
                            :mag crt.metal:+filter-nearest+
                            :label "blit")))))
    (crt.metal:with-render-pass (encoder texture
                                 :clear '(0d0 0d0 0d0 1d0)
                                 :present present
                                 :label "blit")
      (crt.metal:use-pipeline encoder pipeline)
      (crt.metal:bind-fragment-texture encoder source 0)
      (crt.metal:bind-fragment-sampler encoder sampler 0)
      (crt.metal:draw-quad encoder))))

;;; Starting one ------------------------------------------------------------------

(defparameter *default-font* :ibm-vga-8x16
  "Which bundled face to open with.  The font manager maps a profile's fontName
to one of these in M4; until then it is the face IBM VGA 8x16 names.")

(defparameter *default-profile* "Default Amber"
  "The profile a new window opens with -- cool-retro-term's own default.")

(defconstant +default-line-spacing+ 0.1d0
  "The leading all fourteen built-in profiles ask for.

Only a fallback now, for the paths that have no profile in hand.  The profile's
own LINE-SPACING is what the renderer is built with -- see PROFILE-METRICS --
which matters for an imported or hand-written profile and for nothing else,
since every built-in one says 0.1.")

(defun profile-metrics (profile)
  "(values LINE-SPACING FONT-WIDTH) for PROFILE, as the text renderer wants them.

Two of the twenty-seven profile keys that reach the glyph grid rather than a
shader, and the pair that decides the SIZE OF A CELL -- so every call that
measures, fits or builds a grid has to agree on them or a resize shifts every
glyph.  One reader, used by all of them."
  (if profile
      (values (float (crt.settings:profile-line-spacing profile) 1d0)
              (float (crt.settings:profile-font-width profile) 1d0))
      (values +default-line-spacing+ 1.0d0)))

(defparameter *default-columns* 80)
(defparameter *default-rows* 25
  "The grid a new window opens at.

80x25 rather than 80x24: 25 is what an IBM VGA text mode actually was, and this
program's default face is PxPlus_IBM_VGA_8x16.  A window is still free to be any
size -- the grid follows it after the first resize -- but it OPENS at the size
the thing it is imitating had.")

(defun screen-backing-scale ()
  "The main screen's backing scale, needed BEFORE there is a window to ask.

2 on a Retina display.  It decides the magnification, and therefore the window
size, so it has to be known before the window exists."
  (let ((screen (objc:invoke "NSScreen" "mainScreen")))
    (if (crt.metal:null-object-p screen)
        1
        (max 1 (round (objc:invoke-into 'double-float screen "backingScaleFactor"))))))

(defun font-scale-for (font-name)
  "How far to magnify FONT-NAME's glyphs.

The backing scale for a low-resolution face, so its native pixels come out
square and one glyph pixel covers one device pixel per step; 1 for the modern
faces, which are outlines and are rasterised at the size they are drawn."
  (if (crt.text::bundled-font-low-resolution-p font-name)
      (screen-backing-scale)
      1))

(defun default-session-command ()
  "The command a new terminal runs when nobody named one: the custom command if
the settings ask for one, otherwise NIL and the login shell.

Here rather than only in APPLY-OPTIONS, because APPLY-OPTIONS runs once at
startup and New Window and New Tab do not go through it -- so a custom command
was honoured by the first terminal of a session and by none of the others, which
is the sort of difference nobody reports as a bug and everybody notices."
  (let ((settings crt.settings:*settings*))
    (when (crt.settings:settings-use-custom-command settings)
      (let ((text (crt.settings:settings-custom-command settings)))
        (when (plusp (length text))
          ;; Fully qualified: the parser lives in the CATHODE-RAY-TUBE
          ;; package, below the seam, because a command line is arithmetic over
          ;; strings.  CRT.UI does not use that package and should not start.
          (cathode-ray-tube:tokenize-command-line text))))))

(defun profile-system-font-p (profile)
  "True when PROFILE's fontName names an INSTALLED family rather than one of ours.

fontSource is 0 for bundled and 1 for system (fontmanager.cpp:434), and all
fourteen built-in profiles say 0 -- so this is reachable only through the
settings window or an imported profile, which is exactly why it was easy to
leave unimplemented and easy not to notice."
  (and profile (eql 1 (crt.settings:profile-font-source profile))))

(defun profile-font (profile &optional override)
  "The bundled face PROFILE asks for.

OVERRIDE wins when given.  Otherwise the profile's own fontName decides, which
is half of what distinguishes the fourteen looks -- Commodore PET is PetMe and
Apple ][ is PrintChar21, and rendering both in IBM VGA makes the port look far
less faithful than its shader work actually is.  A name this build does not
ship falls back to the default rather than refusing to open a window, since a
profile written by a newer cool-retro-term may name a face we do not have.

Meaningless for a fontSource-1 profile, whose fontName is a family this machine
has rather than a face we ship: LOAD-PROFILE-FONT is what decides between them."
  (or override
      (crt.text:font-for-profile-name (crt.settings:profile-font-name profile))
      *default-font*))

(defun load-profile-font (profile &key override pixel-size)
  "(values FONT FACE SCALE) for PROFILE: what to draw with and how far to magnify.

FACE is the bundled keyword, or NIL for a system family -- which is what tells
the fallback chain and the magnification apart, since neither has an answer for
a family we know nothing about.

A system family that will not load falls back rather than refusing to open the
window -- a profile naming a font the machine it was written on happened to have
is a thing that travels between machines, and the wrong font is a much better
outcome than no terminal.  It falls back to the DEFAULT face and not to the
profile's usual one, because for a fontSource-1 profile fontName IS the family:
there is no bundled name left in the profile to resolve."
  (let ((face (profile-font profile override)))
    (flet ((system-font ()
             (when (and (null override) (profile-system-font-p profile))
               (let ((family (crt.settings:profile-font-name profile)))
                 (when (and family (plusp (length family)))
                   (crt.text:load-family-font
                    family (or pixel-size
                               (crt.text:bundled-font-native-size *default-font*))))))))
      (let ((system (system-font)))
        (cond
          ;; Scale 1: a system family is an outline rasterised at the size it is
          ;; drawn, so there is nothing to magnify by a whole number.  FACE NIL
          ;; says "not one of ours", which is what the fallback chain needs to
          ;; know, since it has no table entry for a family we know nothing about.
          (system (values system nil 1))
          (t (let ((font (crt.text:load-bundled-font face :pixel-size pixel-size)))
               (unless font
                 (error "Could not load a font for profile ~S."
                        (crt.settings:profile-name profile)))
               (values font face (font-scale-for face)))))))))

(defun make-session (&key width height (columns *default-columns*)
                          (rows *default-rows*)
                          (command (default-session-command)) directory
                          font (title "cathode-ray-tube")
                          (profile *default-profile*) (effects t) tab-of)
  "A window running a shell.  Main thread only.

Sized from COLUMNS by ROWS unless WIDTH and HEIGHT say otherwise -- the opposite
of the way the grid is computed afterwards, and deliberately so: a terminal
should OPEN at a familiar number of characters and only then start following
whatever size the window is dragged to."
  (ensure-appkit)
  (let* ((chosen (or (crt.settings:find-profile profile)
                     (error "No profile named ~S." profile)))
         ;; The PROFILE is resolved first, because it is what decides the face,
         ;; and the face is what decides the cell size, which decides the window.
         (margin (crt.settings:margin chosen)))
    (multiple-value-bind (loaded face scale) (load-profile-font chosen :override font)
    ;; Device pixels for the grid, then points for the window: AppKit sizes
    ;; windows in points and the glyphs are in device pixels, and conflating the
    ;; two gives a window half the size it should be on a Retina display.
    (unless (and width height)
      (multiple-value-bind (pixel-width pixel-height)
          (multiple-value-bind (line-spacing font-width) (profile-metrics chosen)
            (crt.text:grid-pixel-size loaded columns rows :scale scale
                                      :margin margin
                                      :line-spacing line-spacing
                                      :font-width font-width))
        (let ((backing (screen-backing-scale)))
          (setf width (ceiling pixel-width backing)
                height (ceiling pixel-height backing)))))
    (make-session-in-window loaded face scale margin width height title command
                            directory chosen effects tab-of))))

(defconstant +min-font-scaling+ 0.25d0)
(defconstant +max-font-scaling+ 2.5d0)

(defun make-session-in-window (loaded face scale margin width height title command
                               directory profile effects &optional tab-of)
  (let* ((window (make-crt-window :width width :height height :title title
                                  :draw-function #'draw-session))
         (view (crt-window-view window)))
    ;; TimeManager.qml advances the effect clock every Nth frame rather than
    ;; every frame, and the quantisation is part of the look: flicker reads as
    ;; chunky rather than smooth.  The view defaulted to upstream's 3 and there
    ;; was no way to reach the setting that holds it.
    (setf (view-frame-skip view)
          (crt.settings:settings-effects-frame-skip crt.settings:*settings*))
    (destructuring-bind (dw dh) (view-drawable-size view)
      (multiple-value-bind (line-spacing font-width) (profile-metrics profile)
      (let* ((renderer (crt.text:make-text-renderer
                        :font loaded :width dw :height dh
                        :scale scale
                        :margin margin
                        :line-spacing line-spacing
                        :font-width font-width
                        :fallbacks (crt.text:font-fallback-chain
                                    face
                                    :pixel-size (crt.text:font-pixel-size loaded))))
             (session (%make-session :window window :view view
                                     :renderer renderer :font loaded
                                     :margin (float margin 1.0)
                                     :scale scale
                                     :profile profile :effects effects)))
        (when effects
          (setf (session-graph session)
                (crt.effects:make-graph :profile profile :width dw :height dh)))
        ;; The grid MAKE-TEXT-RENDERER already fitted, rather than a second
        ;; TEXT-GRID-SIZE with the same arguments: two computations of the same
        ;; number are two chances to disagree, and the one that decides how big
        ;; the child thinks it is has to be the one the glyphs are drawn on.
        (let ((cols (crt.text:text-renderer-cols renderer))
              (rows (crt.text:text-renderer-rows renderer)))
          (setf (session-terminal session)
                (crt.terminal:make-terminal
                 :rows rows :cols cols :command command :directory directory
                 ;; The reader thread is not the main thread, and closing a
                 ;; window from anywhere else is a crash waiting for a quiet
                 ;; afternoon.
                 :on-exit (lambda (terminal status)
                            (declare (ignore terminal status))
                            (on-main-thread (lambda () (end-session session)))))))
        ;; WITHOUT THIS THE TERMINAL IS READ-ONLY.  -keyDown: reaches the view,
        ;; the view looks for a handler, finds NIL, and drops the keystroke --
        ;; silently, because a view with nothing to do about a key is not an
        ;; error.  Nothing else in the program refers to VIEW-KEY-HANDLER, so
        ;; nothing else notices it was never set.
        (setf (view-key-handler view)
              (lambda (string)
                ;; Typing returns to the live screen, as every terminal does:
                ;; a keystroke that vanished into a scrolled-back view would
                ;; look like the terminal had stopped responding.
                (crt.terminal:terminal-scroll-to-bottom (session-terminal session))
                (crt.terminal:terminal-send-string (session-terminal session)
                                                   string)))
        (setf (view-mouse-handlers view)
              (list :down (lambda (event) (handle-mouse-down session event))
                    :up (lambda (event) (handle-mouse-up session event))
                    :dragged (lambda (event) (handle-mouse-dragged session event))
                    :double (lambda (event) (handle-double-click session event))
                    :right-down (lambda (event) (show-context-menu session event))
                    :wheel (lambda (event) (handle-scroll-wheel session event))))
        (apply-session-opacity session)
        ;; showTerminalSize, upstream's own default of true.  NIL rather than a
        ;; flag on the overlay: a session that will never show one should not
        ;; carry the buffer for it.
        (when (crt.settings:settings-show-terminal-size crt.settings:*settings*)
          (setf (session-overlay session) (crt.text:make-overlay)))
        (push session *sessions*)
        (show-crt-window window :tab-of (and tab-of (session-window tab-of)))
        session)))))

(defun end-session (session)
  (setf *sessions* (remove session *sessions*))
  (when (session-terminal session)
    (crt.terminal:terminal-close (session-terminal session))
    (setf (session-terminal session) nil))
  (when (session-graph session)
    (crt.effects:release-graph (session-graph session))
    (setf (session-graph session) nil))
  (when (session-overlay session)
    (crt.text:release-overlay (session-overlay session))
    (setf (session-overlay session) nil))
  (when (session-renderer session)
    (crt.text:release-text-renderer (session-renderer session))
    (setf (session-renderer session) nil))
  (when (session-font session)
    (crt.text:release-font (session-font session))
    (setf (session-font session) nil))
  (let ((window (session-window session)))
    (when window
      (close-crt-window window)
      (objc:invoke (crt-window-handle window) "close")))
  ;; It does NOT call -terminate:, and this is the second time that lesson has
  ;; been learned in this program -- CLOSE-CRT-WINDOW had the same line and the
  ;; same comment justifying it.
  ;;
  ;; Closing the last window is AppKit's cue, answered by
  ;; -applicationShouldTerminateAfterLastWindowClosed:, and AppKit only asks
  ;; while it is running its own event loop.  Calling -terminate: here instead
  ;; exits the process the instant the last session ends: indistinguishable from
  ;; correct in the application, and catastrophic under test, where the suite's
  ;; own teardown ended the run mid-suite with status 0.  A green exit code and
  ;; no summary, twice.
  (values))

;;; Zoom -------------------------------------------------------------------------

(defun apply-font-scaling (session)
  "Rebuild the text renderer at the session's current zoom.

Two different mechanisms, because the two kinds of face want different things.
An OUTLINE face is rasterised at whatever size is asked for, so zoom changes the
pixel size and is smooth.  A BITMAP face has one true size and is magnified by a
whole number, so zoom changes that number -- which is coarse, and is the honest
consequence of magnifying bitmaps by integers rather than interpolating them."
  (let* ((profile (session-profile session))
         (system (profile-system-font-p profile))
         (face (unless system (profile-font profile)))
         ;; A system family is an outline and takes the smooth path, whatever it
         ;; happens to be called: there is no table entry saying otherwise, and
         ;; asking BUNDLED-FONT-LOW-RESOLUTION-P about a family we do not ship
         ;; would be asking a table a question it has no row for.
         (low-res (and face (crt.text:bundled-font-low-resolution-p face)))
         (zoom (session-font-scaling session))
         (base (if face (font-scale-for face) 1)))
    (multiple-value-bind (pixel-size scale)
        (if low-res
            ;; UTIL:QROUND, not CL:ROUND.  With a base of 2 and a zoom of 1.25
            ;; the product is exactly 2.5, and CL rounds half to EVEN -- so
            ;; zooming in was a no-op and the next step jumped by a whole
            ;; multiple.  Third time this rounding rule has bitten in this
            ;; program; it is why QROUND exists.
            (values (crt.text:bundled-font-native-size face)
                    (max 1 (util:qround (* base zoom))))
            (values (max 6 (util:qround (* 24 zoom))) 1))
      (let ((font (if system
                      (crt.text:load-family-font
                       (crt.settings:profile-font-name profile) pixel-size)
                      (crt.text:load-bundled-font face :pixel-size pixel-size))))
        (unless font (return-from apply-font-scaling nil))
        (when (session-font session) (crt.text:release-font (session-font session)))
        (crt.text:release-text-renderer (session-renderer session))
        (destructuring-bind (dw dh) (view-drawable-size (session-view session))
          (multiple-value-bind (line-spacing font-width)
              (profile-metrics (session-profile session))
            (setf (session-font session) font
                  (session-scale session) scale
                  (session-renderer session)
                  (crt.text:make-text-renderer
                   :font font :width dw :height dh
                   :scale scale
                   :margin (session-margin session)
                   :line-spacing line-spacing
                   :font-width font-width
                   :fallbacks (crt.text:font-fallback-chain
                               face :pixel-size (crt.text:font-pixel-size font)))))
          (fit-terminal-to-view session))
        t))))

(defun set-font-scaling (session zoom)
  (let ((clamped (max +min-font-scaling+ (min +max-font-scaling+ zoom))))
    (unless (= clamped (session-font-scaling session))
      (setf (session-font-scaling session) clamped)
      (apply-font-scaling session))
    clamped))

(defun zoom-step (session direction)
  "Change the zoom until something actually changes.

Stepping the MULTIPLIER is not enough on its own: a bitmap face's magnification
is a whole number, so a quarter-step either does nothing or jumps by a whole
multiple depending on where the rounding lands.  This walks in small increments
until the effective size moves, which makes one keystroke mean one visible
change on both kinds of face."
  (let* ((before (crt.text:text-renderer-scale (session-renderer session)))
         (before-size (crt.text:font-pixel-size (session-font session)))
         (step (* direction 0.1d0)))
    (loop repeat 24
          for zoom = (+ (session-font-scaling session) step)
          while (<= +min-font-scaling+ zoom +max-font-scaling+)
          do (set-font-scaling session zoom)
             (when (or (/= before (crt.text:text-renderer-scale
                                   (session-renderer session)))
                       (/= before-size (crt.text:font-pixel-size
                                        (session-font session))))
               (return t))
          finally (return nil))))

(defun zoom-in (session) (zoom-step session 1))
(defun zoom-out (session) (zoom-step session -1))

(defun zoom-reset (session)
  (set-font-scaling session 1.0d0))

;;; Scrolling ----------------------------------------------------------------------

(defun scroll-viewport (session lines)
  (crt.terminal:terminal-scroll (session-terminal session) lines))

;;; Mouse reporting ------------------------------------------------------------------

(defun vt-modifiers (event)
  "NSEvent's flags as libvterm's modifier bits: 1 shift, 2 alt, 4 control."
  (let ((flags (event-modifiers event)) (mods 0))
    (when (logtest flags +modifier-shift+) (setf mods (logior mods 1)))
    (when (logtest flags +modifier-option+) (setf mods (logior mods 2)))
    (when (logtest flags +modifier-control+) (setf mods (logior mods 4)))
    mods))

(defun report-mouse (session event row col kind)
  (let ((button (1+ (objc:invoke-into 'integer event "buttonNumber"))))
    (crt.terminal:terminal-report-mouse
     (session-terminal session)
     :row row :col col :button button :pressed (eq kind :press)
     :modifiers (vt-modifiers event))))

(defun report-wheel (session up lines)
  "Wheel as buttons 4 and 5, which is what the protocol calls them."
  (let ((terminal (session-terminal session)))
    (dotimes (i (min lines 5))
      (crt.terminal:terminal-report-mouse terminal :button (if up 4 5)
                                                   :pressed t))))

(defun apply-session-opacity (session)
  "Make the window as solid as SESSION's profile asks for.

The shader has been emitting a premultiplied alpha below 1 for translucent
profiles since the dynamic pass was written, and an opaque layer threw it away
every frame -- so windowOpacity was a setting that did nothing, and looked from
the inside like a setting that worked."
  (let ((profile (session-profile session)))
    (when profile
      (set-view-opaque (session-view session)
                       (not (crt.settings:window-transparent-p profile))))))

(defun set-session-profile (session name)
  "Switch profiles.  The pipelines for the new specialisation compile once.

FIND-ANY-PROFILE, so the user's own count: a saved profile that could not be
selected again would be unreachable from the window that saved it."
  (let ((profile (or (crt.settings:find-any-profile name)
                     (error "No profile named ~S." name))))
    (setf (session-profile session) profile)
    (when (session-graph session)
      (crt.effects:set-graph-profile (session-graph session) profile))
    ;; The face and the margin are the profile's too, so switching look means
    ;; rebuilding the renderer -- not only re-specialising the shaders.
    (setf (session-margin session) (float (crt.settings:margin profile) 1.0))
    (apply-session-opacity session)
    (apply-font-scaling session)
    profile))

(defun run-terminal (&key width height (columns *default-columns*)
                          (rows *default-rows*) command directory fullscreen
                          (profile *default-profile*) (effects t))
  "Open a terminal window and run the application.  Blocks."
  (ensure-appkit)
  (let ((app (objc.runloop:shared-application
              :activation-policy +ns-application-activation-policy-regular+)))
    (setf *delegate* (make-instance 'application-delegate))
    (objc:invoke app "setDelegate:" (objc:objc-object-pointer *delegate*))
    (make-menu-bar)
    (let ((session (make-session :width width :height height
                                 :columns columns :rows rows
                                 :command command :directory directory
                                 :profile profile :effects effects)))
      (when fullscreen
        (objc:invoke (crt-window-handle (session-window session))
                     "toggleFullScreen:" (cffi:null-pointer))))
    (objc.runloop:run-cocoa-application)))
