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
      (multiple-value-bind (cols rows) (crt.text:resize-text-renderer renderer width height)
        (crt.terminal:terminal-resize (session-terminal session) rows cols)
        (when (session-graph session)
          (crt.effects:resize-graph (session-graph session) width height))
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
      (let* ((terminal (session-terminal session))
             (snapshot (crt.terminal:terminal-snapshot terminal))
             (renderer (session-renderer session))
             ;; The terminal's own colours, which the effect chain then recolours
             ;; through convertWithChroma.  White on black is deliberate: the
             ;; profile's foreground and background are applied in the DYNAMIC
             ;; pass, and painting them here as well would apply them twice.
             (target (crt.text:render-text renderer snapshot
                                           :default-fg '(255 255 255)
                                           :default-bg '(0 0 0))))
        (if (and (session-effects session) (session-graph session))
            (crt.effects:render-effects
             (session-graph session) target texture
             :drawable drawable
             :time (crt.ui:view-effect-time view)
             :painted (crt.terminal:snapshot-painted snapshot)
             ;; The TERMINAL's pixel grid, not the drawable's.  This is what
             ;; sets the scanline frequency; conflating it with device pixels
             ;; is the classic way to get scanlines that are the wrong size and
             ;; moire that moves when the window does.
             :virtual-width (* (crt.terminal:terminal-cols terminal)
                               (crt.text::text-renderer-cell-width renderer))
             :virtual-height (* (crt.terminal:terminal-rows terminal)
                                (crt.text::text-renderer-cell-height renderer)))
            (blit-to-drawable session target texture drawable))))))

(defun update-session-title (session)
  (let ((title (crt.terminal:terminal-title (session-terminal session))))
    (when (and title (not (equal title (session-title session))))
      (setf (session-title session) title)
      (objc:invoke (crt-window-handle (session-window session)) "setTitle:" title))))

(defun blit-to-drawable (session source texture drawable)
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
                                 :present drawable
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
  "The leading every one of the fourteen profiles asks for.

A constant here rather than read from the profile because the WINDOW is sized
before a profile is chosen, and a terminal whose rows changed height when you
switched profiles would be worse than one whose leading is slightly wrong for
some of them.  All fourteen say 0.1, so there is nothing to choose between.")

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

(defun make-session (&key width height (columns *default-columns*)
                          (rows *default-rows*) command
                          (font *default-font*) (title "cathode-ray-tube")
                          (profile *default-profile*) (effects t))
  "A window running a shell.  Main thread only.

Sized from COLUMNS by ROWS unless WIDTH and HEIGHT say otherwise -- the opposite
of the way the grid is computed afterwards, and deliberately so: a terminal
should OPEN at a familiar number of characters and only then start following
whatever size the window is dragged to."
  (ensure-appkit)
  (let* ((loaded (crt.text::load-bundled-font font))
         (scale (font-scale-for font)))
    (unless loaded
      (error "Could not load the bundled font ~S." font))
    ;; Device pixels for the grid, then points for the window: AppKit sizes
    ;; windows in points and the glyphs are in device pixels, and conflating the
    ;; two gives a window half the size it should be on a Retina display.
    (unless (and width height)
      (multiple-value-bind (pixel-width pixel-height)
          (crt.text:grid-pixel-size loaded columns rows :scale scale
                                    :line-spacing +default-line-spacing+)
        (let ((backing (screen-backing-scale)))
          (setf width (ceiling pixel-width backing)
                height (ceiling pixel-height backing)))))
    (make-session-in-window loaded scale width height title command profile
                            effects)))

(defun make-session-in-window (loaded scale width height title command profile
                               effects)
  (let* ((window (make-crt-window :width width :height height :title title
                                  :draw-function #'draw-session))
         (view (crt-window-view window)))
    (destructuring-bind (dw dh) (view-drawable-size view)
      (let* ((renderer (crt.text:make-text-renderer :font loaded :width dw :height dh
                                                    :scale scale
                                                    :line-spacing
                                                    +default-line-spacing+))
             (chosen (or (crt.settings:find-profile profile)
                         (error "No profile named ~S." profile)))
             (session (%make-session :window window :view view
                                     :renderer renderer :font loaded
                                     :profile chosen :effects effects)))
        (when effects
          (setf (session-graph session)
                (crt.effects:make-graph :profile chosen :width dw :height dh)))
        (multiple-value-bind (cols rows)
            (crt.text:text-grid-size loaded dw dh :scale scale
                                     :line-spacing +default-line-spacing+)
          (setf (session-terminal session)
                (crt.terminal:make-terminal
                 :rows rows :cols cols :command command
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
                (crt.terminal:terminal-send-string (session-terminal session)
                                                   string)))
        (push session *sessions*)
        (show-crt-window window)
        session))))

(defun end-session (session)
  (setf *sessions* (remove session *sessions*))
  (when (session-terminal session)
    (crt.terminal:terminal-close (session-terminal session))
    (setf (session-terminal session) nil))
  (when (session-graph session)
    (crt.effects:release-graph (session-graph session))
    (setf (session-graph session) nil))
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

(defun set-session-profile (session name)
  "Switch profiles.  The pipelines for the new specialisation compile once."
  (let ((profile (or (crt.settings:find-profile name)
                     (error "No profile named ~S." name))))
    (setf (session-profile session) profile)
    (when (session-graph session)
      (crt.effects::set-graph-profile (session-graph session) profile))
    profile))

(defun run-terminal (&key width height (columns *default-columns*)
                          (rows *default-rows*) command
                          (profile *default-profile*) (effects t))
  "Open a terminal window and run the application.  Blocks."
  (ensure-appkit)
  (let ((app (objc.runloop:shared-application
              :activation-policy +ns-application-activation-policy-regular+)))
    (setf *delegate* (make-instance 'application-delegate))
    (objc:invoke app "setDelegate:" (objc:objc-object-pointer *delegate*))
    (make-menu-bar)
    (make-session :width width :height height :columns columns :rows rows
                  :command command :profile profile :effects effects)
    (objc.runloop:run-cocoa-application)))
