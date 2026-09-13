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
  (sampler nil)
  (margin 8.0 :type single-float)
  (title nil))

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
             (target (crt.text:render-text renderer snapshot)))
        (blit-to-drawable session target texture drawable)))))

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
  "Which bundled face to open with.  The font manager and the profiles replace
this in M4; until then it is the one cool-retro-term's IBM VGA profile uses.")

(defun make-session (&key (width 1024) (height 640) command
                          (font *default-font*) (title "cathode-ray-tube"))
  "A window running a shell.  Main thread only."
  (ensure-appkit)
  (let* ((loaded (crt.text::load-bundled-font font))
         (window (make-crt-window :width width :height height :title title
                                  :draw-function #'draw-session))
         (view (crt-window-view window)))
    (unless loaded
      (error "Could not load the bundled font ~S." font))
    (destructuring-bind (dw dh) (view-drawable-size view)
      (let* ((renderer (crt.text:make-text-renderer :font loaded :width dw :height dh))
             (session (%make-session :window window :view view
                                     :renderer renderer :font loaded)))
        (multiple-value-bind (cols rows)
            (crt.text:text-grid-size loaded dw dh)
          (setf (session-terminal session)
                (crt.terminal:make-terminal
                 :rows rows :cols cols :command command
                 :on-exit (lambda (terminal status)
                            (declare (ignore terminal status))
                            ;; The reader thread is not the main thread, and
                            ;; closing a window from anywhere else is a crash
                            ;; waiting for a quiet afternoon.
                            (on-main-thread (lambda () (end-session session))))
                 :on-title (lambda (terminal title)
                             (declare (ignore terminal))
                             (on-main-thread
                              (lambda ()
                                (objc:invoke (crt-window-handle window)
                                             "setTitle:" title)))))))
        (push session *sessions*)
        (show-crt-window window)
        session))))

(defun end-session (session)
  (setf *sessions* (remove session *sessions*))
  (when (session-terminal session)
    (crt.terminal:terminal-close (session-terminal session))
    (setf (session-terminal session) nil))
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
  ;; With no windows left, quit -- which is what
  ;; -applicationShouldTerminateAfterLastWindowClosed: would do under [NSApp run]
  ;; and does not do when the last window is closed programmatically.
  (when (null *sessions*)
    (objc:invoke (objc.runloop:shared-application) "terminate:" nil)))

(defun run-terminal (&key (width 1024) (height 640) command)
  "Open a terminal window and run the application.  Blocks."
  (ensure-appkit)
  (let ((app (objc.runloop:shared-application
              :activation-policy +ns-application-activation-policy-regular+)))
    (setf *delegate* (make-instance 'application-delegate))
    (objc:invoke app "setDelegate:" (objc:objc-object-pointer *delegate*))
    (make-menu-bar)
    (make-session :width width :height height :command command)
    (objc.runloop:run-cocoa-application)))
