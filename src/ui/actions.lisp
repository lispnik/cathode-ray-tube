;;;; src/ui/actions.lisp -- what the menus do.
;;;;
;;;; ONE Objective-C object is the target of every menu item, in the menu bar
;;;; and in the context menu both.  Cocoa's usual arrangement is to leave the
;;;; target nil and let the item travel the responder chain, which finds the
;;;; first object implementing the selector -- elegant when views are objects
;;;; with behaviour, and useless here, where the behaviour lives in a Lisp
;;;; structure that AppKit has never heard of.
;;;;
;;;; So every action finds its session the same way: through the key window.
;;;; With one window that is trivially right; with several it is still right,
;;;; because a menu command means the window you are looking at.

(in-package #:cathode-ray-tube.ui)

(objc:define-objc-class menu-actions ()
  ()
  (:objc-class-name "CathodeRayTubeActions"))

(defvar *menu-target* nil)

(defun menu-target ()
  (or *menu-target* (setf *menu-target* (make-instance 'menu-actions))))

(defun key-session ()
  "The session of the key window, or the only one, or NIL.

Falls back to the only session because a context menu can be raised on a window
that is not yet key -- right-clicking an inactive window activates it, but the
menu is built first."
  (let ((window (objc:invoke (objc.runloop:shared-application) "keyWindow")))
    (or (unless (crt.metal:null-object-p window)
          (find-if (lambda (session)
                     (cffi:pointer-eq
                      (objc:objc-object-pointer
                       (crt-window-handle (session-window session)))
                      (objc:objc-object-pointer window)))
                   *sessions*))
        (when (= 1 (length *sessions*)) (first *sessions*)))))

(defmacro define-action (selector (&optional (session (gensym "SESSION"))
                                             (sender (gensym "SENDER")))
                         &body body)
  "An Objective-C action that finds the key session and cannot signal."
  `(objc:define-objc-method (,selector :void)
       ((self menu-actions) (,sender objc:objc-object-pointer))
     (declare (ignorable ,sender))
     (handling-errors (,selector)
       (let ((,session (key-session)))
         (declare (ignorable ,session))
         ,@body))))

(define-action "crtNothing:" () nil)

(define-action "crtShowSettings:" ()
  (show-settings-window))

;;; Edit ---------------------------------------------------------------------------

(define-action "crtCopy:" (session)
  (when session (copy-selection session)))

(define-action "crtPaste:" (session)
  (when session (paste-clipboard session)))

(define-action "crtSelectAll:" (session)
  (when session
    (let ((terminal (session-terminal session)))
      (setf (session-selection session)
            (make-selection 0 0
                            (1- (crt.terminal:terminal-rows terminal))
                            (crt.terminal:terminal-cols terminal))))))

;;; View ----------------------------------------------------------------------------

(define-action "crtZoomIn:" (session) (when session (zoom-in session)))
(define-action "crtZoomOut:" (session) (when session (zoom-out session)))
(define-action "crtZoomReset:" (session) (when session (zoom-reset session)))

(define-action "crtToggleEffects:" (session)
  (when session
    (setf (session-effects session) (not (session-effects session)))))

;;; Profiles --------------------------------------------------------------------------

(define-action "crtSetProfile:" (session sender)
  (when session
    (let ((title (objc:invoke-into 'string sender "title")))
      (set-session-profile session title))))

;;; File -----------------------------------------------------------------------------

(define-action "crtNewWindow:" ()
  (make-session))

(define-action "crtNewTab:" (session)
  "A new terminal in the key window's tab group, or a window if there is none.

The new tab inherits the current one's PROFILE, which is what makes a window of
tabs look like one terminal rather than several.  It does NOT inherit the
shell's working directory: knowing that means the child telling us, through OSC
7 or an equivalent, and a tab that opened in the right directory for bash and
the wrong one for everything else would be worse than one that is honestly
always home.  Upstream does not inherit it either."
  (if session
      (make-session :tab-of session
                    :profile (crt.settings:profile-name (session-profile session)))
      (make-session)))

(defmacro define-tab-action (selector index)
  "Cmd-N brings the Nth tab forward, doing nothing when there is no such tab.

Upstream binds Meta+1 through Meta+9 (TerminalWindow.qml:105-170) and guards each
on the tab count; these are Cmd-1 through Cmd-9, which is what the same keys are
called here."
  `(define-action ,selector (session)
     (when session
       (select-window-tab (session-window session) ,index))))

(define-tab-action "crtSelectTab1:" 0)
(define-tab-action "crtSelectTab2:" 1)
(define-tab-action "crtSelectTab3:" 2)
(define-tab-action "crtSelectTab4:" 3)
(define-tab-action "crtSelectTab5:" 4)
(define-tab-action "crtSelectTab6:" 5)
(define-tab-action "crtSelectTab7:" 6)
(define-tab-action "crtSelectTab8:" 7)
(define-tab-action "crtSelectTab9:" 8)

(define-action "crtCloseWindow:" (session)
  (when session
    (objc:invoke (crt-window-handle (session-window session))
                 "performClose:" (cffi:null-pointer))))

;;; Help ------------------------------------------------------------------------------

(define-action "crtAbout:" ()
  (show-about-panel))

(defun show-about-panel ()
  "The standard About panel, with the credit the licence requires.

-orderFrontStandardAboutPanelWithOptions: rather than a window of our own: it is
the panel every Mac application has, it is free, and the only thing it needs
from us is what to say."
  (let ((options (objc:invoke "NSMutableDictionary" "dictionary")))
    (flet ((put (key value)
             (objc:invoke options "setObject:forKey:" value key)))
      (put "ApplicationName" "cathode-ray-tube")
      (put "ApplicationVersion" (or (asdf:component-version
                                     (asdf:find-system :cathode-ray-tube nil))
                                    "0.1.0"))
      (put "Copyright"
           (concatenate 'string
                        "GPL-3.0-or-later.  A Common Lisp port of cool-retro-term "
                        "by Filippo Scognamiglio, whose shaders and profiles this "
                        "is a translation of.  Terminal emulation by libvterm "
                        "(Paul Evans), vendored under MIT.")))
    (objc:invoke (objc.runloop:shared-application)
                 "orderFrontStandardAboutPanelWithOptions:" options)
    (objc:invoke (objc.runloop:shared-application) "activateIgnoringOtherApps:" t)))
