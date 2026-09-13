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
