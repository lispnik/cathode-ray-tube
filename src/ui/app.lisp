;;;; src/ui/app.lisp -- the application object, its delegate, and the run loop.

(in-package #:cathode-ray-tube.ui)

(objc:define-objc-class application-delegate ()
  ((launched :initform nil :accessor delegate-launched-p))
  (:objc-class-name "CathodeRayTubeAppDelegate"))

(objc:define-objc-method ("applicationDidFinishLaunching:" :void)
    ((self application-delegate) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (handling-errors ("applicationDidFinishLaunching:")
    (setf (delegate-launched-p self) t)))

;;; Closing the last window quits, which is what a terminal does.  (A browser
;;; would answer NO here and stay in the Dock.)
(objc:define-objc-method ("applicationShouldTerminateAfterLastWindowClosed:"
                          objc:objc-bool)
    ((self application-delegate) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  t)

(defvar *delegate* nil)

(defun menu-item (menu title selector key &key (modifiers nil) (target t)
                                                    (state nil) tag)
  "One item, targeted at the action object.

KEY is the key equivalent as a lowercase string; MODIFIERS defaults to Command
alone, which is what a bare key equivalent means.  The TARGET is set explicitly
because the responder chain cannot find behaviour that lives in a Lisp
structure -- see actions.lisp."
  (let ((item (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                           "initWithTitle:action:keyEquivalent:"
                           title
                           (if selector
                               (objc:coerce-to-selector selector)
                               (cffi:null-pointer))
                           (or key ""))))
    (when modifiers (objc:invoke item "setKeyEquivalentModifierMask:" modifiers))
    (when (and target selector)
      (objc:invoke item "setTarget:" (objc:objc-object-pointer (menu-target))))
    (when state (objc:invoke item "setState:" state))
    (when tag (objc:invoke item "setTag:" tag))
    (objc:invoke menu "addItem:" item)
    item))

(defun menu-separator (menu)
  (objc:invoke menu "addItem:" (objc:invoke "NSMenuItem" "separatorItem")))

(defun submenu (bar title)
  "A top-level menu.  Returns the NSMenu to fill in.

The TITLE has to be set on the NSMenu and not only on the item: AppKit reads the
menu's title for the application menu and for the Window and Help menus it
adopts by name, and an untitled menu quietly loses those behaviours."
  (let ((item (objc:invoke (objc:invoke "NSMenuItem" "alloc") "init"))
        (menu (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" title)))
    (objc:invoke item "setSubmenu:" menu)
    (objc:invoke bar "addItem:" item)
    menu))

;;; NSEventModifierFlags, for key equivalents.
(defconstant +key-command+ +modifier-command+)
(defconstant +key-command-shift+ (logior +modifier-command+ +modifier-shift+))

(defun make-menu-bar (&optional (name "cathode-ray-tube"))
  "The menu bar.

Not decoration: without one, AppKit does no key-equivalent handling at all, so
Cmd-Q, Cmd-C and Cmd-V are dead keys rather than commands.  The structure
follows cool-retro-term's WindowMenu.qml -- File, Edit, View, Profiles, Help --
with the macOS conventions it cannot express, chiefly an application menu named
after the application with About and Quit in it."
  (let ((bar (objc:invoke (objc:invoke "NSMenu" "alloc") "init")))
    ;; The application menu.  Its title is ignored -- AppKit always shows the
    ;; process name in bold -- but the FIRST menu is always this one.
    (let ((app-menu (submenu bar name)))
      (menu-item app-menu (format nil "About ~A" name) "crtAbout:" nil)
      (menu-separator app-menu)
      ;; Comma, which is the macOS convention and not upstream's -- its settings
      ;; window opens from a View menu item, because Qt has no application menu
      ;; to put it in.  Anyone on this platform will try Cmd-, first.
      (menu-item app-menu "Settings..." "crtShowSettings:" ",")
      (menu-separator app-menu)
      (menu-item app-menu (format nil "Hide ~A" name) "hide:" "h" :target nil)
      (menu-item app-menu "Hide Others" "hideOtherApplications:" "h"
                 :modifiers (logior +modifier-command+ +modifier-option+)
                 :target nil)
      (menu-item app-menu "Show All" "unhideAllApplications:" nil :target nil)
      (menu-separator app-menu)
      (menu-item app-menu (format nil "Quit ~A" name) "terminate:" "q"
                 :target nil))

    (let ((file (submenu bar "File")))
      (menu-item file "New Window" "crtNewWindow:" "n")
      (menu-item file "New Tab" "crtNewTab:" "t")
      (menu-separator file)
      (menu-item file "Close" "crtCloseWindow:" "w"))

    (let ((edit (submenu bar "Edit")))
      (menu-item edit "Copy" "crtCopy:" "c")
      (menu-item edit "Paste" "crtPaste:" "v")
      (menu-separator edit)
      (menu-item edit "Select All" "crtSelectAll:" "a"))

    (let ((view (submenu bar "View")))
      ;; -toggleFullScreen: is NSWindow's own and needs no target: the responder
      ;; chain finds the key window, which is exactly the right one.
      (menu-item view "Enter Full Screen" "toggleFullScreen:" "f"
                 :modifiers (logior +modifier-command+ +modifier-control+)
                 :target nil)
      (menu-separator view)
      (menu-item view "Zoom In" "crtZoomIn:" "+")
      (menu-item view "Zoom Out" "crtZoomOut:" "-")
      (menu-item view "Actual Size" "crtZoomReset:" "0")
      (menu-separator view)
      (menu-item view "Effects" "crtToggleEffects:" nil :state 1)
      (menu-separator view)
      ;; NSWindow implements these four itself, so they need no target: the
      ;; responder chain finds the key window, which is the right one.  They are
      ;; what makes the system tab bar usable, and reimplementing them would be
      ;; reimplementing the part macOS already does well.
      (menu-item view "Show All Tabs" "toggleTabOverview:" nil :target nil)
      (menu-item view "Show Tab Bar" "toggleTabBar:" nil :target nil)
      (menu-item view "Show Next Tab" "selectNextTab:" "]"
                 :modifiers (logior +modifier-command+ +modifier-shift+)
                 :target nil)
      (menu-item view "Show Previous Tab" "selectPreviousTab:" "["
                 :modifiers (logior +modifier-command+ +modifier-shift+)
                 :target nil)
      (menu-separator view)
      ;; Cmd-1 through Cmd-9, as upstream binds Meta+1 through Meta+9.  Nine
      ;; items rather than a loop because each needs its own selector: a menu
      ;; item carries an action and no argument.
      (loop for index from 1 to 9
            do (menu-item view (format nil "Tab ~D" index)
                          (format nil "crtSelectTab~D:" index)
                          (princ-to-string index))))

    ;; The Profiles menu is the only way to change look at run time, and its
    ;; absence was the most visible thing missing from this program.
    (let ((profiles (submenu bar "Profiles")))
      (dolist (profile crt.settings:+profiles+)
        (menu-item profiles (crt.settings:profile-name profile)
                   "crtSetProfile:" nil)))

    (let ((window (submenu bar "Window")))
      (menu-item window "Minimize" "performMiniaturize:" "m" :target nil)
      (menu-item window "Zoom" "performZoom:" nil :target nil)
      ;; Named to AppKit, which then adds the window list and keeps it current.
      (objc:invoke (objc.runloop:shared-application) "setWindowsMenu:" window))

    (objc:invoke (objc.runloop:shared-application) "setMainMenu:" bar)
    bar))

(defun gradient-frame (view texture drawable time)
  "The M1 draw function: a gradient, at vsync.

Replaced in M3 by the effect chain.  It is not a placeholder for its own sake --
it exercises the synthesised quad, a uniform block delivered by
setFragmentBytes:, a specialised pipeline, and presenting a drawable, which is
every mechanism the real graph is built from."
  (declare (ignore view))
  (let ((pipeline (crt.metal:pipeline :fragment "gradient_fragment"
                                      :constants (list 3 t)
                                      :pixel-format crt.metal:+pixel-format-bgra8unorm+
                                      :label "gradient")))
    (cffi:with-foreign-object (uniforms :float 2)
      (setf (cffi:mem-aref uniforms :float 0) (float time 1.0)
            (cffi:mem-aref uniforms :float 1) 1.0)
      (crt.metal:with-render-pass (encoder texture
                                   :clear '(0d0 0d0 0d0 1d0)
                                   :present drawable
                                   :label "gradient")
        (crt.metal:use-pipeline encoder pipeline)
        (crt.metal:bind-fragment-bytes encoder uniforms 8 0)
        (crt.metal:draw-quad encoder)))))

(defun run (&key (width 1024) (height 768) (draw-function #'gradient-frame))
  "Open a window and run the application.  Blocks; AppKit owns this thread.

Must be the MAIN thread -- AppKit is not merely thread-hostile about this, it
refuses.  MAIN is responsible for getting here on the right one."
  (ensure-appkit)
  (let ((app (objc.runloop:shared-application
              :activation-policy +ns-application-activation-policy-regular+)))
    (setf *delegate* (make-instance 'application-delegate))
    (objc:invoke app "setDelegate:" (objc:objc-object-pointer *delegate*))
    (make-menu-bar)
    (show-crt-window (make-crt-window :width width :height height
                                      :draw-function draw-function))
    (objc.runloop:run-cocoa-application)))
