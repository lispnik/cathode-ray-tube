;;;; src/settings/store.lisp -- settings that survive a restart.
;;;;
;;;; cool-retro-term keeps three JSON blobs in a SQLite database through
;;;; QtQuick.LocalStorage, and flushes them only when the last window closes --
;;;; so a crash loses everything you changed.  This writes s-expressions to a
;;;; file under Application Support, and writes them when something changes.
;;;;
;;;; S-EXPRESSIONS RATHER THAN JSON, for the settings themselves.  Nothing but
;;;; this program reads them, READ is already here, and a settings file a user
;;;; can open and edit is a feature.  PROFILES are still JSON, because those do
;;;; travel -- a profile exported here has to be importable by cool-retro-term.
;;;;
;;;; READING IS UNTRUSTING.  A settings file is a file on disk, which means it
;;;; can be truncated, half-written, or edited by hand into nonsense.  Any
;;;; failure falls back to the defaults and says so, because a terminal that
;;;; refuses to start because of its own preferences file is worse than one that
;;;; forgets your preferences.

(in-package #:cathode-ray-tube.settings)

(defparameter +settings-version+ 1)

(defun settings-directory ()
  (merge-pathnames "Library/Application Support/cathode-ray-tube/"
                   (user-homedir-pathname)))

(defun settings-file ()
  (merge-pathnames "settings.lisp" (settings-directory)))

;;; The global settings, which are not part of a profile ---------------------------

(defstruct settings
  "Everything cool-retro-term keeps outside a profile.

The quality knobs are upstream's names and defaults.  They were all wired
through to the render graph and frozen at their initial values, because nothing
could set them."
  (profile-name "Default Amber" :type string)
  (effects t)
  ;; TimeManager.qml: `time' advances every Nth frame, so the effects animate at
  ;; about 20Hz while the quad redraws at 60.  Part of the look, not a budget.
  (effects-frame-skip 3 :type (integer 1 10))
  (window-scaling 1.0d0 :type double-float)
  (bloom-quality 0.5d0 :type double-float)
  (burn-in-quality 0.5d0 :type double-float)
  (font-scaling 1.0d0 :type double-float)
  (columns 80 :type fixnum)
  (rows 25 :type fixnum)
  (show-terminal-size t)
  (use-custom-command nil)
  (custom-command "" :type string)
  ;; Custom profiles, as (NAME . JSON-STRING).  JSON because a custom profile is
  ;; exactly the thing that travels between the two programs.
  (custom-profiles '() :type list))

(defvar *settings* (make-settings)
  "The live settings.  One set, for the whole application, as upstream has.")

;;; Writing --------------------------------------------------------------------------

(defun settings-to-plist (settings)
  (list :version +settings-version+
        :profile-name (settings-profile-name settings)
        :effects (settings-effects settings)
        :effects-frame-skip (settings-effects-frame-skip settings)
        :window-scaling (settings-window-scaling settings)
        :bloom-quality (settings-bloom-quality settings)
        :burn-in-quality (settings-burn-in-quality settings)
        :font-scaling (settings-font-scaling settings)
        :columns (settings-columns settings)
        :rows (settings-rows settings)
        :show-terminal-size (settings-show-terminal-size settings)
        :use-custom-command (settings-use-custom-command settings)
        :custom-command (settings-custom-command settings)
        :custom-profiles (settings-custom-profiles settings)))

(defun save-settings (&optional (settings *settings*))
  "Write the settings.  Returns the path, or NIL if it could not be written.

Never signals: failing to save preferences must not take down a terminal in the
middle of someone's work.  Written to a temporary file and renamed, so an
interrupted write leaves the previous settings rather than half of these."
  (handler-case
      (let* ((path (settings-file))
             (temporary (make-pathname :type "tmp" :defaults path)))
        (ensure-directories-exist path)
        (with-open-file (out temporary :direction :output :if-exists :supersede
                                       :external-format :utf-8)
          (let ((*print-readably* nil) (*print-pretty* t) (*package* (find-package :keyword)))
            (format out ";;;; cathode-ray-tube settings.  Written by the program;~%")
            (format out ";;;; safe to edit, and safe to delete -- anything wrong~%")
            (format out ";;;; here is ignored and the defaults are used.~%~%")
            (prin1 (settings-to-plist settings) out)
            (terpri out)))
        (rename-file temporary path)
        path)
    (error (condition)
      (format *error-output* "~&cathode-ray-tube: could not save settings: ~A~%"
              condition)
      nil)))

;;; Reading --------------------------------------------------------------------------

(defun read-settings-form (path)
  "The plist in PATH, or NIL.

READ with *READ-EVAL* off: this is a file on disk and #. would otherwise let it
run anything the moment the program starts."
  (handler-case
      (with-open-file (in path :if-does-not-exist nil :external-format :utf-8)
        (when in
          (let ((*read-eval* nil)
                (*package* (find-package :keyword)))
            (read in nil nil))))
    (error (condition)
      (format *error-output* "~&cathode-ray-tube: ignoring unreadable settings: ~A~%"
              condition)
      nil)))

(defmacro getf-typed (plist key default type)
  "PLIST's KEY when it is of TYPE, else DEFAULT.

Every field is checked, because a hand-edited file can put a string where a
number goes and the failure would otherwise surface somewhere far away -- as a
division by a string in the middle of a frame."
  (let ((value (gensym)))
    `(let ((,value (getf ,plist ,key :missing)))
       (if (typep ,value ',type) ,value ,default))))

(defun load-settings (&key (path (settings-file)))
  "Read the settings, falling back to the defaults for anything missing or wrong."
  (let ((plist (read-settings-form path))
        (settings (make-settings)))
    (when (listp plist)
      (setf (settings-profile-name settings)
            (getf-typed plist :profile-name "Default Amber" string)
            (settings-effects settings) (not (eq (getf plist :effects t) nil))
            (settings-effects-frame-skip settings)
            (getf-typed plist :effects-frame-skip 3 (integer 1 10))
            (settings-window-scaling settings)
            (getf-typed plist :window-scaling 1.0d0 (double-float 0.25d0 1.0d0))
            (settings-bloom-quality settings)
            (getf-typed plist :bloom-quality 0.5d0 (double-float 0.25d0 1.0d0))
            (settings-burn-in-quality settings)
            (getf-typed plist :burn-in-quality 0.5d0 (double-float 0.25d0 1.0d0))
            (settings-font-scaling settings)
            (getf-typed plist :font-scaling 1.0d0 (double-float 0.25d0 2.5d0))
            (settings-columns settings) (getf-typed plist :columns 80 (integer 8 500))
            (settings-rows settings) (getf-typed plist :rows 25 (integer 4 200))
            (settings-show-terminal-size settings)
            (not (eq (getf plist :show-terminal-size t) nil))
            (settings-use-custom-command settings)
            (and (getf plist :use-custom-command nil) t)
            (settings-custom-command settings)
            (getf-typed plist :custom-command "" string)
            (settings-custom-profiles settings)
            (remove-if-not (lambda (entry)
                             (and (consp entry) (stringp (car entry))
                                  (stringp (cdr entry))))
                           (getf-typed plist :custom-profiles '() list))))
    settings))

(defun load-settings-into-place ()
  (setf *settings* (load-settings)))

;;; Custom profiles ------------------------------------------------------------------

(defun all-profiles (&optional (settings *settings*))
  "The fourteen built-ins followed by the user's own."
  (append +profiles+
          (loop for (name . json) in (settings-custom-profiles settings)
                for profile = (handler-case (profile-from-json json)
                                (error () nil))
                when profile
                  collect (progn (setf (profile-name profile) name) profile))))

(defun find-any-profile (name &optional (settings *settings*))
  (find name (all-profiles settings) :key #'profile-name :test #'string-equal))

(defun builtin-profile-p (name)
  (and (find-profile name) t))

(defun save-custom-profile (name profile &optional (settings *settings*))
  "Add or replace a custom profile.  Built-in names are refused.

Refused rather than shadowed: a custom profile called \"Default Amber\" that
quietly replaced the built-in one would leave no way back to it."
  (when (builtin-profile-p name)
    (error "~S is a built-in profile and cannot be replaced." name))
  (let ((json (profile-to-json profile)))
    (setf (settings-custom-profiles settings)
          (cons (cons name json)
                (remove name (settings-custom-profiles settings)
                        :key #'car :test #'string-equal))))
  (save-settings settings)
  name)

(defun remove-custom-profile (name &optional (settings *settings*))
  (setf (settings-custom-profiles settings)
        (remove name (settings-custom-profiles settings)
                :key #'car :test #'string-equal))
  (save-settings settings)
  name)
