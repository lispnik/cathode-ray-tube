;;;; src/ui/keyboard.lisp -- NSEvent to the bytes a terminal expects.
;;;;
;;;; -[NSEvent characters] has already done most of this: it applies the keyboard
;;;; layout, composes dead keys, and turns Control-C into #\Etx.  What it cannot
;;;; do is the keys that are not characters -- arrows, Home, the function keys --
;;;; which AppKit reports as private-use codepoints in the 0xF700 block.  Those
;;;; are the table below.

(in-package #:cathode-ray-tube.ui)

;;; NSEventModifierFlags, the ones that reach a terminal.
(defconstant +modifier-shift+   #x00020000)
(defconstant +modifier-control+ #x00040000)
(defconstant +modifier-option+  #x00080000)
(defconstant +modifier-command+ #x00100000)

;;; The NSxxxFunctionKey constants, which AppKit places in Unicode's private use
;;; area rather than inventing an event type for.
(defparameter +function-keys+
  '((#xF700 . :up) (#xF701 . :down) (#xF702 . :left) (#xF703 . :right)
    (#xF704 . :f1) (#xF705 . :f2) (#xF706 . :f3) (#xF707 . :f4)
    (#xF708 . :f5) (#xF709 . :f6) (#xF70A . :f7) (#xF70B . :f8)
    (#xF70C . :f9) (#xF70D . :f10) (#xF70E . :f11) (#xF70F . :f12)
    (#xF728 . :delete) (#xF729 . :home) (#xF72B . :end)
    (#xF72C . :page-up) (#xF72D . :page-down) (#xF727 . :insert)))

(defparameter +key-sequences+
  '((:up . "~C[A") (:down . "~C[B") (:right . "~C[C") (:left . "~C[D")
    (:home . "~C[H") (:end . "~C[F")
    (:page-up . "~C[5~~") (:page-down . "~C[6~~")
    (:insert . "~C[2~~") (:delete . "~C[3~~")
    (:f1 . "~COP") (:f2 . "~COQ") (:f3 . "~COR") (:f4 . "~COS")
    (:f5 . "~C[15~~") (:f6 . "~C[17~~") (:f7 . "~C[18~~") (:f8 . "~C[19~~")
    (:f9 . "~C[20~~") (:f10 . "~C[21~~") (:f11 . "~C[23~~") (:f12 . "~C[24~~"))
  "Format strings, each taking one ESC.  The plain xterm forms: application
cursor keys and modified sequences are the terminal's business to request and
belong with the rest of the mode handling, not here.")

(defun function-key-sequence (key)
  (let ((format-string (cdr (assoc key +key-sequences+))))
    (when format-string (format nil format-string #\Escape))))

(defun event-key-string (event)
  "The bytes EVENT should send to the child, or NIL.

Option is treated as Meta -- ESC then the character -- which is what a terminal
user expects and what every terminal emulator on macOS offers as a setting.  It
costs the Option key its role in typing accented characters, which is the trade
every one of them makes by default."
  (let* ((modifiers (objc:invoke-into 'integer event "modifierFlags"))
         (characters (objc:invoke-into 'string event "characters")))
    (when (plusp (length characters))
      (let* ((code (char-code (char characters 0)))
             (function-key (cdr (assoc code +function-keys+))))
        (cond
          (function-key (function-key-sequence function-key))
          ;; Command is the application's, never the child's: Cmd-Q, Cmd-C and
          ;; Cmd-V must not reach the shell as bytes.
          ((logtest modifiers +modifier-command+) nil)
          ((logtest modifiers +modifier-option+)
           (concatenate 'string (string #\Escape) characters))
          (t characters))))))
