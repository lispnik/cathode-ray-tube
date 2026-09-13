;;;; src/ui/mouse.lisp -- selection, the clipboard, the wheel, and reporting.
;;;;
;;;; Two audiences for a mouse event, and which one gets it is the child's
;;;; decision, not ours.  A program that has asked for mouse reporting -- vim,
;;;; tmux, less -- wants the events as escape sequences; anything else means the
;;;; user is selecting text.  Shift forces selection regardless, which is the
;;;; convention every terminal follows and the only way to select inside vim.

(in-package #:cathode-ray-tube.ui)

;;; The clipboard ----------------------------------------------------------------

(defun set-clipboard-string (string)
  "Put STRING on the general pasteboard."
  (when (plusp (length string))
    (let ((pasteboard (objc:invoke "NSPasteboard" "generalPasteboard")))
      ;; clearContents FIRST, and its return value is the change count -- a
      ;; pasteboard that is written without being cleared keeps whatever type
      ;; was there before alongside the new one.
      (objc:invoke pasteboard "clearContents")
      (objc:invoke pasteboard "setString:forType:" string "public.utf8-plain-text"))
    string))

(defun clipboard-string ()
  "The general pasteboard's text, or NIL."
  (let* ((pasteboard (objc:invoke "NSPasteboard" "generalPasteboard"))
         (string (objc:invoke pasteboard "stringForType:" "public.utf8-plain-text")))
    (unless (crt.metal:null-object-p string)
      (objc:invoke-into 'string string "description"))))

;;; Selection --------------------------------------------------------------------

(defstruct (selection (:constructor make-selection (anchor-row anchor-col
                                                    end-row end-col))
                      ;; No copier: DEFSTRUCT would name it COPY-SELECTION and
                      ;; clobber the function of that name, which is the one
                      ;; that puts the selection on the pasteboard.
                      (:copier nil))
  "A half-open range over the grid, in the order it was dragged.

Kept as ANCHOR and END rather than start and end because a selection dragged
upward has its anchor below its end, and normalising on every mouse-move would
lose which end the user is holding."
  (anchor-row 0 :type fixnum) (anchor-col 0 :type fixnum)
  (end-row 0 :type fixnum) (end-col 0 :type fixnum))

(defun selection-ordered (selection)
  "(values START-ROW START-COL END-ROW END-COL) with start before end."
  (let ((ar (selection-anchor-row selection)) (ac (selection-anchor-col selection))
        (er (selection-end-row selection)) (ec (selection-end-col selection)))
    (if (or (< er ar) (and (= er ar) (< ec ac)))
        (values er ec ar ac)
        (values ar ac er ec))))

(defun selection-empty-p (selection)
  (and selection
       (= (selection-anchor-row selection) (selection-end-row selection))
       (= (selection-anchor-col selection) (selection-end-col selection))))

(defun cell-selected-p (selection row col)
  "True when ROW, COL falls inside SELECTION, in reading order.

Reading order, not a rectangle: a selection spanning three lines takes all of
the first line after the anchor, all of the middle one, and the start of the
last -- which is what selecting prose means and what every terminal does."
  (when (and selection (not (selection-empty-p selection)))
    (multiple-value-bind (sr sc er ec) (selection-ordered selection)
      (cond ((or (< row sr) (> row er)) nil)
            ((= sr er) (and (>= col sc) (< col ec)))
            ((= row sr) (>= col sc))
            ((= row er) (< col ec))
            (t t)))))

(defun selection-text (session)
  "The selected text, as the child would have written it."
  (let ((selection (session-selection session))
        (terminal (session-terminal session)))
    (when (and selection (not (selection-empty-p selection)))
      (multiple-value-bind (sr sc er ec) (selection-ordered selection)
        (crt.terminal:with-terminal-locked (terminal)
          (let ((vt (crt.terminal:terminal-vt terminal)))
            (if (= sr er)
                (crt.vt:vt-text vt sr (1+ sr) :start-col sc :end-col ec)
                ;; Three pieces: the tail of the first line, the whole of the
                ;; middle, and the head of the last.  vt-text takes a rectangle,
                ;; so a reading-order selection is assembled rather than asked
                ;; for in one call.
                (let ((parts '()))
                  (push (crt.vt:vt-text vt sr (1+ sr) :start-col sc) parts)
                  (when (> er (1+ sr))
                    (push (crt.vt:vt-text vt (1+ sr) er) parts))
                  (when (plusp ec)
                    (push (crt.vt:vt-text vt er (1+ er) :start-col 0 :end-col ec)
                          parts))
                  (format nil "~{~A~^~%~}" (nreverse parts))))))))))

(defun copy-selection (session)
  "Copy the selection to the pasteboard.  Returns what was copied, or NIL."
  (let ((text (selection-text session)))
    (when (and text (plusp (length text)))
      (set-clipboard-string (string-right-trim '(#\Space #\Newline) text)))))

(defun paste-clipboard (session)
  "Send the pasteboard to the child, bracketed if it asked for that.

Bracketed paste is what stops a shell from executing every newline in a pasted
block the moment it arrives, and what lets an editor tell pasted text from typed
text."
  (let ((text (clipboard-string))
        (terminal (session-terminal session)))
    (when (and text (plusp (length text)) terminal)
      (crt.terminal:terminal-paste terminal text)
      text)))

(defun clear-selection (session)
  (setf (session-selection session) nil))

;;; Events -------------------------------------------------------------------------

(defun mouse-event-point (view event)
  "EVENT's location in VIEW's coordinates, as (values X Y) in points."
  (let ((point (objc:invoke-into 'vector (objc:objc-object-pointer view)
                                 "convertPoint:fromView:"
                                 (objc:invoke-into 'vector event "locationInWindow")
                                 (cffi:null-pointer))))
    (values (aref point 0) (aref point 1))))

(defun event-modifiers (event)
  (objc:invoke-into 'integer event "modifierFlags"))

(defun reporting-mouse-p (session)
  "True when the child has asked for mouse events."
  (let ((terminal (session-terminal session)))
    (and terminal (crt.terminal:terminal-mouse-reporting-p terminal))))

(defun selection-gesture-p (session event)
  "True when this event is the user selecting rather than the child's business.

Shift always means selection, even when the child is reporting -- otherwise
there would be no way to select anything inside vim."
  (or (not (reporting-mouse-p session))
      (logtest (event-modifiers event) +modifier-shift+)))

(defun handle-mouse-down (session event)
  (multiple-value-bind (x y) (mouse-event-point (session-view session) event)
    (multiple-value-bind (col row) (view-point-to-cell session x y :clamp t)
      (if (selection-gesture-p session event)
          (setf (session-selection session) (make-selection row col row col)
                (session-dragging session) t)
          (report-mouse session event row col :press)))))

(defun handle-mouse-dragged (session event)
  (when (session-dragging session)
    (multiple-value-bind (x y) (mouse-event-point (session-view session) event)
      (multiple-value-bind (col row) (view-point-to-cell session x y :clamp t)
        (let ((selection (session-selection session)))
          (when selection
            (setf (selection-end-row selection) row
                  (selection-end-col selection) col)))))))

(defun handle-mouse-up (session event)
  (if (session-dragging session)
      (setf (session-dragging session) nil)
      (unless (selection-gesture-p session event)
        (multiple-value-bind (x y) (mouse-event-point (session-view session) event)
          (multiple-value-bind (col row) (view-point-to-cell session x y :clamp t)
            (report-mouse session event row col :release))))))

(defun handle-double-click (session event)
  "Select the word under the pointer."
  (multiple-value-bind (x y) (mouse-event-point (session-view session) event)
    (multiple-value-bind (col row inside) (view-point-to-cell session x y)
      (when inside
        (multiple-value-bind (start end) (word-bounds session row col)
          (setf (session-selection session) (make-selection row start row end)))))))

(defun word-character-p (character)
  (or (alphanumericp character)
      (find character "_-./~$@:")))

(defun word-bounds (session row col)
  "(values START END) of the word at ROW, COL, END exclusive."
  (let* ((terminal (session-terminal session))
         (cols (crt.terminal:terminal-cols terminal)))
    (crt.terminal:with-terminal-locked (terminal)
      (let ((vt (crt.terminal:terminal-vt terminal)))
        (flet ((wordish (c)
                 (let ((cell (crt.vt:vt-cell vt row c)))
                   (and cell (word-character-p (crt.vt:cell-char cell))))))
          (if (not (wordish col))
              (values col (1+ col))
              (let ((start col) (end col))
                (loop while (and (> start 0) (wordish (1- start))) do (decf start))
                (loop while (and (< end (1- cols)) (wordish (1+ end))) do (incf end))
                (values start (1+ end)))))))))

;;; The wheel ----------------------------------------------------------------------

(defun handle-scroll-wheel (session event)
  "Scroll the viewport, or report the wheel, or zoom.

Ctrl-wheel zooms, which is what upstream binds and what a terminal user
expects."
  (let* ((modifiers (event-modifiers event))
         (dy (objc:invoke-into 'double-float event "scrollingDeltaY"))
         (lines (round (max 1 (abs (/ dy 10.0))))))
    (cond
      ((logtest modifiers +modifier-control+)
       (if (plusp dy) (zoom-in session) (zoom-out session)))
      ((and (reporting-mouse-p session)
            (not (logtest modifiers +modifier-shift+)))
       (report-wheel session (plusp dy) lines))
      (t
       ;; The alternate screen has no scrollback of its own -- a full-screen
       ;; program owns the whole display -- so the wheel is turned into arrow
       ;; keys there, which is what makes a pager scroll.
       (if (crt.terminal:terminal-alternate-screen-p (session-terminal session))
           (crt.terminal:terminal-send-string
            (session-terminal session)
            (with-output-to-string (out)
              (dotimes (i (min lines 5))
                (write-string (format nil "~C[~C" #\Escape (if (plusp dy) #\A #\B))
                              out))))
           (scroll-viewport session (if (plusp dy) lines (- lines))))))))

;;; The context menu -----------------------------------------------------------------

(defun show-context-menu (session event)
  "Copy, Paste, and the profiles -- on right-click.

Right-click is given to the CHILD when it is reporting the mouse and Shift is
not held, for the same reason the other buttons are: a program that asked for
mouse events is entitled to all of them."
  (if (not (selection-gesture-p session event))
      (multiple-value-bind (x y) (mouse-event-point (session-view session) event)
        (multiple-value-bind (col row) (view-point-to-cell session x y :clamp t)
          (report-mouse session event row col :press)))
      (let ((menu (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" "")))
        (flet ((item (title action &optional (enabled t))
                 (let ((entry (objc:invoke
                               (objc:invoke "NSMenuItem" "alloc")
                               "initWithTitle:action:keyEquivalent:"
                               title (objc:coerce-to-selector action) "")))
                   (objc:invoke entry "setTarget:"
                                (objc:objc-object-pointer (menu-target)))
                   (objc:invoke entry "setEnabled:" (and enabled t))
                   (objc:invoke menu "addItem:" entry)
                   entry)))
          (item "Copy" "crtCopy:" (and (session-selection session)
                                       (not (selection-empty-p
                                             (session-selection session)))))
          (item "Paste" "crtPaste:" (and (clipboard-string) t))
          (objc:invoke menu "addItem:" (objc:invoke "NSMenuItem" "separatorItem"))
          (let* ((profiles (objc:invoke (objc:invoke "NSMenu" "alloc")
                                        "initWithTitle:" "Profiles"))
                 (entry (item "Profiles" "crtNothing:")))
            (dolist (profile crt.settings:+profiles+)
              (let ((sub (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                                      "initWithTitle:action:keyEquivalent:"
                                      (crt.settings:profile-name profile)
                                      (objc:coerce-to-selector "crtSetProfile:") "")))
                (objc:invoke sub "setTarget:"
                             (objc:objc-object-pointer (menu-target)))
                ;; A tick beside the one in use, which is the only feedback a
                ;; menu of fourteen otherwise identical items gives.
                (objc:invoke sub "setState:"
                             (if (eq profile (session-profile session)) 1 0))
                (objc:invoke profiles "addItem:" sub)))
            (objc:invoke entry "setSubmenu:" profiles)))
        (objc:invoke "NSMenu" "popUpContextMenu:withEvent:forView:"
                     menu event (objc:objc-object-pointer (session-view session)))
        (objc:release menu))))
