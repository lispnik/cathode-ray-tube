;;;; tests/vt-tests.lisp -- Tier 1: the terminal core, headless.
;;;;
;;;; Written against CRT.VT's PROTOCOL rather than against libvterm, so this is
;;;; a conformance suite for any backend rather than a test of one.  If
;;;; libghostty-vt ever arrives, these are what say whether it works.

(in-package #:cathode-ray-tube/tests)
(in-suite vt)

(defmacro with-vt ((var rows cols) &body body)
  `(let ((,var (vt:make-vt ,rows ,cols)))
     (unwind-protect (progn ,@body)
       (vt:vt-close ,var))))

(defun feed (term string)
  (vt:vt-write term (babel:string-to-octets string :encoding :utf-8)))

(defun row-text (term row)
  "ROW as a string, trailing blanks removed."
  (let ((cells (make-array (vt:vt-cols term) :initial-element nil)))
    (vt:vt-row-cells term row cells)
    (string-right-trim
     " " (map 'string (lambda (c) (if c (vt:cell-char c) #\Space)) cells))))

(test plain-text-lands
  (with-vt (term 24 80)
    (feed term "hello, world")
    (is (string= "hello, world" (row-text term 0)))))

(test cursor-advances
  (with-vt (term 24 80)
    (feed term "hello")
    (multiple-value-bind (row col visible) (vt:vt-cursor term)
      (is (= 0 row))
      (is (= 5 col))
      (is-true visible))))

(test newline-and-wrap
  (with-vt (term 24 10)
    (feed term (format nil "one~%~Ctwo" #\Return))
    (is (string= "one" (row-text term 0)))
    (is (string= "two" (row-text term 1))))
  (with-vt (term 24 5)
    ;; Six characters into a five-column screen must wrap, not truncate.
    (feed term "abcdef")
    (is (string= "abcde" (row-text term 0)))
    (is (string= "f" (row-text term 1)))))

(test sgr-sets-an-indexed-foreground
  (with-vt (term 24 80)
    (feed term (format nil "~C[31mR" #\Escape))
    (let ((cell (vt:vt-cell term 0 0)))
      (is (char= #\R (vt:cell-char cell)))
      (is (vt:vt-color-indexed-p (vt:cell-fg cell)))
      (is (= 1 (vt:vt-color-index (vt:cell-fg cell))) "ESC[31m is palette entry 1"))))

(test sgr-truecolor
  (with-vt (term 24 80)
    (feed term (format nil "~C[38;2;10;20;30mX" #\Escape))
    (let ((fg (vt:cell-fg (vt:vt-cell term 0 0))))
      (is (not (vt:vt-color-indexed-p fg)))
      (is (= 10 (vt:vt-color-red fg)))
      (is (= 20 (vt:vt-color-green fg)))
      (is (= 30 (vt:vt-color-blue fg))))))

(test sgr-attributes
  (with-vt (term 24 80)
    (feed term (format nil "~C[1mB~C[0m~C[3mI~C[0m~C[4mU~C[0m~C[7mR"
                       #\Escape #\Escape #\Escape #\Escape
                       #\Escape #\Escape #\Escape))
    (flet ((attrs-at (col) (vt:cell-attrs (vt:vt-cell term 0 col))))
      (is (vt:attr-set-p (attrs-at 0) vt:+attr-bold+))
      (is (vt:attr-set-p (attrs-at 1) vt:+attr-italic+))
      (is (vt:attr-set-p (attrs-at 2) vt:+attr-underline+))
      (is (= 1 (vt:cell-underline-style (attrs-at 2))) "single underline")
      (is (vt:attr-set-p (attrs-at 3) vt:+attr-reverse+))
      (is (not (vt:attr-set-p (attrs-at 0) vt:+attr-italic+))
          "attributes must not leak between cells"))))

(test erase-clears-the-screen
  (with-vt (term 24 80)
    (feed term "something")
    (is (string/= "" (row-text term 0)))
    (feed term (format nil "~C[2J~C[H" #\Escape #\Escape))
    (is (string= "" (row-text term 0)))))

(test utf8-and-wide-characters
  (with-vt (term 24 80)
    (feed term "漢字x")
    (let ((c0 (vt:vt-cell term 0 0))
          (c1 (vt:vt-cell term 0 1))
          (c2 (vt:vt-cell term 0 2)))
      (is (char= (code-char #x6F22) (vt:cell-char c0)))
      (is (= 2 (vt:cell-width c0)) "a CJK glyph occupies two columns")
      ;; The second column of a wide glyph is width 0: it exists, and nothing is
      ;; drawn in it.  The renderer has to skip it rather than draw a blank.
      (is (= 0 (vt:cell-width c1)))
      (is (char= (code-char #x5B57) (vt:cell-char c2))))))

(test combining-marks
  (with-vt (term 24 80)
    ;; e followed by COMBINING ACUTE ACCENT: one cell, one mark.
    (feed term (format nil "e~C" (code-char #x0301)))
    (let ((cell (vt:vt-cell term 0 0)))
      (is (char= #\e (vt:cell-char cell)))
      (is (equal (list (code-char #x0301)) (vt:cell-combining cell))))))

(test damage-tracking
  (with-vt (term 24 80)
    (vt:vt-clear-dirty term)
    (is (not (vt:vt-dirty-p term)) "nothing is dirty after a clear")
    (feed term "x")
    (is (vt:vt-dirty-p term) "writing makes something dirty")
    (is (= 1 (sbit (vt:vt-dirty-rows term) 0)) "specifically row 0")
    (is (= 0 (sbit (vt:vt-dirty-rows term) 5)) "and not row 5")))

(test scrollback-grows-and-reads-back
  (with-vt (term 5 20)
    (dotimes (i 20)
      (feed term (format nil "line ~D~C~%" i #\Return)))
    (is (plusp (vt:vt-scrollback-length term))
        "lines scrolled off the top must be kept")
    ;; The most recent scrolled-off line is index 0.
    (let ((line (vt:vt-scrollback-line term 0)))
      (is (not (null line)))
      (is (search "line" (map 'string #'vt:cell-char line))))))

(test resize-reflows
  (with-vt (term 24 80)
    (feed term "hello")
    (vt:vt-resize term 30 100)
    (is (= 30 (vt:vt-rows term)))
    (is (= 100 (vt:vt-cols term)))
    (is (= 30 (length (vt:vt-dirty-rows term)))
        "the dirty map must be resized with the screen")
    (is (string= "hello" (row-text term 0)) "text survives a resize")))

(test title-from-osc
  (with-vt (term 24 80)
    (feed term (format nil "~C]0;a title~C" #\Escape (code-char 7)))
    (is (string= "a title" (vt:vt-title term)))))

(test text-extraction
  (with-vt (term 24 80)
    (feed term "copy me")
    (is (string= "copy me" (string-right-trim " " (vt:vt-text term 0 1))))))

(test output-hook-receives-replies
  "A device-status request makes the terminal want to WRITE.  The pty thread
owns the fd, so the hook queues rather than writing."
  (with-vt (term 24 80)
    (let ((replies '()))
      (setf (vt:vt-output-hook term) (lambda (octets) (push octets replies)))
      ;; ESC[6n -- report cursor position.
      (feed term (format nil "~C[6n" #\Escape))
      (is (not (null replies)) "the terminal should have replied")
      (let ((text (babel:octets-to-string (first replies))))
        (is (search "R" text) "a cursor-position report ends in R, got ~S" text)))))

(test row-cells-reuses-structures
  "VT-ROW-CELLS must reuse the CELLs it is given.

The renderer walks every dirty row every frame; allocating a fresh structure per
character would make tens of thousands of short-lived objects a second for no
reason at all."
  (with-vt (term 24 80)
    (feed term "abc")
    (let* ((cells (make-array 80))
           (_ (dotimes (i 80) (setf (aref cells i) (vt:make-cell))))
           (first-cell (aref cells 0)))
      (declare (ignore _))
      (vt:vt-row-cells term 0 cells)
      (is (eq first-cell (aref cells 0)) "the same CELL object must come back")
      (is (char= #\a (vt:cell-char (aref cells 0))) "filled in place"))))
