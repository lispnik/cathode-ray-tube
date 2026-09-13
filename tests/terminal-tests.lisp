;;;; tests/terminal-tests.lisp -- Tier 1: a real shell, end to end, headless.

(in-package #:cathode-ray-tube/tests)
(in-suite terminal)

(defun wait-until (predicate &key (timeout 5.0) (interval 0.02))
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (let ((value (funcall predicate)))
        (when value (return value)))
      (when (> (get-internal-real-time) deadline) (return nil))
      (sleep interval))))

(defun screen-text (terminal)
  "The whole screen as one string, for searching."
  (crt.terminal:with-terminal-locked (terminal)
    (crt.vt:vt-text (crt.terminal:terminal-vt terminal) 0 (crt.terminal:terminal-rows terminal))))

(defmacro with-terminal ((var &rest args) &body body)
  `(let ((,var (crt.terminal:make-terminal ,@args)))
     (unwind-protect (progn ,@body)
       (crt.terminal:terminal-close ,var))))

(test shell-runs-and-echoes
  "A real child on a real pty, its output parsed by a real VT."
  (with-terminal (terminal :rows 24 :cols 80
                           :command '("/bin/sh" "-c" "printf 'MARKER-OK'; sleep 5"))
    (is-true (wait-until (lambda () (search "MARKER-OK" (screen-text terminal))))
             "the child's output never reached the screen")))

(test input-reaches-the-child
  "Input is QUEUED and written by the reader thread, never from here -- a pty
master write blocks when the child is not reading, and doing it from the UI
thread would hang the window behind a slow program."
  (with-terminal (terminal :rows 24 :cols 80
                           :command '("/bin/sh" "-c" "read line; printf 'GOT:%s' \"$line\""))
    (crt.terminal:terminal-send-string terminal (format nil "hello~%"))
    (is-true (wait-until (lambda () (search "GOT:hello" (screen-text terminal))))
             "screen was: ~S" (screen-text terminal))))

(test resize-reaches-both-sides
  (with-terminal (terminal :rows 24 :cols 80
                           :command '("/bin/sh" "-c" "read x; stty size; sleep 5"))
    (crt.terminal:terminal-resize terminal 30 100)
    (is (= 30 (crt.terminal:terminal-rows terminal)) "the screen resized")
    (is (= 100 (crt.terminal:terminal-cols terminal)))
    (multiple-value-bind (rows cols)
        (crt.pty:get-winsize (crt.pty:pty-fd (crt.terminal:terminal-pty terminal)))
      (is (= 30 rows) "and so did the kernel's idea of the pty")
      (is (= 100 cols)))
    (crt.terminal:terminal-send-string terminal (format nil "~%"))
    (is-true (wait-until (lambda () (search "30 100" (screen-text terminal))))
             "the child never saw the new size; screen was ~S" (screen-text terminal))))

(test exit-is-noticed-and-reported
  (let ((reported nil))
    (with-terminal (terminal :rows 24 :cols 80
                             :command '("/bin/sh" "-c" "exit 3")
                             :on-exit (lambda (term status)
                                        (declare (ignore term))
                                        (setf reported status)))
      (is-true (wait-until (lambda () reported))
               "the on-exit hook never ran")
      (is (eql 3 reported) "exit status should be 3, got ~S" reported)
      (is-false (crt.terminal:terminal-alive-p terminal)))))

(test snapshot-copies-only-dirty-rows
  "A terminal at rest must copy nothing.

This is what makes taking a snapshot every frame affordable: the renderer asks
sixty times a second, and a screen that has not changed costs a lock and a
bit-vector copy."
  (with-terminal (terminal :rows 24 :cols 80
                           :command '("/bin/sh" "-c" "printf 'abc'; sleep 5"))
    (is-true (wait-until (lambda () (search "abc" (screen-text terminal)))))
    (let ((first (crt.terminal:terminal-snapshot terminal)))
      (is-true (crt.terminal:snapshot-painted first) "the first snapshot should be painted")
      (is (= 1 (sbit (crt.terminal:snapshot-dirty first) 0)) "row 0 changed"))
    ;; Nothing has happened since.
    (let ((second (crt.terminal:terminal-snapshot terminal)))
      (is-false (crt.terminal:snapshot-painted second)
                "nothing changed, so PAINTED must be false -- it is what decides
whether the burn-in accumulator advances, and advancing it every frame would
decay the phosphor at the display's refresh rate instead of the terminal's")
      (is (not (find 1 (crt.terminal:snapshot-dirty second))) "and no row is dirty"))))

(test snapshot-reuses-its-cells
  (with-terminal (terminal :rows 24 :cols 80
                           :command '("/bin/sh" "-c" "printf 'x'; sleep 5"))
    (is-true (wait-until (lambda () (search "x" (screen-text terminal)))))
    (let* ((first (crt.terminal:terminal-snapshot terminal))
           (cell (aref (aref (crt.terminal:snapshot-cells first) 0) 0))
           (second (crt.terminal:terminal-snapshot terminal)))
      (is (eq first second) "the snapshot structure itself is reused")
      (is (eq cell (aref (aref (crt.terminal:snapshot-cells second) 0) 0))
          "and so are the CELLs inside it"))))

(test title-arrives
  (with-terminal (terminal :rows 24 :cols 80
                           :command (list "/bin/sh" "-c"
                                          (format nil "printf '~C]0;my title~C'; sleep 5"
                                                  #\Escape (code-char 7))))
    (is-true (wait-until (lambda () (equal "my title" (crt.terminal:terminal-title terminal))))
             "title was ~S" (crt.terminal:terminal-title terminal))))

(test close-is-prompt-and-idempotent
  (let ((terminal (crt.terminal:make-terminal :rows 24 :cols 80
                                      :command '("/bin/sh" "-c" "sleep 30"))))
    (let ((start (get-internal-real-time)))
      (crt.terminal:terminal-close terminal)
      (let ((elapsed (/ (float (- (get-internal-real-time) start))
                        internal-time-units-per-second)))
        ;; The reader polls with a 250ms timeout and the close bounds its join
        ;; at two seconds, so anything near three means something is wedged.
        (is (< elapsed 3.0) "closing took ~,2Fs" elapsed)))
    (finishes (crt.terminal:terminal-close terminal))))
