;;;; tests/seam-tests.lisp -- the layering, enforced by reading the sources.
;;;;
;;;; The lower half of this program -- UTIL, SETTINGS, VT, PTY, TERMINAL, CLI --
;;;; is free of Objective-C and of implementation-specific packages, which is
;;;; what lets it load and be tested on ECL where there is no window and no GPU.
;;;;
;;;; That property is not maintained by intending to maintain it.  Until now the
;;;; only thing checking it was the ECL leg of CI: four minutes away, and once
;;;; already red for a network error that had nothing to do with the code.  This
;;;; is the same check in under a second, and it runs on both implementations.
;;;;
;;;; The temptation it exists to catch is a real and reasonable one.  SB-ALIEN
;;;; can do things CFFI cannot -- measured: it receives a four-int struct BY
;;;; VALUE in a callback, which is exactly what libvterm's damage(VTermRect,
;;;; void*) needs and which CFFI signals CASE-FAILURE on.  Reaching for it in
;;;; src/vt/ would work, on SBCL, and would quietly cost the whole ECL leg.

(in-package #:cathode-ray-tube/tests)
(in-suite portable)

(defparameter +portable-modules+
  '("src/util" "src/settings" "src/vt" "src/pty" "src/terminal")
  "The directories that must stay implementation-agnostic.")

(defparameter +portable-files+ '("src/cli.lisp")
  "And the loose files.  The command line is arithmetic over strings.")

(defconstant +minimum-portable-files+ 15
  "A FLOOR under the count, asserted before anything else.

`No offenders' is true of an empty scan, so a test that enumerates files has to
prove it enumerated some.  This is not hypothetical: ASDF:SYSTEM-RELATIVE-PATHNAME
QUOTES a wildcard, so \"src/*.lisp\" names one file that does not exist and
DIRECTORY returns NIL -- a scan that finds nothing and reports success.")

(defun portable-source-files ()
  "Every .lisp file in the portable half, as pathnames."
  (let ((root (asdf:system-source-directory :cathode-ray-tube/portable)))
    (append
     (loop for module in +portable-modules+
           append (uiop:directory-files (merge-pathnames
                                         (concatenate 'string module "/") root)
                                        "*.lisp"))
     (loop for file in +portable-files+
           for path = (merge-pathnames file root)
           when (probe-file path) collect path))))

(defparameter +forbidden-prefixes+
  '("sb-alien:" "sb-alien::" "sb-sys:" "sb-sys::" "sb-ext:" "sb-ext::"
    "sb-thread:" "sb-thread::" "sb-int:" "sb-kernel:" "sb-kernel::"
    "sb-unix:" "sb-unix::" "sb-c:" "sb-vm:"
    "ext:" "si:" "ffi:"
    "objc:" "objc::" "cocoa:" "objc.runloop:")
  "Package prefixes that must not appear below the seam.

The SB- ones are SBCL, the EXT/SI/FFI ones are ECL, and the OBJC ones are the
bridge.  Each of them would make the file load on one implementation or one
platform and not the other, which is the whole thing this layer exists to avoid.

CFFI is not here and is deliberately allowed: it is portable, it is how libvterm
is reached at all, and it works the same on both.")

(defun code-only (text)
  "TEXT with its comments and string literals blanked out.

Necessary, and the failure without it is instructive: the two files that tripped
this test on its first honest run were doing so in PROSE.  pty.lisp explains that
the environment is read from C's `environ' RATHER THAN from SB-EXT:POSIX-ENVIRON,
and libvterm.lisp quotes the error a null handle produces,
`SB-SYS:SYSTEM-AREA-POINTER'.  Both comments exist precisely because someone
might otherwise reach for those, and a guard that forbade naming the trap would
be a guard against writing the warning.

Blanked rather than deleted, so that a reported position still means something."
  (let ((out (copy-seq text))
        (i 0)
        (n (length text)))
    (flet ((blank (from to)
             (loop for k from from below (min to n)
                   unless (char= (char text k) #\Newline)
                     do (setf (char out k) #\Space))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ;; #\; and friends: a character literal, not a comment.
                   ((and (char= c #\#) (< (+ i 1) n) (char= (char text (1+ i)) #\\))
                    (incf i 3))
                   ((char= c #\;)
                    (let ((end (or (position #\Newline text :start i) n)))
                      (blank i end)
                      (setf i end)))
                   ((and (char= c #\#) (< (+ i 1) n) (char= (char text (1+ i)) #\|))
                    (let ((end (or (search "|#" text :start2 (+ i 2)) n)))
                      (blank i (min n (+ end 2)))
                      (setf i (min n (+ end 2)))))
                   ((char= c #\")
                    (let ((j (1+ i)))
                      (loop while (and (< j n) (char/= (char text j) #\"))
                            do (incf j (if (char= (char text j) #\\) 2 1)))
                      (blank i (min n (1+ j)))
                      (setf i (min n (1+ j)))))
                   (t (incf i))))))
    out))

(defun symbol-constituent-p (character)
  (or (alphanumericp character)
      (find character "-+*/=<>!?%&$_.:")))

(defun package-reference-position (text prefix)
  "Where TEXT names the package PREFIX, or NIL.

The boundary check is the whole of it.  A plain SEARCH for \"ffi:\" matches
inside \"cffi:\", and \"ext:\" matches inside \"sb-ext:\" -- so the first
version of this test reported six offenders, every one of them CFFI being used
exactly as intended.  A prefix counts only where what precedes it could not be
part of a longer symbol."
  (let ((start 0))
    (loop
      (let ((at (search prefix text :start2 start :test #'char-equal)))
        (cond ((null at) (return nil))
              ((or (zerop at)
                   (not (symbol-constituent-p (char text (1- at)))))
               (return at))
              (t (setf start (1+ at))))))))

(test the-portable-half-has-no-implementation-specific-code
  "Read every source file below the seam and look for the packages that would
tie it to one implementation.

Asserting on the TEXT rather than on whether it loads, because `it loads here'
is exactly the thing that is true right up until someone runs it elsewhere."
  (let ((files (portable-source-files)))
    (is (>= (length files) +minimum-portable-files+)
        "the scan found ~D files, which is fewer than the ~D that certainly
exist -- so it is not scanning what it thinks it is"
        (length files) +minimum-portable-files+)
    (dolist (file files)
      (let ((text (code-only (uiop:read-file-string file))))
        (dolist (prefix +forbidden-prefixes+)
          (let ((at (package-reference-position text prefix)))
            (is-false at
                      "~A uses ~A, which would make the portable half load on
one implementation and not the other.  If it is genuinely needed, the code
belongs above the seam -- in CRT.UI, CRT.METAL or CRT.TEXT."
                      (file-namestring file) prefix)))))))

(test the-portable-half-has-no-read-time-conditionals
  "#+sbcl below the seam is the same problem wearing a different hat: it makes
the file mean two different things and only one of them is ever tested here."
  (dolist (file (portable-source-files))
    (let ((text (code-only (uiop:read-file-string file))))
      (dolist (marker '("#+sbcl" "#-sbcl" "#+ecl" "#-ecl"))
        (is-false (search marker text :test #'char-equal)
                  "~A has a ~A" (file-namestring file) marker)))))
