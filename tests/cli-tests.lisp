;;;; tests/cli-tests.lisp -- Tier 1: the command line and the settings store.

(in-package #:cathode-ray-tube/tests)
(in-suite cli)

(defun parse (&rest arguments)
  (cathode-ray-tube:parse-command-line arguments))

(test flags-that-print-and-exit
  (is (search "cathode-ray-tube" (getf (parse "--help") :exit)))
  (is (search "cathode-ray-tube" (getf (parse "-h") :exit)))
  (is (search "." (getf (parse "--version") :exit)) "a version has a dot in it")
  (is (search "Default Amber" (getf (parse "--list-profiles") :exit))))

(test geometry
  (let ((options (parse "--geometry" "132x43")))
    (is (= 132 (getf options :columns)))
    (is (= 43 (getf options :rows))))
  (is (getf (parse "--geometry" "nonsense") :error))
  (is (getf (parse "--geometry" "80") :error) "needs both halves")
  (is (getf (parse "--geometry") :error) "and a value at all"))

(test dash-e-swallows-the-rest
  "Everything after -e belongs to the CHILD, including things that look like our
own flags.  That is what makes `-e ssh host -v' work rather than printing our
version and exiting."
  (let ((options (parse "-p" "Plasma" "-e" "ssh" "host" "-v" "--help")))
    (is (equal '("ssh" "host" "-v" "--help") (getf options :command)))
    (is (string= "Plasma" (getf options :profile)))
    (is (null (getf options :exit))
        "--help after -e is the child's argument, not ours")))

(test unknown-options-are-refused
  (is (getf (parse "--wat") :error))
  (is (null (getf (parse "-psn_0_12345") :error))
      "AppKit passes -psn_ to a bundle launched from the Finder; not an error"))

(test simple-flags
  (is-true (getf (parse "--default-settings") :default-settings))
  (is-true (getf (parse "--fullscreen") :fullscreen))
  (is (equal '(:effects nil) (parse "--no-effects")))
  (is (string= "/tmp" (getf (parse "--workdir" "/tmp") :directory))))

(test the-command-line-tokeniser
  "A port of GLib's, which is what the custom-command setting has always been
parsed by.  SPLIT-STRING would break on the first path containing a space."
  (is (equal '("ls" "-la") (cathode-ray-tube:tokenize-command-line "ls -la")))
  (is (equal '("/Applications/My App/bin/thing" "x")
             (cathode-ray-tube:tokenize-command-line
              "\"/Applications/My App/bin/thing\" x")))
  (is (equal '("a b" "c")
             (cathode-ray-tube:tokenize-command-line "'a b' c")))
  (is (equal '("a b") (cathode-ray-tube:tokenize-command-line "a\\ b")))
  (is (equal '() (cathode-ray-tube:tokenize-command-line "   ")))
  (is (equal '("echo" "") (cathode-ray-tube:tokenize-command-line "echo \"\""))
      "an explicitly empty argument survives"))

;;; The settings store --------------------------------------------------------------

(defmacro with-temporary-settings ((path) &body body)
  `(let ((,path (merge-pathnames (format nil "crt-settings-~D.lisp" (random 100000))
                                 (uiop:temporary-directory))))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,path)))))

(test settings-round-trip
  (with-temporary-settings (path)
    (let ((settings (crt.settings:make-settings :profile-name "Plasma"
                                                :effects-frame-skip 5
                                                :bloom-quality 0.75d0
                                                :columns 132 :rows 43)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (prin1 (crt.settings::settings-to-plist settings) out))
      (let ((back (crt.settings:load-settings :path path)))
        (is (string= "Plasma" (crt.settings:settings-profile-name back)))
        (is (= 5 (crt.settings:settings-effects-frame-skip back)))
        (is (= 0.75d0 (crt.settings:settings-bloom-quality back)))
        (is (= 132 (crt.settings:settings-columns back)))
        (is (= 43 (crt.settings:settings-rows back)))))))

(test a-broken-settings-file-falls-back
  "A terminal that refuses to start because of its own preferences file is worse
than one that forgets your preferences."
  (with-temporary-settings (path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "{ this is not a plist" out))
    (let ((settings (crt.settings:load-settings :path path)))
      (is (string= "Default Amber" (crt.settings:settings-profile-name settings))))))

(test settings-values-are-type-checked
  "A hand-edited file can put a string where a number goes, and the failure would
otherwise surface far away -- as a division by a string in the middle of a
frame."
  (with-temporary-settings (path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (prin1 '(:version 1 :profile-name 42 :bloom-quality "lots"
               :effects-frame-skip 9999 :columns -5)
             out))
    (let ((settings (crt.settings:load-settings :path path)))
      (is (string= "Default Amber" (crt.settings:settings-profile-name settings))
          "a number where a string goes falls back")
      (is (= 0.5d0 (crt.settings:settings-bloom-quality settings)))
      (is (= 3 (crt.settings:settings-effects-frame-skip settings))
          "out of range falls back too")
      (is (= 80 (crt.settings:settings-columns settings))))))

(test settings-cannot-execute-what-they-read
  "READ with *READ-EVAL* off.  A settings file is a file on disk, and #. would
otherwise let it run anything the moment the program starts."
  (with-temporary-settings (path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "(:version 1 :columns #.(error \"executed\"))" out))
    (finishes (crt.settings:load-settings :path path))))

(test custom-profiles-cannot-shadow-built-ins
  "Refused rather than shadowed: a custom profile called \"Default Amber\" that
quietly replaced the built-in one would leave no way back to it."
  (let ((settings (crt.settings:make-settings)))
    (signals error
      (crt.settings:save-custom-profile "Default Amber"
                                        (crt.settings:find-profile "Plasma")
                                        settings))))

(test all-profiles-includes-custom-ones
  (let* ((settings (crt.settings:make-settings))
         (mine (crt.settings:find-profile "Plasma")))
    (setf (crt.settings:settings-custom-profiles settings)
          (list (cons "Mine" (crt.settings:profile-to-json mine))))
    (let ((names (mapcar #'crt.settings:profile-name
                         (crt.settings:all-profiles settings))))
      (is (= 15 (length names)) "fourteen built-ins plus one")
      (is-true (member "Mine" names :test #'string=))
      (is-true (member "Default Amber" names :test #'string=)))
    (is-true (crt.settings:find-any-profile "Mine" settings))))

(test a-corrupt-custom-profile-is-skipped-not-fatal
  (let ((settings (crt.settings:make-settings)))
    (setf (crt.settings:settings-custom-profiles settings)
          (list (cons "Broken" "{ not json")))
    (is (= 14 (length (crt.settings:all-profiles settings)))
        "the broken one is dropped and the built-ins still load")))
