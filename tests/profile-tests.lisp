;;;; tests/profile-tests.lisp -- Tier 1: the numbers that define the look.

(in-package #:cathode-ray-tube/tests)
(in-suite profile)

(defun ~~ (a b &optional (tol 1d-6)) (< (abs (- a b)) tol))

(test all-fourteen-are-present
  (is (= 14 (length crt.settings:+profiles+)))
  (dolist (name '("Default Amber" "Monochrome Green" "Deep Blue" "Commodore 64"
                  "Commodore PET" "Apple ][" "Atari 400" "IBM VGA 8x16"
                  "IBM 3278 Reborn" "Neon Cyan" "Ghost Terminal" "Plasma"
                  "Boring" "E-Ink"))
    (is (crt.settings:find-profile name) "~S is missing" name)))

(test profiles-match-the-qml
  "Re-extract from cool-retro-term and assert this file still agrees.

The checked-in profiles are generated, not typed, and this is what keeps them
that way.  Skips where the upstream checkout is not present, since it is a
sibling rather than a dependency."
  (let ((script (asdf:system-relative-pathname
                 :cathode-ray-tube "tools/extract-profiles.py"))
        (upstream (merge-pathnames "Projects/cool-retro-term/"
                                   (user-homedir-pathname))))
    (if (not (and (probe-file script)
                  (probe-file (merge-pathnames "app/qml/ApplicationSettings.qml"
                                               upstream))))
        (skip "no cool-retro-term checkout to compare against")
        (let ((fresh (uiop:run-program
                      (list (namestring script) (namestring upstream))
                      :output :string))
              (current (uiop:read-file-string
                        (asdf:system-relative-pathname
                         :cathode-ray-tube "src/settings/builtin-profiles.lisp"))))
          (is (string= (string-right-trim '(#\Newline) fresh)
                       (string-right-trim '(#\Newline) current))
              "src/settings/builtin-profiles.lisp no longer matches the QML.~%~
               Regenerate it: tools/extract-profiles.py > src/settings/builtin-profiles.lisp")))))

(test default-amber-is-what-it-should-be
  "Spot-check the profile everything else is judged against."
  (let ((p (crt.settings:find-profile "Default Amber")))
    (is (string= "#ff8100" (crt.settings:profile-font-color p)))
    (is (string= "#000000" (crt.settings:profile-background-color p)))
    (is (~~ 0.6d0 (crt.settings:profile-bloom p)))
    (is (~~ 0.3d0 (crt.settings:profile-burn-in p)))
    (is (~~ 0.2d0 (crt.settings:profile-chroma-color p)))
    (is (= 0 (crt.settings:profile-rasterization p)))
    (is (~~ 0.2d0 (crt.settings:profile-screen-curvature p)))))

(test every-profile-round-trips-through-json
  "A profile must survive a trip through cool-retro-term's own file format."
  (dolist (p crt.settings:+profiles+)
    (let ((back (crt.settings:profile-from-json (crt.settings:profile-to-json p))))
      (is (string= (crt.settings:profile-name p) (crt.settings:profile-name back))
          "~A lost its name" (crt.settings:profile-name p))
      (is (string= (crt.settings:profile-font-color p) (crt.settings:profile-font-color back)))
      (is (~~ (crt.settings:profile-bloom p) (crt.settings:profile-bloom back) 1d-4)
          "~A: bloom ~F became ~F" (crt.settings:profile-name p)
          (crt.settings:profile-bloom p) (crt.settings:profile-bloom back))
      (is (eq (crt.settings:profile-blinking-cursor p)
              (crt.settings:profile-blinking-cursor back))))))

(test a-partial-profile-overrides-only-what-it-names
  "loadProfileString applies each key only when it is defined, so a profile with
three keys changes three things -- which is what makes a hand-written override
file usable."
  (let ((p (crt.settings:profile-from-alist '(("bloom" . 0.9d0))
                                        :into (copy-structure
                                               (crt.settings:find-profile "Default Amber")))))
    (is (~~ 0.9d0 (crt.settings:profile-bloom p)) "the named key changed")
    (is (string= "#ff8100" (crt.settings:profile-font-color p))
        "an unnamed key must be left alone")))

(test unknown-keys-are-ignored
  (finishes (crt.settings:profile-from-alist '(("somethingFromTheFuture" . 1)))))

(test wrong-version-is-refused
  (signals error
    (crt.settings:profile-from-json "{\"version\": 99, \"bloom\": 0.5}")))

;;; Derived values --------------------------------------------------------------

(test burn-in-is-a-reciprocal
  "A LARGER burnIn setting is a SLOWER fade, because the shader gets a rate."
  (let ((slow (crt.settings:make-profile :burn-in 1.0d0))
        (fast (crt.settings:make-profile :burn-in 0.0d0)))
    (is (~~ (/ 1d0 1.6d0) (crt.settings:burn-in-fade-time slow)))
    (is (~~ (/ 1d0 0.16d0) (crt.settings:burn-in-fade-time fast)))
    (is (< (crt.settings:burn-in-fade-time slow) (crt.settings:burn-in-fade-time fast))
        "more burn-in must mean a lower decay rate")))

(test screen-radius-is-pixels-not-a-fraction
  (is (~~ 4.0d0 (crt.settings:screen-radius (crt.settings:make-profile :screen-radius 0d0)))
      "even zero rounds by four pixels")
  (is (~~ 120.0d0 (crt.settings:screen-radius (crt.settings:make-profile :screen-radius 1d0))))
  (is (~~ 27.2d0 (crt.settings:screen-radius (crt.settings:make-profile :screen-radius 0.2d0)))))

(test margin-accounts-for-the-corner
  "A rounded corner eats into the usable rectangle; the margin compensates."
  (let* ((square (crt.settings:make-profile :margin 0d0 :screen-radius 0d0))
         (round (crt.settings:make-profile :margin 0d0 :screen-radius 1d0)))
    (is (< (crt.settings:margin square) (crt.settings:margin round))
        "a rounder screen needs a bigger margin, or text runs under the bezel")
    (is (~~ (+ 1.0d0 (* (- 1d0 (/ 1d0 (sqrt 2d0))) 120d0))
            (crt.settings:margin round)))))

(test frame-enabled-follows-any-of-three
  (is-false (crt.settings:frame-enabled-p
             (crt.settings:make-profile :ambient-light 0d0 :frame-size 0d0
                                    :screen-curvature 0d0)))
  (dolist (key '(:ambient-light :frame-size :screen-curvature))
    ;; The override goes FIRST: with duplicate keyword arguments the leftmost
    ;; wins, so putting it last would have no effect at all -- and the test
    ;; would then be asserting that all-zero enables the frame, which it does
    ;; not, which is how this was noticed.
    (is-true (crt.settings:frame-enabled-p
              (apply #'crt.settings:make-profile
                     (append (list key 0.1d0)
                             (list :ambient-light 0d0 :frame-size 0d0
                                   :screen-curvature 0d0))))
             "~A alone should enable the frame" key)))

(test rasterization-fades-out-when-undersampled
  "Scanlines need two device pixels per terminal pixel to exist and four to be
clean; below that they alias, so they fade rather than shimmer."
  (is (~~ 0d0 (crt.settings:rasterization-intensity 100d0 100d0 100d0 100d0))
      "1x oversampling: no scanlines")
  (is (~~ 0d0 (crt.settings:rasterization-intensity 100d0 100d0 200d0 200d0))
      "2x: still none")
  (is (~~ 1d0 (crt.settings:rasterization-intensity 100d0 100d0 400d0 400d0))
      "4x: full strength")
  (is (< 0d0 (crt.settings:rasterization-intensity 100d0 100d0 300d0 300d0) 1d0)
      "3x: partway up the ramp"))

(test contrast-moves-the-colours-apart
  (let ((low (crt.settings:make-profile :contrast 0d0 :font-color "#ffffff"
                                    :background-color "#000000"))
        (high (crt.settings:make-profile :contrast 1d0 :font-color "#ffffff"
                                     :background-color "#000000")))
    (flet ((spread (p) (- (crt.util:rgba-r (crt.settings:derived-font-color p))
                          (crt.util:rgba-r (crt.settings:derived-background-color p)))))
      (is (> (spread high) (spread low))
          "raising contrast must separate the derived colours"))
    (is (> (crt.util:rgba-r (crt.settings:derived-font-color low)) 0.1d0)
        "even at zero contrast the mix is 0.7, not 0 -- washed out, not blank")))

(test normalized-window-scale-keeps-geometry-constant
  (is (~~ 1d0 (crt.settings:normalized-window-scale 1024d0 1024d0)))
  (is (> (crt.settings:normalized-window-scale 512d0 512d0)
         (crt.settings:normalized-window-scale 2048d0 2048d0))
      "a smaller window gets a larger scale, so the curvature looks the same"))

(test every-profile-names-a-font-we-ship
  "All fourteen profiles must resolve to a bundled face.

The mapping is upstream's fontName strings, kept unchanged so that a profile
file can be read by either program.  A profile whose face does not resolve opens
in the default one, silently -- so this is what says whether that is happening."
  (dolist (profile crt.settings:+profiles+)
    (let* ((name (crt.settings:profile-font-name profile))
           (face (crt.text:font-for-profile-name name)))
      (is-true face "~A names the font ~S, which maps to nothing"
          (crt.settings:profile-name profile) name)
      (when face
        (is-true (probe-file (crt.text:bundled-font-path face))
            "~A wants ~S -> ~S, and that file is not there"
            (crt.settings:profile-name profile) name face)))))

(test the-font-table-covers-upstreams
  "Every face cool-retro-term ships is nameable here."
  (is (= 24 (length crt.text:+profile-font-names+)))
  (dolist (entry crt.text:+profile-font-names+)
    (is-true (probe-file (crt.text:bundled-font-path (cdr entry)))
        "~S -> ~S is missing its file" (car entry) (cdr entry))))
