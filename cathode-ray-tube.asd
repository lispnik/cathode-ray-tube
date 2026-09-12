;;;; cathode-ray-tube.asd -- a CRT terminal emulator for macOS, in Common Lisp.
;;;;
;;;; Three systems.  #:cathode-ray-tube is the application; its lower half --
;;;; UTIL, SETTINGS, VT, PTY, TERMINAL -- is deliberately free of Objective-C
;;;; and of SBCL-isms, so it loads and is tested on ECL where there is no window
;;;; and no Metal.  #:cathode-ray-tube/app builds bin/cathode-ray-tube.
;;;; #:cathode-ray-tube/tests is the FiveAM suite.
;;;;
;;;; The .app bundle is NOT here.  It lives in cathode-ray-tube-bundle.asd,
;;;; because :DEFSYSTEM-DEPENDS-ON is resolved when a .asd is READ rather than
;;;; when the system it belongs to is built -- so declaring the bundle here
;;;; would make asdf-macos-app a hard requirement for merely loading the
;;;; library.  See the header of utc-status-app-bundle.asd, where that bug was
;;;; found the hard way.

(asdf:defsystem #:cathode-ray-tube
  :description "A terminal emulator that looks like a cathode-ray tube."
  :long-description
  "A port of cool-retro-term (Filippo Scognamiglio, GPL-3) to a native macOS
Cocoa application written in Common Lisp: the GLSL effects translated to Metal
Shading Language, the window and render graph driven through the objc bindings,
and terminal emulation by a vendored libvterm."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :homepage "https://github.com/lispnik/cathode-ray-tube"
  :depends-on (#:objc #:cffi #:babel #:bordeaux-threads #:alexandria
               #:float-features #:com.inuoe.jzon)
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:module "util"
      :serial t
      :components ((:file "package")
                   (:file "math")
                   (:file "color")
                   (:file "resource")
                   (:file "foreign")))
     (:module "metal"
      :serial t
      :components ((:file "package")
                   (:file "constants")
                   (:file "device")
                   (:file "resources")
                   (:file "library")
                   (:file "pass")))))))

(asdf:defsystem #:cathode-ray-tube/tests
  :description "The FiveAM suite."
  :depends-on (#:cathode-ray-tube #:fiveam)
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "package")
                 (:file "math-tests")
                 (:file "color-tests")
                 (:file "metal-tests"))))
  ;; FIVEAM:RUN! prints failures and returns NIL, and ASDF discards what a
  ;; TEST-OP returns -- which is exactly how a suite goes green with failing
  ;; tests.  The exit status comes from RUN-TESTS instead.
  :perform (asdf:test-op (o c)
             (unless (uiop:symbol-call :cathode-ray-tube/tests :run-tests)
               (error "The test suite failed."))))
