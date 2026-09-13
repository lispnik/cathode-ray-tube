;;;; cathode-ray-tube.asd -- a CRT terminal emulator for macOS, in Common Lisp.
;;;;
;;;; Three systems.  #:cathode-ray-tube is the application; its lower half --
;;;; UTIL, SETTINGS, VT, PTY, TERMINAL, CLI -- is deliberately free of Objective-C
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

(asdf:defsystem #:cathode-ray-tube/portable
  :description "The half of cathode-ray-tube that has no Objective-C in it."
  :long-description
  "UTIL, SETTINGS, VT, PTY, TERMINAL and CLI: the arithmetic, the fourteen
profiles, the terminal core, the pseudo-terminal and the command line.  No AppKit, no Metal, no CoreText,
and no SBCL-isms -- so it loads and is tested on ECL, where none of those exist.

This system is not a convenience.  It is how the layering is ENFORCED: the
property that half this program can be reasoned about without a GPU is one
nobody maintains by intending to, and the ECL leg of CI is what makes it true.
The day someone reaches for SB-EXT:POSIX-ENVIRON in the pty layer, that leg goes
red and the SBCL legs do not."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  ;; No JSON library: com.inuoe.jzon does not compile on ECL, and a profile is a
  ;; flat object of numbers and strings.  See src/settings/json.lisp.
  :depends-on (#:cffi #:babel #:bordeaux-threads #:alexandria)
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
     (:module "settings"
      :serial t
      :components ((:file "json")
                   (:file "profile")
                   (:file "builtin-profiles")
                   (:file "derived")
                   (:file "store")))
     (:module "vt"
      :serial t
      :components ((:file "cells")
                   (:file "protocol")
                   (:file "ffi")
                   (:file "libvterm")))
     (:module "pty"
      :serial t
      :components ((:file "pty")
                   (:file "shell")))
     (:module "terminal"
      :serial t
      :components ((:file "terminal")))
     ;; The command line is pure arithmetic over strings and belongs here, not
     ;; in the application: CLI-TESTS runs on the ECL leg, and a parser tested
     ;; only where AppKit exists is a parser tested on one implementation.
     (:file "cli")))))

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
  :depends-on (#:cathode-ray-tube/portable
               #:objc #:cffi #:float-features #:trivial-main-thread)
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:module "metal"
      :serial t
      :components ((:file "package")
                   (:file "constants")
                   (:file "device")
                   (:file "resources")
                   (:file "library")
                   (:file "pass")
                   (:file "uniforms")))
     (:module "text"
      :serial t
      :components ((:file "font")
                   (:file "atlas")
                   (:file "pass")
                   (:file "overlay")))
     (:module "effects"
      :serial t
      :components ((:file "uniforms")
                   (:file "graph")))
     (:module "ui"
      :serial t
      :components ((:file "frameworks")
                   (:file "main-thread")
                   (:file "keyboard")
                   (:file "view")
                   (:file "window")
                   (:file "app")
                   (:file "session")
                   (:file "geometry")
                   (:file "actions")
                   (:file "mouse")))
     (:file "main")))))

(asdf:defsystem #:cathode-ray-tube/portable-tests
  :description "The suites that need neither a GPU nor a window -- the ECL leg."
  :depends-on (#:cathode-ray-tube/portable #:fiveam)
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "package")
                 (:file "math-tests")
                 (:file "color-tests")
                 (:file "profile-tests")
                 (:file "cli-tests")
                 (:file "vt-tests")
                 (:file "pty-tests")
                 (:file "terminal-tests")))))

(asdf:defsystem #:cathode-ray-tube/tests
  :description "Everything: the portable suites plus the GPU and window ones."
  :depends-on (#:cathode-ray-tube #:cathode-ray-tube/portable-tests #:fiveam)
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "gpu-package")
                 (:file "metal-tests")
                 (:file "text-tests")
                 (:file "effects-tests")
                 (:file "ui-tests"))))
  ;; FIVEAM:RUN! prints failures and returns NIL, and ASDF discards what a
  ;; TEST-OP returns -- which is exactly how a suite goes green with failing
  ;; tests.  The exit status comes from RUN-TESTS instead.
  :perform (asdf:test-op (o c)
             (unless (uiop:symbol-call :cathode-ray-tube/tests :run-tests)
               (error "The test suite failed."))))
