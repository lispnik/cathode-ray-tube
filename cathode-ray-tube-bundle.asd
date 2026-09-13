;;;; cathode-ray-tube-bundle.asd -- the .app, in its own file.
;;;;
;;;; SEPARATE FROM cathode-ray-tube.asd ON PURPOSE.  :DEFSYSTEM-DEPENDS-ON is
;;;; resolved when a .asd is READ, not when the system it belongs to is built --
;;;; so with this system in the main file, merely LOADING #:cathode-ray-tube
;;;; would require asdf-macos-app to be present, and anyone who had cloned
;;;; without it would get `Component "asdf-macos-app" not found'.  CI would never
;;;; see it, because CI always has both.
;;;;
;;;; utc-status-app's bundle file records the same bug, found the same way.  The
;;;; `test-no-bundle' target exists to keep it fixed.

(asdf:defsystem #:cathode-ray-tube-bundle
  :description "cathode-ray-tube.app."
  :defsystem-depends-on ("asdf-macos-app")
  :class :macos-app-system
  :build-operation "macos-app-op"
  :depends-on (#:cathode-ray-tube)
  :entry-point "cathode-ray-tube:main"
  :version "0.1.0"
  :bundle-identifier "com.lispnik.cathode-ray-tube"
  :bundle-name "cathode-ray-tube"
  :bundle-executable "cathode-ray-tube"
  :bundle-category "public.app-category.developer-tools"
  :bundle-copyright "GPL-3.0-or-later. A port of cool-retro-term by Filippo Scognamiglio."
  ;; NSPrincipalClass, so AppKit is initialised the way it would be for any
  ;; Cocoa application rather than halfway through our own startup.
  :bundle-principal-class "NSApplication"
  ;; CADisplayLink -- the frame clock -- is macOS 14 and later.  Declared here as
  ;; well as in vendor/Makefile's -mmacosx-version-min so the two cannot drift.
  :bundle-minimum-system-version "14.0"
  :bundle-high-resolution t
  ;; Drawn by tools/icon.lisp using the same objc bindings the application is
  ;; built on, so there is no checked-in artwork and changing it is an edit.
  ;; asdf-macos-app converts the PNG with sips and iconutil.
  :bundle-icon "res/icon.png"
  ;; The shaders, the noise texture and twenty-six fonts.  CRT.UTIL:RESOURCE
  ;; finds them here in a bundle and under res/ in a checkout, and nothing else
  ;; in the program knows the difference.
  :bundle-resources (("res/shaders/" . "shaders/")
                     ("res/images/" . "images/")
                     ("res/fonts/" . "fonts/"))
  ;; libcathode.dylib is dlopen'd by CFFI rather than linked, so it is named
  ;; here to be certain it lands in Contents/Frameworks with its own
  ;; dependencies rewritten to @loader_path.
  :bundle-foreign-libraries ("vendor/lib/libcathode.dylib")
  ;; Read from the environment, defaulting to AD HOC.
  ;;
  ;; Hardcoding a Developer ID here would break CI and anyone else who cloned
  ;; this: codesign answers "no identity found" for a certificate that is not in
  ;; the keychain, and there is no reason a BUILD should require one.  Ad hoc
  ;; runs locally and cannot be notarised, which is the right default;
  ;; notarising is the deliberate act that supplies the identity.
  ;;
  ;;   make app SIGN_IDENTITY="Developer ID Application: You (TEAMID)"
  ;;
  ;; #. rather than a call: ASDF does not evaluate a defsystem initarg, so the
  ;; value is computed when the file is READ.
  :code-signing-identity #.(let ((identity (uiop:getenv "CRT_SIGN_IDENTITY")))
                             ;; An EMPTY value counts as absent.  GETENV answers
                             ;; "" for a variable that is set and empty, which
                             ;; make does whenever SIGN_IDENTITY is unset, and
                             ;; "" is not NIL -- so an OR here would hand
                             ;; codesign an empty identity rather than falling
                             ;; back to ad hoc.
                             (if (and identity (plusp (length identity)))
                                 identity
                                 "-"))
  :hardened-runtime t
  :bundle-output-directory "build/"
  :components ((:module "src"
                :components ((:file "main")))))
