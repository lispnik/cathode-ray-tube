# Makefile for cathode-ray-tube.
#
#   make deps       vendor the C library and restore the Lisp dependencies
#   make probe      M0: prove the hard things work on this machine
#   make test       the FiveAM suite
#   make repl       an SBCL with the system loaded
#   make clean      remove build products and this tree's ASDF cache
#
# Adapted from utc-status-app's Makefile, which ships a signed, notarised
# bundle; the app, notarise and dmg targets arrive with M5.

LISP    ?= sbcl
VENDOR   = vendor
DYLIB    = $(VENDOR)/lib/libcathode.dylib

# :IGNORE-INHERITED-CONFIGURATION so a missing dependency fails HERE rather than
# resolving to whatever happens to be in the developer's ~/.sbclrc -- the same
# reason objc's own Makefile does it.  ocicl vendors into ./ocicl, which the
# tree scan picks up.
REGISTRY = (asdf:initialize-source-registry \
              (list :source-registry \
                    (list :tree (truename "./")) \
                    :ignore-inherited-configuration))

# objc compiles one sb-alien trampoline per distinct method signature and each
# prints a compiler note.  They are a property of the bridge, not of this code.
QUIET = (proclaim (quote (sb-ext:muffle-conditions sb-ext:compiler-note style-warning)))

RUNLISP = $(LISP) --non-interactive --no-userinit --no-sysinit \
	  --eval '(require :asdf)' \
	  --eval '$(QUIET)' \
	  --eval '$(REGISTRY)'

.PHONY: all deps vendor check-vendor probe constants check-metal-constants \
        test test-no-bundle check-undefined run repl app icon check-app check-dist run-app \
        settings-shot \
        install-app \
        notarize dmg notarize-dmg release gallery clean distclean

all: deps

# --- dependencies ------------------------------------------------------------

deps: vendor
	ocicl install

vendor: $(DYLIB)

$(DYLIB): $(wildcard $(VENDOR)/libvterm/src/*.c)
	$(MAKE) -C $(VENDOR)

# The guard that matters, run where it is cheap to fix: a dylib that picks up a
# Homebrew dependency notarises perfectly well and then dies with a dyld error
# on a Mac that has never had Homebrew.
check-vendor: $(DYLIB)
	$(MAKE) -C $(VENDOR) check

# --- M0 ----------------------------------------------------------------------

# Not a test: a REPORT.  It answers "can this machine do the things the design
# assumes" by measuring, and it is worth re-running after any OS or Xcode
# update, because every answer in it is about the platform rather than about us.
probe: $(DYLIB)
	$(RUNLISP) \
	  --eval '(asdf:load-system :objc)' \
	  --eval '(asdf:load-system :babel)' \
	  --eval '(push (truename "$(VENDOR)/lib/") cffi:*foreign-library-directories*)' \
	  --load tools/probe.lisp \
	  --eval '(uiop:quit (if (crt-probe:run) 0 1))'

# --- generated sources -------------------------------------------------------

# Metal's enumerations, read out of the SDK rather than remembered.  `objc' has
# no constant tables and will not grow any -- it sends messages, and a message
# does not know what 70 means -- so the numbers have to come from somewhere.  A
# typo in a hand-copied table does not raise an error; it gets you a pipeline
# that builds, a texture that allocates, and a black window.
constants:
	tools/metal-constants.sh > /tmp/crt-constants.sexp
	@echo "regenerate src/metal/constants.lisp from /tmp/crt-constants.sexp"

# Asserted by the suite too; this is the one-line form for a workflow.
check-metal-constants:
	@$(RUNLISP) \
	  --eval '(asdf:load-system :cathode-ray-tube/tests)' \
	  --eval '(uiop:quit (if (fiveam:run! (quote cathode-ray-tube/tests::metal-constants-match-the-sdk)) 0 1))'

# --- the suite ---------------------------------------------------------------

# A FORCED compile of everything, failing on any undefined function.
#
# This catches a class the test suite structurally cannot: a call to a function
# that does not exist is a compile-time WARNING and a run-time error, so it stays
# invisible until something executes that exact line.  It happened -- a function
# referenced from CRT.UI that lives in the CATHODE-RAY-TUBE package, where CRT.UI
# does not look.  Everything compiled, the suite was green, and the code was one
# call away from an error.
#
# --force, because ASDF will not recompile a file whose fasl is newer, and a
# warning printed during an earlier build is a warning nobody sees again.
#
# NOT $(RUNLISP): that muffles STYLE-WARNING, and `undefined function' is one.
# The first version of this target used it, reported ok, and went on reporting ok
# with a deliberately broken call compiled in -- which is why every guard here is
# broken on purpose once before it is trusted.
check-undefined:
	@$(LISP) --non-interactive --no-userinit --no-sysinit \
	  --eval '(require :asdf)' \
	  --eval '(proclaim (quote (sb-ext:muffle-conditions sb-ext:compiler-note)))' \
	  --eval '$(REGISTRY)' \
	  --eval '(asdf:load-system :cathode-ray-tube :force t)' 2>&1 \
	  | grep -iE 'undefined (function|variable)' \
	  && { echo 'error: the build references something that does not exist' >&2; \
	       exit 1; } || echo 'ok: no undefined functions or variables'

test: $(DYLIB)
	$(RUNLISP) \
	  --eval '(asdf:load-system :cathode-ray-tube/tests)' \
	  --eval '(uiop:quit (if (cathode-ray-tube/tests:run-tests) 0 1))'

# Opens a window and blocks.  AppKit owns the thread once this starts.
run: $(DYLIB)
	$(RUNLISP) \
	  --eval '(asdf:load-system :cathode-ray-tube)' \
	  --eval '(cathode-ray-tube:main)'

repl: $(DYLIB)
	$(LISP) --no-userinit --no-sysinit \
	  --eval '(require :asdf)' \
	  --eval '$(QUIET)' \
	  --eval '$(REGISTRY)' \
	  --eval '(asdf:load-system :cathode-ray-tube)'

# --- the bundle --------------------------------------------------------------
#
# "cathode-ray-tube.app" has no space in it, unlike utc-status-app's, so it can
# be an ordinary Make target -- but it is still tracked by a stamp so that
# `make app' does not redump a 50MB image every time.

APP        = build/cathode-ray-tube.app
APP_STAMP  = build/.app-stamp
DIST       = dist
VERSION    = $(shell sed -n 's/.*:version "\(.*\)".*/\1/p' cathode-ray-tube.asd | head -1)
ARCH       = $(shell uname -m)
DMG        = $(DIST)/cathode-ray-tube-$(VERSION)-$(ARCH).dmg

# The codesigning identity.  Empty means AD HOC, which builds anywhere and
# cannot be notarised; a Developer ID makes the bundle distributable.  It
# reaches the .asd through the environment -- see cathode-ray-tube-bundle.asd.
SIGN_IDENTITY ?=
export CRT_SIGN_IDENTITY = $(SIGN_IDENTITY)

# The notarytool keychain profile, stored once with
#   xcrun notarytool store-credentials $(NOTARY_PROFILE) \
#     --apple-id <you> --team-id <your team>
# which prompts for an APP-SPECIFIC password from appleid.apple.com -- not your
# Apple ID password.
NOTARY_PROFILE ?= cathode-ray-tube-app

app: $(APP_STAMP)

# WHO SIGNED IT is part of what the bundle is, so changing SIGN_IDENTITY has to
# rebuild.  It did not: the stamp depends on the sources, the identity is not a
# source, and `make app SIGN_IDENTITY=...' over a current tree did nothing at
# all.  You then got an ad-hoc signed bundle with no indication anything had
# been ignored -- and found out from `make notarize', which refuses it, or from
# Apple, which is slower.
#
# Recorded in a file, rewritten only when it actually changes so an unchanged
# identity does not force a rebuild every time.
SIGN_RECORD = build/.sign-identity

.PHONY: $(SIGN_RECORD)
$(SIGN_RECORD):
	@mkdir -p build
	@printf '%s' '$(SIGN_IDENTITY)' | cmp -s - $@ 2>/dev/null \
	  || printf '%s' '$(SIGN_IDENTITY)' > $@

$(APP_STAMP): cathode-ray-tube-bundle.asd $(DYLIB) res/icon.png $(SIGN_RECORD) \
              $(shell find src res/shaders -type f)
	$(RUNLISP) --eval '(asdf:make :cathode-ray-tube-bundle)'
	@mkdir -p build
	@touch $(APP_STAMP)
	@echo "built $(APP)$(if $(SIGN_IDENTITY), signed by $(SIGN_IDENTITY), (ad hoc))"

# The settings window, every tab, into docs/settings/.  Needs a window server as
# well as a GPU, because the controls are AppKit's rather than ours.
settings-shot: $(DYLIB)
	$(RUNLISP) \
	  --eval '(asdf:load-system :cathode-ray-tube)' \
	  --load tools/settings-shot.lisp \
	  --eval '(crt-settings-shot:render-all)'

# Every profile, rendered headlessly into docs/gallery/.  Needs a GPU; needs no
# window.  This is how the port is judged.
gallery: $(DYLIB)
	$(RUNLISP) \
	  --eval '(asdf:load-system :cathode-ray-tube)' \
	  --load tools/snapshot-png.lisp \
	  --load tools/gallery.lisp \
	  --eval '(crt-gallery:render-all)'

icon: res/icon.png

res/icon.png: tools/icon.lisp
	$(RUNLISP) \
	  --eval '(asdf:load-system :cathode-ray-tube)' \
	  --load tools/icon.lisp \
	  --eval '(crt-icon:render-icon "res/icon.png")'
	@echo "drew res/icon.png"

# Everything the bundle loads must be INSIDE it.
#
# utc-status-app's version of this guard runs otool on Contents/MacOS/<exe> only,
# and copied here it would give a FALSE PASS: libcathode.dylib is dlopen'd by
# CFFI and is never a link-time dependency of that binary, so it would never
# appear.  This walks Contents/Frameworks as well, and accepts @loader_path and
# @rpath only when the file they name is actually in the bundle.
#
# The property being defended is the one that matters for distribution: a
# runtime that links Homebrew's libzstd notarises PERFECTLY WELL and then dies
# with a dyld error on a Mac that has never had Homebrew.  Apple checks the
# signature, not whether your dylibs exist on someone else's disk.
define check-links
	@echo "checking what $(1) loads"
	@fail=0; \
	for bin in "$(1)/Contents/MacOS/"* "$(1)/Contents/Frameworks/"*.dylib; do \
	  [ -f "$$bin" ] || continue; \
	  self=$$(otool -D "$$bin" 2>/dev/null | tail -1); \
	  otool -L "$$bin" | tail -n +2 | awk '{print $$1}' \
	    | grep -v -F -x "$${self:-/dev/null}" | while read dep; do \
	    case "$$dep" in \
	      /usr/lib/*|/System/*) ;; \
	      @loader_path/*|@rpath/*|@executable_path/*) \
	        name=$${dep##*/}; \
	        [ -f "$(1)/Contents/Frameworks/$$name" ] || { \
	          echo "  error: $$bin needs $$dep, which is not in Frameworks" >&2; \
	          exit 1; } ;; \
	      *) echo "  error: $$bin links $$dep, outside the bundle" >&2; exit 1 ;; \
	    esac; \
	  done || fail=1; \
	done; \
	[ $$fail -eq 0 ] && echo "  ok: nothing outside /usr/lib and /System"
endef

# STRUCTURE, not distributability.  The linkage guard is reported here but does
# not fail the target, because a bundle built against Homebrew's SBCL is
# perfectly good for testing and is not shippable -- and CI builds with Homebrew
# SBCL on purpose, since building one from source for every push would cost more
# than it tells anyone.  `make notarize' is where linkage is a hard failure, and
# the release workflow builds an SBCL that passes it.
check-app: app
	@$(MAKE) --no-print-directory check-dist || \
	  echo "  note: not distributable as built; make notarize is where that is enforced"
	@echo "Info.plist:"
	@plutil -p "$(APP)/Contents/Info.plist" | grep -E "CFBundleIdentifier|NSPrincipalClass|LSMinimumSystemVersion|CFBundleName"
	@test -d "$(APP)/Contents/Resources/fonts" || { echo "error: fonts are missing" >&2; exit 1; }
	@test -f "$(APP)/Contents/Resources/shaders/crt.metal" || { echo "error: the shader is missing" >&2; exit 1; }
	@test -f "$(APP)/Contents/Resources/images/allNoise512.png" || { echo "error: the noise texture is missing" >&2; exit 1; }
	@echo "  ok: resources are present"

run-app: app
	"$(APP)/Contents/MacOS/cathode-ray-tube"

install-app: app
	@mkdir -p $(HOME)/Applications
	rm -rf "$(HOME)/Applications/cathode-ray-tube.app"
	cp -R "$(APP)" "$(HOME)/Applications/"
	@echo "installed $(HOME)/Applications/cathode-ray-tube.app"

# --- distribution ------------------------------------------------------------

# Submit to Apple's notary service and staple the ticket into the bundle.
#
# The two guards are here because both failures are slow and neither is obvious.
# An AD HOC signature is refused by Apple, but only after the upload; and a
# bundle that loads something from outside itself notarises and then fails to
# launch elsewhere.
# The distribution guard on its own, so CI can report it without failing and
# `notarize' can insist on it.
check-dist: app
	$(call check-links,$(APP))

notarize: app
	@codesign -dvv "$(APP)" 2>&1 | grep -q adhoc && { \
	  echo "error: $(APP) is signed ad hoc, and Apple will refuse it." >&2; \
	  echo "  build with SIGN_IDENTITY=\"Developer ID Application: You (TEAMID)\"" >&2; \
	  exit 1; } || true
	$(call check-links,$(APP))
	$(RUNLISP) \
	  --eval '(asdf:load-system :asdf-macos-app)' \
	  --eval '(macos-app:notarize "$(APP)" :keychain-profile "$(NOTARY_PROFILE)")'
	@echo
	@spctl -a -vvv -t install "$(APP)"
	@xcrun stapler validate "$(APP)"

# A disk image, which is what people expect to download.
dmg: $(DMG)

$(DMG): $(APP_STAMP)
	@mkdir -p $(DIST)
	rm -f "$(DMG)"
	@# A staging directory with the app and a link to /Applications, which is
	@# the convention every Mac user already knows how to act on.
	rm -rf "$(DIST)/stage"
	mkdir -p "$(DIST)/stage"
	cp -R "$(APP)" "$(DIST)/stage/"
	ln -s /Applications "$(DIST)/stage/Applications"
	hdiutil create -volname "cathode-ray-tube" -srcfolder "$(DIST)/stage" \
	  -ov -format UDZO "$(DMG)"
	rm -rf "$(DIST)/stage"
	@if [ -n "$(SIGN_IDENTITY)" ]; then \
	  echo "signing the disk image"; \
	  codesign --force --sign "$(SIGN_IDENTITY)" --timestamp "$(DMG)"; \
	else \
	  echo "note: unsigned disk image (no SIGN_IDENTITY)"; \
	fi
	@echo "built $(DMG)"

# Notarise the disk image IN ITS OWN RIGHT and staple the ticket to it.
#
# Stapling only the app leaves the download itself unrecognised, so the first
# thing a user touches -- the disk image -- is the thing Gatekeeper complains
# about.  Notarising the app first is still required: the notary service checks
# what is inside.
notarize-dmg: $(DMG)
	xcrun notarytool submit "$(DMG)" --keychain-profile "$(NOTARY_PROFILE)" --wait
	xcrun stapler staple "$(DMG)"
	@echo
	@spctl -a -vvv -t open --context context:primary-signature "$(DMG)"

release: notarize notarize-dmg
	@echo "$(DMG) is signed, notarised and stapled."

# The library must load WITHOUT asdf-macos-app.
#
# :DEFSYSTEM-DEPENDS-ON resolves when a .asd is READ, so a bundle declaration in
# the main file would make asdf-macos-app a hard requirement for merely loading
# the library -- and nobody who has both installed would ever notice.  This is
# the only thing that catches it.
test-no-bundle:
	@$(LISP) --non-interactive --no-userinit --no-sysinit \
	  --eval '(require :asdf)' \
	  --eval '$(QUIET)' \
	  --eval '(asdf:initialize-source-registry (list :source-registry (list :directory (truename "./")) (list :tree (truename "./ocicl/")) (list :tree (truename "./src/")) :ignore-inherited-configuration))' \
	  --eval '(asdf:load-system :cathode-ray-tube)' \
	  --eval '(format t "~&ok: the library loads without asdf-macos-app~%")'

# --- housekeeping ------------------------------------------------------------

clean:
	$(MAKE) -C $(VENDOR) clean
	rm -rf bin build dist
	rm -rf $(HOME)/.cache/common-lisp/*/$(CURDIR)

distclean: clean
	rm -rf ocicl
