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

# The dylib is dlopen'd by name, so it has to be findable.  In a bundle
# asdf-macos-app puts it in Contents/Frameworks and pushes that directory onto
# cffi:*foreign-library-directories*; from a checkout, this does the same.
LOADPATH = (push (truename "$(VENDOR)/lib/") cffi:*foreign-library-directories*)

RUNLISP = $(LISP) --non-interactive --no-userinit --no-sysinit \
	  --eval '(require :asdf)' \
	  --eval '$(QUIET)' \
	  --eval '$(REGISTRY)'

.PHONY: all deps vendor probe test repl clean distclean

all: deps

# --- dependencies ------------------------------------------------------------

deps: vendor
	ocicl install

vendor: $(DYLIB)

$(DYLIB): $(wildcard $(VENDOR)/libvterm/src/*.c) $(VENDOR)/shim/crt_shim.c \
          $(VENDOR)/shim/crt_shim.h
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
	  --eval '$(LOADPATH)' \
	  --load tools/probe.lisp \
	  --eval '(uiop:quit (if (crt-probe:run) 0 1))'

# --- the suite ---------------------------------------------------------------

test: $(DYLIB)
	$(RUNLISP) \
	  --eval '$(LOADPATH)' \
	  --eval '(asdf:load-system :cathode-ray-tube/tests)' \
	  --eval '(uiop:quit (if (cathode-ray-tube/tests:run-tests) 0 1))'

repl: $(DYLIB)
	$(LISP) --no-userinit --no-sysinit \
	  --eval '(require :asdf)' \
	  --eval '$(QUIET)' \
	  --eval '$(REGISTRY)' \
	  --eval '$(LOADPATH)' \
	  --eval '(asdf:load-system :cathode-ray-tube)'

# --- housekeeping ------------------------------------------------------------

clean:
	$(MAKE) -C $(VENDOR) clean
	rm -rf bin build dist
	rm -rf $(HOME)/.cache/common-lisp/*/$(CURDIR)

distclean: clean
	rm -rf ocicl
