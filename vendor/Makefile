# vendor/Makefile -- libvterm, as a dylib.
#
# ONE library rather than two, and built here rather than taken from Homebrew,
# for the same reason: this has to run on a Mac that has never had Homebrew.
# `otool -L' on the result must show /usr/lib/libSystem.B.dylib and nothing
# else; the `check-links' target in the top-level Makefile enforces that on the
# built bundle, and `make check' below enforces it here, where the fix is
# cheaper.
#
# There is no configure, no libtool, and no pkg-config: libvterm is nine C
# files with no dependencies.  A C compiler is needed to compile C, but it is
# needed only HERE -- never at Lisp build time, and never by anyone running
# the application.
#
# There used to be a shim/ beside it, flattening the structures libvterm passes
# BY VALUE because CFFI cannot express one.  It is gone: sb-alien can, and so can
# ECL's dynamic FFI on lispnik/ecl, so the crossings are made from Lisp now --
# see src/vt/vterm-abi.lisp and src/pty/ioctl.lisp.  What is left here is
# upstream libvterm, unmodified.

NAME    = cathode
DYLIB   = lib/lib$(NAME).dylib
VTERM   = libvterm

CC     ?= cc
# -fvisibility=hidden would hide libvterm's own symbols, which Lisp calls
# directly; everything in this library is public API by construction.
CFLAGS ?= -O2 -g -fPIC -Wall
CFLAGS += -I$(VTERM)/include -I$(VTERM)/src -DHAVE_STDBOOL_H
# Deployment target pinned so the dylib loads on the oldest OS the app claims.
# CADisplayLink puts our floor at macOS 14; keep the two in step.
CFLAGS += -mmacosx-version-min=14.0
# A BARE install_name, not @rpath.  Nothing links this library -- CFFI dlopen's
# it by name from a directory it was told about -- so @rpath would be a promise
# no binary keeps, and asdf-macos-app says so: "references @rpath/libcathode.dylib
# but no LC_RPATH resolves it".  A bare name is what a dlopen-only library
# should carry.
LDFLAGS += -dynamiclib -install_name lib$(NAME).dylib \
           -mmacosx-version-min=14.0 -compatibility_version 1.0 -current_version 1.0

SOURCES = $(wildcard $(VTERM)/src/*.c)
OBJECTS = $(SOURCES:.c=.o)
HEADERS = $(VTERM)/include/vterm.h $(VTERM)/include/vterm_keycodes.h

.PHONY: all clean check install-headers

all: $(DYLIB) install-headers

$(DYLIB): $(OBJECTS)
	@mkdir -p lib
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)
	@echo "built $@"

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Headers land in one place so the Lisp side has a single -I to think about
# and `make check' has somewhere to point.
install-headers: $(HEADERS)
	@mkdir -p include
	@cp $(HEADERS) include/
	@echo "installed headers into include/"

# The guard that matters, run where it is cheap to fix.  A dylib that picks up
# a Homebrew dependency here notarises perfectly well and then dies with a dyld
# error on someone else's Mac.
check: $(DYLIB)
	@echo "otool -L $(DYLIB):"
	@otool -L $(DYLIB) | tail -n +2
	@# The library's OWN install name is the first entry otool -L prints for a
	@# dylib, and it is not a dependency.  It is excluded by asking otool -D
	@# what it is rather than by matching a literal, which is what broke when
	@# the install name changed from @rpath/libcathode.dylib to a bare name:
	@# the filter still named the old form, so the library reported itself as
	@# an external dependency and the guard failed on a correct build.
	@install=$$(otool -D $(DYLIB) | tail -1); \
	 otool -L $(DYLIB) | tail -n +2 | awk '{print $$1}' \
	  | grep -v -F -x "$$install" \
	  | grep -v '^/usr/lib/' | grep -v '^/System/' \
	  | grep . && { echo "error: links something outside /usr/lib and /System" >&2; exit 1; } \
	  || echo "ok: nothing outside /usr/lib and /System"
	@nm -gU $(DYLIB) | grep -c ' T _' | xargs -I{} echo "{} exported symbols"

clean:
	rm -f $(OBJECTS) $(DYLIB)
	rm -rf include
