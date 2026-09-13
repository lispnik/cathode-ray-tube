#!/bin/sh
# tools/errno-constants.sh -- read the errno values out of the SDK headers and
# print them as a Lisp form.
#
# The same discipline as tools/metal-constants.sh, for the same reason: EINTR is
# 4 and EAGAIN is 35 on Darwin and have been for decades, and a number typed
# from memory is still a number nobody can check.  This reads them.
#
#   tools/errno-constants.sh > /tmp/errno.sexp   # regenerate
#   make check-errno-constants                   # assert nothing moved
set -eu

SDK="$(xcrun --show-sdk-path)/usr/include/sys"
[ -f "$SDK/errno.h" ] || { echo "no errno.h under $SDK" >&2; exit 1; }

emit() {                       # emit <lisp-name> <C-name>
    value=$(grep -hoE "^#define[[:space:]]+$2[[:space:]]+[0-9]+" "$SDK/errno.h" \
            | head -1 | sed -E 's/.*[[:space:]]//')
    [ -n "$value" ] || { echo "could not find $2 in $SDK/errno.h" >&2; exit 1; }
    printf '  (%s . %s)\n' "$1" "$value"
}

echo "("
emit eintr  EINTR
emit eagain EAGAIN
emit eio    EIO
echo ")"
