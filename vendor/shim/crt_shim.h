/* crt_shim.h -- the C adaptation layer between libvterm and Common Lisp.
 *
 * This file exists for exactly one reason: CFFI, without libffi, cannot pass
 * or return a structure BY VALUE.  `cffi:foreign-funcall' cannot call such a
 * function and `cffi:defcallback' signals CASE-FAILURE when asked to receive
 * one.  libvterm's API crosses that line in seven places, and Apple's arm64
 * variadic ABI breaks an eighth.  Rather than drag in cffi-libffi -- which
 * needs cffi-grovel, a C toolchain at Lisp build time, and risks linking
 * Homebrew's libffi into a bundle that must run on a Mac without Homebrew --
 * the few crossings are flattened here, in the C we are already compiling.
 *
 * Nothing in this file adds behaviour.  Every function is a transliteration of
 * a libvterm call with its structures spelled out as scalars, and every
 * callback is a trampoline that does the same in the other direction.
 *
 * Reading a structure THROUGH A POINTER is fine in CFFI, so VTermScreenCell is
 * passed through unchanged and decoded on the Lisp side with `defcstruct'.
 * Only by-value crossings appear here.
 */

#ifndef CRT_SHIM_H
#define CRT_SHIM_H

#include <stddef.h>
#include "vterm.h"

#ifdef __cplusplus
extern "C" {
#endif

/* --- the screen callbacks, with VTermRect and VTermPos spelled out ---------
 *
 * libvterm's own VTermScreenCallbacks passes VTermRect (four ints, 16 bytes)
 * and VTermPos (two ints, 8 bytes) by value.  These take the same information
 * as loose ints.  `settermprop' keeps its VTermValue POINTER, which CFFI reads
 * without help; the remaining members are already scalar.
 *
 * Return values follow libvterm: non-zero means "handled".
 */
typedef struct {
  int (*damage)(int start_row, int end_row, int start_col, int end_col,
                void *user);
  int (*moverect)(int dest_start_row, int dest_end_row,
                  int dest_start_col, int dest_end_col,
                  int src_start_row, int src_end_row,
                  int src_start_col, int src_end_col,
                  void *user);
  int (*movecursor)(int row, int col, int old_row, int old_col, int visible,
                    void *user);
  int (*settermprop)(int prop, VTermValue *val, void *user);
  int (*bell)(void *user);
  int (*resize)(int rows, int cols, void *user);
  int (*sb_pushline)(int cols, const VTermScreenCell *cells, void *user);
  int (*sb_popline)(int cols, VTermScreenCell *cells, void *user);
  int (*sb_clear)(void *user);
} crt_screen_callbacks;

/* An opaque box holding one screen's callbacks and the Lisp-side USER datum.
 * Install with crt_screen_set_callbacks, release with crt_screen_context_free
 * AFTER vterm_free -- libvterm may call back during teardown. */
typedef struct crt_screen_context crt_screen_context;

crt_screen_context *crt_screen_set_callbacks(VTermScreen *screen,
                                             const crt_screen_callbacks *cbs,
                                             void *user);
void crt_screen_context_free(crt_screen_context *ctx);

/* --- by-value calls, flattened -------------------------------------------- */

/* vterm_screen_get_cell with POS spelled out. */
int crt_screen_get_cell(const VTermScreen *screen, int row, int col,
                        VTermScreenCell *cell);

/* One FFI call per ROW instead of per cell.  Fills COLS cells starting at
 * column 0 of ROW.  Returns the number actually written, which is COLS unless
 * libvterm refused a position. */
int crt_screen_get_row(const VTermScreen *screen, int row, int cols,
                       VTermScreenCell *cells);

int crt_screen_is_eol(const VTermScreen *screen, int row, int col);

/* vterm_screen_get_text with RECT spelled out.  Returns the byte count, and
 * writes nothing when STR is NULL -- the usual two-call measure-then-fill. */
size_t crt_screen_get_text(const VTermScreen *screen, char *str, size_t len,
                           int start_row, int end_row,
                           int start_col, int end_col);

/* --- the pty window size -------------------------------------------------- */

/* ioctl(3) is VARIADIC, and on Apple arm64 a variadic argument is passed on
 * the stack while a fixed-arity call passes it in a register.  A plain
 * `cffi:foreign-funcall' for TIOCSWINSZ therefore hands the kernel a register
 * it does not read.  Measured, both calls against the same fresh pty:
 *
 *   plain foreign-funcall   ioctl returned  -1  -> child's `stty size': "0 0"
 *   crt_set_winsize         ioctl returned   0  -> child's `stty size': "30 100"
 *
 * So it fails loudly rather than silently -- but it fails, and the child never
 * learns its size.  Declared non-variadic here, it simply works.
 *
 * Returns the ioctl result: 0 on success, -1 with errno set. */
int crt_set_winsize(int fd, int rows, int cols);

/* The other direction, for completeness -- the kernel's idea of the size. */
int crt_get_winsize(int fd, int *rows, int *cols);

#ifdef __cplusplus
}
#endif
#endif /* CRT_SHIM_H */
