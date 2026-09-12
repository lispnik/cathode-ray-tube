/* crt_shim.c -- see crt_shim.h for why this file exists. */

#include "crt_shim.h"

#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>

/* The box we hand libvterm as its USER pointer.  The Lisp-side datum travels
 * inside it, so the trampolines can reach both the function table and whatever
 * Lisp wanted to identify this terminal by. */
struct crt_screen_context {
  crt_screen_callbacks cbs;
  void *user;
};

/* --- trampolines ----------------------------------------------------------
 *
 * Each unpacks the structures libvterm passes by value and calls the flat
 * Lisp function.  A null member means "not handled", which is what libvterm
 * expects a 0 return to mean, so an incomplete callback table is legal.
 */

static int tramp_damage(VTermRect rect, void *user)
{
  crt_screen_context *ctx = (crt_screen_context *)user;
  if (!ctx->cbs.damage) return 0;
  return ctx->cbs.damage(rect.start_row, rect.end_row,
                         rect.start_col, rect.end_col, ctx->user);
}

static int tramp_moverect(VTermRect dest, VTermRect src, void *user)
{
  crt_screen_context *ctx = (crt_screen_context *)user;
  if (!ctx->cbs.moverect) return 0;
  return ctx->cbs.moverect(dest.start_row, dest.end_row,
                           dest.start_col, dest.end_col,
                           src.start_row, src.end_row,
                           src.start_col, src.end_col, ctx->user);
}

static int tramp_movecursor(VTermPos pos, VTermPos oldpos, int visible,
                            void *user)
{
  crt_screen_context *ctx = (crt_screen_context *)user;
  if (!ctx->cbs.movecursor) return 0;
  return ctx->cbs.movecursor(pos.row, pos.col, oldpos.row, oldpos.col,
                             visible, ctx->user);
}

static int tramp_settermprop(VTermProp prop, VTermValue *val, void *user)
{
  crt_screen_context *ctx = (crt_screen_context *)user;
  if (!ctx->cbs.settermprop) return 0;
  return ctx->cbs.settermprop((int)prop, val, ctx->user);
}

static int tramp_bell(void *user)
{
  crt_screen_context *ctx = (crt_screen_context *)user;
  if (!ctx->cbs.bell) return 0;
  return ctx->cbs.bell(ctx->user);
}

static int tramp_resize(int rows, int cols, void *user)
{
  crt_screen_context *ctx = (crt_screen_context *)user;
  if (!ctx->cbs.resize) return 0;
  return ctx->cbs.resize(rows, cols, ctx->user);
}

static int tramp_sb_pushline(int cols, const VTermScreenCell *cells, void *user)
{
  crt_screen_context *ctx = (crt_screen_context *)user;
  if (!ctx->cbs.sb_pushline) return 0;
  return ctx->cbs.sb_pushline(cols, cells, ctx->user);
}

static int tramp_sb_popline(int cols, VTermScreenCell *cells, void *user)
{
  crt_screen_context *ctx = (crt_screen_context *)user;
  if (!ctx->cbs.sb_popline) return 0;
  return ctx->cbs.sb_popline(cols, cells, ctx->user);
}

static int tramp_sb_clear(void *user)
{
  crt_screen_context *ctx = (crt_screen_context *)user;
  if (!ctx->cbs.sb_clear) return 0;
  return ctx->cbs.sb_clear(ctx->user);
}

/* Static, so its address is stable for the life of the process.  libvterm
 * keeps the pointer we hand it rather than copying the table. */
static const VTermScreenCallbacks crt_tramp_table = {
  .damage      = tramp_damage,
  .moverect    = tramp_moverect,
  .movecursor  = tramp_movecursor,
  .settermprop = tramp_settermprop,
  .bell        = tramp_bell,
  .resize      = tramp_resize,
  .sb_pushline = tramp_sb_pushline,
  .sb_popline  = tramp_sb_popline,
  .sb_clear    = tramp_sb_clear,
};

crt_screen_context *crt_screen_set_callbacks(VTermScreen *screen,
                                             const crt_screen_callbacks *cbs,
                                             void *user)
{
  crt_screen_context *ctx;

  if (!screen || !cbs) return NULL;

  ctx = (crt_screen_context *)calloc(1, sizeof(*ctx));
  if (!ctx) return NULL;

  ctx->cbs  = *cbs;   /* by value: the caller's table may be stack-allocated */
  ctx->user = user;

  vterm_screen_set_callbacks(screen, &crt_tramp_table, ctx);
  return ctx;
}

void crt_screen_context_free(crt_screen_context *ctx)
{
  free(ctx);
}

/* --- by-value calls, flattened -------------------------------------------- */

int crt_screen_get_cell(const VTermScreen *screen, int row, int col,
                        VTermScreenCell *cell)
{
  VTermPos pos;
  pos.row = row;
  pos.col = col;
  return vterm_screen_get_cell(screen, pos, cell);
}

int crt_screen_get_row(const VTermScreen *screen, int row, int cols,
                       VTermScreenCell *cells)
{
  VTermPos pos;
  int i;

  pos.row = row;
  for (i = 0; i < cols; i++) {
    pos.col = i;
    if (!vterm_screen_get_cell(screen, pos, &cells[i]))
      return i;
  }
  return cols;
}

int crt_screen_is_eol(const VTermScreen *screen, int row, int col)
{
  VTermPos pos;
  pos.row = row;
  pos.col = col;
  return vterm_screen_is_eol(screen, pos);
}

size_t crt_screen_get_text(const VTermScreen *screen, char *str, size_t len,
                           int start_row, int end_row,
                           int start_col, int end_col)
{
  VTermRect rect;
  rect.start_row = start_row;
  rect.end_row   = end_row;
  rect.start_col = start_col;
  rect.end_col   = end_col;
  return vterm_screen_get_text(screen, str, len, rect);
}

/* --- the pty window size -------------------------------------------------- */

int crt_set_winsize(int fd, int rows, int cols)
{
  struct winsize ws;

  memset(&ws, 0, sizeof(ws));
  ws.ws_row = (unsigned short)rows;
  ws.ws_col = (unsigned short)cols;
  return ioctl(fd, TIOCSWINSZ, &ws);
}

int crt_get_winsize(int fd, int *rows, int *cols)
{
  struct winsize ws;
  int rc;

  memset(&ws, 0, sizeof(ws));
  rc = ioctl(fd, TIOCGWINSZ, &ws);
  if (rc == 0) {
    if (rows) *rows = ws.ws_row;
    if (cols) *cols = ws.ws_col;
  }
  return rc;
}
