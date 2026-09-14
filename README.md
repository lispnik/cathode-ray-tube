# cathode-ray-tube

A native macOS terminal emulator that looks like a cathode-ray tube, written in
Common Lisp.

It is a port of [cool-retro-term](https://github.com/Swordfish90/cool-retro-term)
by Filippo Scognamiglio: the same phosphor burn-in, bloom, scanlines, barrel
distortion, chroma tint, jitter and horizontal-sync tearing, and the same
fourteen profiles — but with the GLSL translated to Metal, the Qt6/QML
application replaced by Cocoa driven from Lisp through the
[`objc`](https://github.com/lispnik/objc) bindings, and the terminal emulation
supplied by a vendored [libvterm](https://www.leonerd.org.uk/code/libvterm/).

**Status: it looks like a cathode-ray tube.** `make run` opens an 80×25 window,
runs your shell, and draws it through the full effect chain — phosphor burn-in,
bloom, scanlines, barrel distortion, chroma tint, jitter, horizontal-sync
tearing and a procedural bezel — under any of the fourteen ported profiles.

![Default Amber](docs/gallery/default-amber.png)

All fourteen, each in the face its profile names — `make gallery` regenerates
them:

| | | |
|---|---|---|
| ![Default Amber](docs/gallery/default-amber.png) Default Amber | ![Monochrome Green](docs/gallery/monochrome-green.png) Monochrome Green | ![Deep Blue](docs/gallery/deep-blue.png) Deep Blue |
| ![Commodore 64](docs/gallery/commodore-64.png) Commodore 64 | ![Commodore PET](docs/gallery/commodore-pet.png) Commodore PET | ![Apple](docs/gallery/apple.png) Apple ][ |
| ![Atari 400](docs/gallery/atari-400.png) Atari 400 | ![IBM VGA 8x16](docs/gallery/ibm-vga-8x16.png) IBM VGA 8x16 | ![IBM 3278 Reborn](docs/gallery/ibm-3278-reborn.png) IBM 3278 Reborn |
| ![Neon Cyan](docs/gallery/neon-cyan.png) Neon Cyan | ![Ghost Terminal](docs/gallery/ghost-terminal.png) Ghost Terminal | ![Plasma](docs/gallery/plasma.png) Plasma |
| ![Boring](docs/gallery/boring.png) Boring | ![E-Ink](docs/gallery/e-ink.png) E-Ink | |

There is a settings window (Cmd-,) with the same four tabs upstream has, tabs
(Cmd-T, and the system's own tab bar), the twenty-four bundled faces and the
machine's own monospace families, custom profiles saved, imported and exported
as cool-retro-term's own JSON, and the size overlay while you resize.

`make test` is 1224 checks on SBCL and 892 on ECL — the lower half of the program
is free of Objective-C and is tested on both. `make app` builds a bundle and
`make release` a signed, notarised disk image.

### ECL

ECL support **requires** [lispnik/ecl](https://github.com/lispnik/ecl)'s
`develop`. Not a preference — stock ECL fails two tests outright, because
`src/pty/ioctl-ecl.lisp` makes a variadic call through the dynamic FFI and stock
cannot. It signals rather than misbehaving, which is the right way round.

The fork carries two fixes stock does not have. Measured on arm64:

| | fork | stock |
|---|---|---|
| a four-int struct **by value** into a libffi closure | receives `(3 7 11 19)` | *"does not denote an elementary foreign type"* |
| a variadic call through `si:call-cfun` | `snprintf` gives `"The integer 42"` | cannot resolve `snprintf` from `:DEFAULT` at all |

The second is the `dlsym(0, ...)` problem: on Darwin that is not the global
scope, so CFFI's `:default` module finds nothing — not even `strlen`. It is why
the [`objc`](https://github.com/lispnik/objc) bindings need the fork to run on
ECL at all.

Between them those two are the whole of what replacing the C shim needed, and
that replacement is now complete — see *The FFI boundary* below.

CI builds the fork from source, cached against `develop`'s SHA, and asserts it
really is the fork before running anything — so a silent fallback to Homebrew's
ECL fails at that step with a clear message instead of two puzzling errors
several minutes later.

Two things are deliberately not claimed as parity. **Bloom** is close rather than
identical: Qt's `FastBlur` is an undisclosed multi-pass box approximation with no
source in the tree to port, so it is tuned by eye. **Rasterization intensity**
differs where magnification is fractional, because the low-resolution faces here
are magnified by whole numbers. Everything else is meant to match, and the
arithmetic that decides it is asserted against a golden table taken from a
running cool-retro-term.

## Building

Needs a Mac with the Command Line Tools (for `cc`), SBCL, and
[ocicl](https://github.com/ocicl/ocicl).

```sh
make deps     # build vendor/lib/libcathode.dylib, restore the Lisp dependencies
make run      # open a terminal
make test     # 181 checks
make probe    # prove this machine can do what the design assumes
```

`make probe` is not a test but a report. It answers, by measuring:

- Does a Metal **render** pipeline work from Lisp? (`objc` ships tested *compute*
  examples and nothing else, so this was the project's biggest unknown.)
- Do `[[function_constant(n)]]` specialisations genuinely fold? That mechanism
  replaces cool-retro-term's 56 precompiled `.qsb` shader variants with one
  `.metal` source, so it had to be proven rather than assumed.
- Do the by-value structure crossings work — `MTLClearColor` (a 32-byte
  homogeneous float aggregate), `MTLViewport` and `MTLRegion` (48 bytes,
  indirect), and an `NSError**` out-param?
- Does libvterm drive correctly, including a `VTermRect` arriving in a Lisp
  callback **by value**? (Through C when the probe was written; through
  `sb-alien` now.)
- Does `TIOCSWINSZ` actually reach the child?

On an M3 it prints 29 checks and 0 failures. Re-run it after an OS or Xcode
update: every answer in it is about the platform, not about this code.

## The FFI boundary, and the C that used to be here

libvterm passes `VTermRect` (four ints) and `VTermPos` (two ints) **by value**,
in both directions — four calls and three callbacks — and `ioctl` is variadic.
CFFI can express none of that: `foreign-funcall` refuses the call and
`defcallback` signals `CASE-FAILURE`. So there was a ~200-line C shim,
`vendor/shim/`, flattening every crossing into loose integers.

**It is gone.** Both implementations can express those crossings; only CFFI
cannot.

| | SBCL | ECL (`lispnik/ecl`) |
|---|---|---|
| struct by value, outbound | `sb-alien` struct type | `si:call-cfun`, member list |
| struct by value, into a callback | `define-alien-callable` | `si::make-dynamic-callback` |
| variadic `ioctl` | `&optional` in the signature | trailing `:default n-fixed` |

`src/vt/vterm-abi.lisp` and `src/pty/ioctl.lisp` hold everything that does not
vary; `*-sbcl.lisp` and `*-ecl.lisp` hold what does — one small file each,
selected by the `.asd`, exactly as [`objc`](https://github.com/lispnik/objc)
splits `abi.lisp` from `abi-ecl.lisp`. Those four files are the *only* ones
below the seam permitted to name an implementation, and `tests/seam-tests.lisp`
asserts both that nothing else does and that each of them still needs to.

Two details worth knowing. A callable's struct parameter is **not addressable**
— `(addr p)` is rejected as "not a valid L-value" — so each is copied into a
`with-alien` local first. And the `ioctl` request numbers are *derived* from
`<sys/ioccom.h>`'s `_IOC` rather than written down, because `2148037735` is
right or wrong with nothing about it to say which.

The cost is one FFI crossing per cell instead of per row. Measured: 80 crossings
are 0.68 µs against 5.9 µs to decode the row, so a full 25-row frame is 0.148 ms
— about a thousandth of a 60 Hz budget.

`vendor/` is now upstream libvterm, unmodified, built to a single
`libcathode.dylib` that links nothing outside `/usr/lib` and `/System` —
`make -C vendor check` enforces that. A C compiler is still needed to compile
libvterm, but only there: never at Lisp build time, and never by anyone running
the application.

## Licence

**GPL-3.** The shader math and the profile data are ported from cool-retro-term,
which is GPL-3, so this is too. See [CREDITS.md](CREDITS.md) — the fonts and
libvterm keep their own licences.
