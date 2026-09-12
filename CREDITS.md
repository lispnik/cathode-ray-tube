# Credits

## cool-retro-term

cathode-ray-tube is a port of
[cool-retro-term](https://github.com/Swordfish90/cool-retro-term) by **Filippo
Scognamiglio**, and would not exist without it. The CRT effects here are a
translation of its GLSL shaders to Metal Shading Language, and the fourteen
built-in profiles are its profiles, value for value. cool-retro-term is
GPL-3-licensed and so is this, for that reason.

Upstream is the original work. This is a port, and it does not speak for it.

## libvterm

Terminal emulation is [libvterm](https://www.leonerd.org.uk/code/libvterm/)
0.3.3 by **Paul "LeoNerd" Evans**, vendored under `vendor/libvterm/` and MIT
licensed. See `vendor/libvterm/LICENSE`.

## Fonts

The bundled fonts are the set cool-retro-term ships, redistributed under the
same arrangement: each family keeps its own license file alongside it in
`res/fonts/`. They are not covered by this project's GPL-3 license and are the
work of their respective authors.

| Family | Directory |
|---|---|
| Apple ][ (PrintChar21, PRNumber3) | `res/fonts/apple2/` |
| Atari Classic | `res/fonts/atari-400-800/` |
| BigBlue Terminal | `res/fonts/bigblue-terminal/` |
| Cozette | `res/fonts/cozette/` |
| Departure Mono | `res/fonts/departure-mono/` |
| Fira Code | `res/fonts/fira-code/` |
| Fixedsys Excelsior | `res/fonts/fixedsys-excelsior/` |
| Gohu | `res/fonts/gohu/` |
| Greybeard | `res/fonts/greybeard/` |
| Hack | `res/fonts/hack/` |
| IBM 3278 (3270) | `res/fonts/ibm-3278/` |
| Iosevka Term | `res/fonts/iosevka/` |
| JetBrains Mono | `res/fonts/jetbrains-mono/` |
| Oldschool PC (PxPlus IBM EGA/VGA) | `res/fonts/oldschool-pc-fonts/` |
| OpenDyslexic Mono | `res/fonts/opendyslexic/` |
| Pet Me / Pet Me 64 | `res/fonts/pet-me/` |
| Source Code Pro (Sauce Code Pro) | `res/fonts/source-code-pro/` |
| Terminess (Terminus) | `res/fonts/terminus/` |
| Unscii | `res/fonts/unscii/` |

Several are Nerd Font patches of the upstream families; the patches are by the
[Nerd Fonts](https://github.com/ryanoasis/nerd-fonts) project.

## Common Lisp

- [`objc`](https://github.com/lispnik/objc) — the Objective-C bridge this is built on.
- [`asdf-macos-app`](https://github.com/lispnik/asdf-macos-app) — the `.app` bundle.
- [`cffi`](https://github.com/cffi/cffi), [`fiveam`](https://github.com/lispci/fiveam),
  and the rest, vendored by [ocicl](https://github.com/ocicl/ocicl) and pinned in
  `ocicl.csv`.
