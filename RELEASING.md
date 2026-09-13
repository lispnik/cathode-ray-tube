# Releasing

```sh
make app                    # ad hoc, runs here, cannot be notarised
make app SIGN_IDENTITY=...  # signed with a Developer ID
make release SIGN_IDENTITY=...   # notarise the app and the dmg, staple both
```

`make release` is `notarize` then `notarize-dmg`, and both matter. Notarising
the **app** is what the notary service inspects; stapling the **disk image** is
what makes the thing a user actually downloads recognisable to Gatekeeper.
Doing only the first leaves the download itself unrecognised, which is the
version of this that looks like it worked.

## Doing it by hand, once

```sh
# The notary profile, stored once in your keychain.  It asks for an
# APP-SPECIFIC password from appleid.apple.com -- not your Apple ID password.
xcrun notarytool store-credentials cathode-ray-tube \
  --apple-id you@example.com --team-id TEAMID

make release SIGN_IDENTITY="Developer ID Application: Your Name (TEAMID)"
```

`dist/cathode-ray-tube-<version>-<arch>.dmg` is then signed, notarised and
stapled.

## Doing it in CI

Push a `v*` tag. `.github/workflows/release.yml` builds both architectures,
signs, notarises, staples and publishes. `workflow_dispatch` with
`dry_run: true` builds and signs without notarising or publishing, which is how
to check a change to the workflow without spending a version number.

### Secrets

| Secret | What it is |
|---|---|
| `MACOS_CERTIFICATE` | the Developer ID Application `.p12`, base64 encoded |
| `MACOS_CERTIFICATE_PASSWORD` | its export password |
| `MACOS_SIGN_IDENTITY` | `Developer ID Application: Your Name (TEAMID)` |
| `KEYCHAIN_PASSWORD` | anything; it is a throwaway keychain the job deletes |
| `APPLE_ID` | the Apple ID the team belongs to |
| `APPLE_TEAM_ID` | the ten-character team identifier |
| `APPLE_APP_PASSWORD` | an app-specific password from appleid.apple.com |

```sh
base64 -i DeveloperID.p12 | pbcopy     # for MACOS_CERTIFICATE
```

## Three things that fail slowly

**SBCL must be built `--without-sb-core-compression`.** Homebrew's enables zstd
and therefore links `/opt/homebrew/lib/libzstd.dylib`. Such a bundle notarises
*perfectly well* and then dies with a dyld error on a Mac that has never had
Homebrew — Apple checks the signature, not whether your dylibs exist on someone
else's disk. The release workflow builds SBCL from source and caches it, and
`make notarize` refuses a bundle that loads anything from outside itself.

**`security set-key-partition-list` is not optional** in CI. Without it,
`codesign` blocks on a UI authorisation prompt no runner can answer, and the job
hangs until its timeout instead of failing.

**There is no universal SBCL core**, so there is no universal binary. Two disk
images, one per architecture, and the release notes say which is which.

## What `make notarize` checks first

Both of these are cheap here and expensive to discover later:

- that the bundle is not signed **ad hoc** — Apple refuses those, but only after
  the upload;
- that nothing in `Contents/MacOS` or `Contents/Frameworks` loads a library from
  outside the bundle.

The second guard is **not** the one `utc-status-app` ships. That version
inspects `Contents/MacOS/<exe>` only, and here it would give a false pass:
`libcathode.dylib` is `dlopen`ed by CFFI and is never a link-time dependency of
that binary, so it would never appear in the output being checked.
