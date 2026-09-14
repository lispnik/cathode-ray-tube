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

## The easy way: do it here

Nothing about this needs CI. Your SBCL is already built
`--without-sb-core-compression` and the bundle links only `/usr/lib/libSystem`,
which is the only genuinely hard prerequisite.

```sh
# Once, ever.  Asks for an APP-SPECIFIC password from appleid.apple.com --
# not your Apple ID password.
xcrun notarytool store-credentials cathode-ray-tube \
  --apple-id you@example.com --team-id Q47YS469F2

# Then, per release:
make release SIGN_IDENTITY="Developer ID Application: Your Name (TEAMID)"
gh release create v0.1.0 dist/*.dmg
```

`make release` signs, notarises, staples, and does the disk image too. That is
the whole thing: one setup command and two per release.

**What you give up is the Intel build**, and only that. There is no universal
SBCL core, so a Mac can only build for its own architecture. If nobody has asked
for an Intel build, ship `arm64` and add the other later.

## Doing it in CI, if you want the Intel build too

Three secrets:

| secret | what |
|---|---|
| `MACOS_CERTIFICATE` | `base64 -i cert.p12` |
| `MACOS_CERTIFICATE_PASSWORD` | the password you gave the `.p12` |
| `APPLE_ID` | your Apple ID email |
| `APPLE_APP_PASSWORD` | an app-specific password |

(Four, strictly -- the certificate and its password travel together.)

There used to be seven. The signing identity, the team id and the keychain
password are all gone, because none of them was information anybody had to
supply: the certificate knows its own identity string and its own team id, and
the keychain password protects a keychain created and deleted inside one job, so
the workflow generates one. Three fewer things to type wrong, each of whose
failure mode was a fifty-minute build dying at the last step.

Export the `.p12` from Keychain Access: **login** keychain, My Certificates,
right-click the Developer ID Application entry, Export.

```sh
gh secret set MACOS_CERTIFICATE < <(base64 -i cert.p12)
gh secret set MACOS_CERTIFICATE_PASSWORD
gh secret set APPLE_ID
gh secret set APPLE_APP_PASSWORD

gh workflow run release.yml -f dry_run=true   # builds both, signs ad hoc
git tag v0.1.0 && git push origin v0.1.0      # the real thing
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
