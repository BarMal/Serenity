# Development

## Active Codespace

Use one persistent Codespace as the cloud development machine, then create normal PR branches from inside it.

Suggested setup:

```bash
git switch master
git pull
git switch -c codex/serenity-active
git push -u origin codex/serenity-active
```

Create the Codespace from `codex/serenity-active` and keep reopening that same Codespace from `https://github.com/codespaces`.

For a specific change, branch from an up-to-date `master` inside the Codespace:

```bash
git switch master
git pull
git switch -c codex/my-change
```

## Checks

Project coding standards live in `docs/coding-standards.md`. Read that document
before changing core reducers, runtime loops, configuration schema, rendering,
or state-management code.

Run the Scala checks before opening or updating a PR:

```bash
sbt -v test assembly architectureCheck
```

Run formatting and Scalafix before committing when changing Scala code:

```bash
sbt -v scalafmtAll "Compile / scalafix" "Test / scalafix"
```

### Licence notices

`sbt generateThirdPartyNotices` rewrites `THIRD-PARTY-NOTICES.md` from the resolved runtime classpath and the registry in
`third-party/`; `sbt checkThirdPartyNotices` (run in CI) fails if it is stale or a dependency has no registry row. See
[CONTRIBUTING.md](CONTRIBUTING.md#licensing).

### Changelog

`CHANGELOG.md` has two kinds of level-two heading and no others:

- `## [Unreleased]`, exactly once and first. User-visible changes land here, grouped under `###` date headings.
- `## X.Y.Z — YYYY-MM-DD` (an em dash; `X.Y.Z-rc.N` is allowed), one per release, newest first.

To cut a release, rename `[Unreleased]` to the version heading and add a fresh empty `## [Unreleased]` above it. The
release workflow publishes the matching section as the release notes and fails if it is missing or empty.

`ChangelogSectionsSpec` fails when the file has no `[Unreleased]` section or a malformed heading. To write the notes for
a version, or for the unreleased section, to a file (the release workflow publishes that file):

```bash
sbt "Test/runMain com.serenity.release.ChangelogNotes CHANGELOG.md 1.2.0 notes.md"
sbt "Test/runMain com.serenity.release.ChangelogNotes CHANGELOG.md --unreleased notes.md"
```

## Automated standards

Three layers enforce `docs/coding-standards.md` rather than leaving it to review.

**Scalafix** (`.scalafix.conf`) bans `var`, `return`, `while`, and `final val`, and organises imports.
Local mutation that genuinely earns its place is scoped with `// scalafix:off DisableSyntax`, as in
`LspFramer`.

**WartRemover** (configured in `build.sbt`, main sources only) adds rules the compiler cannot express:

| Enforced as errors | What it protects |
| --- | --- |
| `FinalCaseClass`, `LeakingSealed` | Data types stay closed; nobody re-opens a sealed hierarchy |
| `TripleQuestionMark`, `ThreadSleep` | The "no stubs" and "no blocking in IO" rules, held at zero |
| `AsInstanceOf`, `IsInstanceOf` | Pattern matching over casting |

`Null`, `Throw`, `OptionPartial` and `IterableOps` are reported as **warnings only** — each needs its
own migration. Java/AWT interop (`ui/terminal`, `ui/accessibility`), the LSP framer and the richtext
codecs are excluded, since they sit at the outermost boundary where those constructs are legitimate.
Tests are exempt entirely.

To take a deliberate exception, annotate the declaration and say why:

```scala
@SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass"))
case class Leaf(...)   // subclassed by RopeMetadataAndTraversalSpec to prove search never materialises the rope
```

**Property and law testing** covers the contracts examples cannot. Most suites are example-based and
should stay that way — "this keystroke sequence produces this buffer" is exactly what an example is
for. Reach for a property when the claim holds over *all* inputs rather than chosen ones:

- Typeclass instances have laws. `checkAll("Order[BufferId]", OrderTests[BufferId].order)` in
  `testkit/IdentifierLawSpec.scala` is the pattern; mix in `FunSuiteDiscipline` and `Configuration`.
- Data structures have representation invariants. `rope/RopePropertySpec.scala` generates tree shapes
  rather than hand-building them, so rope behaviour is tested independently of shape as
  `docs/coding-standards.md` requires.

Shared generators live in `testkit/Generators.scala` — add to that rather than defining `Arbitrary`
instances per suite. If you write a generator whose output could silently degenerate, assert its
variety: `RopePropertySpec` has a property doing exactly that, because a generator that only emitted
flat leaves would make every shape-independence claim in the file pass while testing nothing.

Dependencies (`scalacheck-1-18`, `cats-laws`, `discipline-scalatest`) are `% Test` only.

**`architectureCheck`** enforces size and layering against `project/architecture-baseline.tsv`:
methods ≤ 80 lines, files ≤ 600 lines, and no `java.awt`/layout-engine imports inside
`state/reducers`. It is a ratchet, not a threshold — the baseline lists what is already over, and the
build fails if a file gets worse, a new one starts over target, or a layer is crossed. Fixing an
entry means deleting it:

```bash
sbt writeArchitectureBaseline   # after removing a violation, to bank the win
```

The baseline may shrink, never grow. If you must add to it, explain the entry in review: CI's
`architecture` job fails a PR whose baseline entry count or total measured lines/hits grows unless the
diff also adds a `# paydown: <why, issue link>` comment line to `project/architecture-baseline.tsv`.

## Running the app

### Faster startup with a class-data archive

Most of a cold start is spent loading and verifying classes. JDK 19+ can keep them in a class-data sharing (AppCDS)
archive that it writes on the first exit and maps on every later start:

```bash
mkdir -p "${XDG_CACHE_HOME:-$HOME/.cache}/serenity"
java -XX:SharedArchiveFile="${XDG_CACHE_HOME:-$HOME/.cache}/serenity/serenity.jsa" -XX:+AutoCreateSharedArchive \
  -jar target/scala-3.9.0/Serenity.jar
```

The first run with a missing or stale archive (a rebuilt jar, a different JDK) starts normally and writes the archive
as it exits, printing a few `[warning][cds] Skipping ...` lines for classes CDS cannot store; later runs start from it.
The archive is about 65 MB.

The desktop packages pass the same flags through jpackage's `--java-options`, with the archive next to the jar in the
app image (`$APPDIR/serenity.jsa`), and build their runtime with `--generate-cds-archive` because a dynamic archive
needs the runtime's own base archive. macOS is left out: writing into a signed `.app` would break its signature.

### Heap flags in the desktop packages

The packaged app runs with `-XX:+UseG1GC -XX:MaxRAMPercentage=25 -XX:G1PeriodicGCInterval=60000` on every OS. The heap
is capped at a quarter of physical memory, and G1 (named explicitly, since the JVM picks Serial on small machines) runs
a collection after 60 s idle so freed heap goes back to the OS. They are set beside the AppCDS flags in
`.github/workflows/desktop-publish.yml` and `desktop-release.yml`; `sbt run` is unaffected.

### Startup warm-up

Once the first frame is drawn, Serenity types, deletes and moves through a throwaway editor drawn off-screen, so the
JIT compiles those paths before the first real keystrokes. It never touches your buffers, session, undo history or
language servers, and stops at the first key press or click. Turn it off with `startup.warm_up = false`; the log
reports how it ended (`[WARMUP] Completed(...)` or `[WARMUP] Interrupted`).

### JetBrains Runtime and native Wayland

On a Wayland desktop, a stock JDK draws through XWayland. The [JetBrains Runtime](https://github.com/JetBrains/JetBrainsRuntime/releases)
(JBR 21 or later; the `jbr` or `jbrsdk` build for your platform) also has a native Wayland toolkit, and Serenity selects
it automatically when it runs on JBR with `WAYLAND_DISPLAY` set:

```bash
tar -xzf jbr-21*-linux-x64-*.tar.gz -C ~/.local/share
export JAVA_HOME=~/.local/share/jbr-21...   # the extracted directory
"$JAVA_HOME/bin/java" -jar target/scala-3.9.0/Serenity.jar
```

The startup log names the choice: `[TOOLKIT] Wayland (Wayland session on JetBrains s.r.o.)`. To override it, set
`SERENITY_TOOLKIT`:

| Value | Effect |
| --- | --- |
| `auto` (or unset) | Native Wayland on JBR in a Wayland session; the JVM's default otherwise |
| `wayland` | Native Wayland whenever the runtime has it, even without `WAYLAND_DISPLAY` |
| `x11` | X11, through XWayland on a Wayland desktop |

Passing `-Dawt.toolkit.name=...` yourself always wins over both. The choice has to be made before any AWT class loads,
which is why it is an environment variable rather than a config setting. Under the native toolkit, `window.chrome = auto`
uses the compositor's decorations, since Wayland does not let an application move its own window from a custom title bar.

JBR's Vulkan renderer is opt-in and experimental; try it with `-Dsun.java2d.vulkan=true`. The AppCDS flags above work
on JBR too, but keep a separate archive per runtime or each switch will rewrite it.

## Codex CLI

Codex CLI is installed in the Codespace image. Start it from the repository root:

```bash
codex
```

Sign in with ChatGPT when prompted, or add `OPENAI_API_KEY` as a Codespaces secret before creating or rebuilding the Codespace if you prefer API-key auth.

Desktop package checks run in GitHub Actions on PRs. Publishing is handled by `desktop-publish.yml`, which has two channels.

### Releasing

The `vX.Y.Z` tag is the one source of truth for a version: sbt-dynver derives `BuildInfo.version` (what `--version` and
About report) from it, `PackageVersion` derives the numeric jpackage `--app-version` from it (`sbt writePackageVersion`
writes `target/package/app-version.txt`), and the workflow fails if they disagree. A first component of 0 is rejected
because macOS packaging refuses it, so the first release is `v1.0.0` or later.

To cut a release:

1. In `CHANGELOG.md`, rename `## [Unreleased]` to `## X.Y.Z — YYYY-MM-DD` and add a fresh empty `## [Unreleased]` above it
   (see [Changelog](#changelog)), then merge that to `master`.
2. Tag the merge commit and push the tag: `git tag -a vX.Y.Z -m "Serenity X.Y.Z" && git push origin vX.Y.Z`.

Pushing the tag runs the full three-OS build and test, then publishes a release named `Serenity X.Y.Z` carrying
`Serenity.jar`, the three app-image zips and `SHA256SUMS`, with that version's CHANGELOG section as the notes. A plain
`X.Y.Z` is published as the latest release; `X.Y.Z-rc.N` is published as a pre-release. The run fails, and publishes
nothing, if the tag is not `vX.Y.Z[-pre]`, is not on `master`, has no non-empty CHANGELOG section, the release already
exists, or the tag, jpackage version and `--version` output disagree. Releases are never rewritten; fix forward with a
new tag.

To verify a download, put it beside `SHA256SUMS` and run `sha256sum --check --ignore-missing SHA256SUMS`
(`shasum -a 256 -c` on macOS).

Every push to `master` also republishes the `desktop-latest` pre-release, titled "Nightly desktop build". It is a
rolling channel, not a version: it is replaced in place and carries the unreleased changes. macOS builds are signed
ad hoc, and Developer ID signing and notarization switch on when the `MACOS_*` secrets in `desktop-publish.yml` are set.
