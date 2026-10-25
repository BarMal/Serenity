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

### DNG-free Skiko natives

The stock Skiko natives link the Adobe DNG SDK, whose licence terms conflict with GPLv3 (#2028). `.github/workflows/skiko-natives.yml`
rebuilds them from the upstream Skia and Skiko tags in `ci/skiko/pins.env` with the reviewed patches beside it, scans every
jar with `scripts/check_no_dng.py` (after proving the scanner on the stock library), and on a manual run publishes the six
native jars and the patched Kotlin API jar (`skiko-awt`), attested, to the `skiko-maven` branch that GitHub Pages serves.

The patch `ci/skiko/skiko-read-pixels-into.patch` adds `Bitmap.readPixelsInto` (and `readPixelsArraySize`) to Skiko's Kotlin
classes, which let the Skia raster path read a frame into an array it reuses instead of allocating one per chunk. Those classes
are the same on every platform, so the linux-x64 group builds the `skiko-awt` jar once.

A pull request builds all four runner groups and merges and scans the six native jars and the API jar; nothing is published. Skia is cached by pin and
patch, so a warm run takes about ten minutes. A cold run builds Skia for every target: roughly 20 minutes on Linux, 50 on macOS
and Windows.

To change a pin or a patch:

1. Edit `ci/skiko/pins.env` or a patch, and bump `NATIVES_VERSION` in `ci/skiko/natives-version.env`
   (`<skiko version>-dngfree.<n>`): the Maven coordinates follow from it. The Skia cache key follows `pins.env` and the Skia
   patch only, so a new version with the same Skia sources does not rebuild Skia.
2. Check `git apply --check` for each patch against the new tag, and run the unit tests in `scripts/`.
3. Open the pull request and wait for the four groups and the merge job to pass.
4. Run the workflow manually with `groups` = `all` and `publish` = `true`, then point the build at the new `NATIVES_VERSION`.

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
own migration. Java/AWT interop (`ui/terminal`, `ui/accessibility`) and the LSP framer are excluded, since they
sit at the outermost boundary where those constructs are legitimate. The richtext codecs are checked like everything
else; the JAXP callbacks that must throw carry a `@SuppressWarnings` saying why.
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

`architectureCheck` also enforces purity (`project/PurityChecks.scala`, #1677), at the level of declarations, so a local
`var` or mutable collection inside a method is fine. A top-level `object` may not declare a `val`/`lazy val`/`var` that
holds `AtomicReference/Integer/Long/Boolean`, `synchronized`, `ThreadLocal`, `ConcurrentHashMap`, `mutable.`, or a
`java.util` `HashMap`/`LinkedHashMap`/`ArrayDeque`/`ArrayList`/`WeakHashMap`/`IdentityHashMap` (only where the file
imports it), and may not hold an instance of a class that has such a member (`new Cache`, `Cache.bounded()`).
Declarations that cannot move are named in `ArchitectureChecks.PurityAllowlist` with a reason, apart from the baseline;
`docs/allowlist-review.md` argues each one's redesign.

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

What the app actually holds, and what each flag does to it, is measured in [docs/memory-footprint.md](docs/memory-footprint.md);
`scripts/measure-footprint.sh` reproduces it against an assembled jar.

### Returning heap to the OS

At startup, unless `performance.memory.compact = false`, Serenity sets `MinHeapFreeRatio=10`, `MaxHeapFreeRatio=30` and
(under G1 only) `G1PeriodicGCInterval=30000` through `HotSpotDiagnosticMXBean.setVMOption`, in both window and terminal
modes. The packaged `-XX:G1PeriodicGCInterval=60000` is therefore replaced by 30 s once the app is up. With the setting
off the JVM keeps its launch values. A measured idle footprint dropped from about 520 MiB to about 317 MiB
(`docs/memory-footprint.md`, PR #2266). The log records what was applied on a `[MEMORY]` line.

### Startup warm-up

Once the first frame is drawn, Serenity types, deletes and moves through a throwaway editor drawn off-screen, so the
JIT compiles those paths before the first real keystrokes. It never touches your buffers, session, undo history or
language servers, and stops at the first key press or click. Turn it off with `startup.warm_up = false`; the log
reports how it ended (`[WARMUP] Completed(...)` or `[WARMUP] Interrupted`).

### Java version and launch options

CI, the benchmarks and the packages use Temurin 25 (LTS); the app images bundle it through jlink. Two launch options come
with JDK 24 and later, and both packages pass them (`desktop-publish.yml`, `desktop-release.yml`):

- `--enable-native-access=ALL-UNNAMED`. Skiko, JNA and JLine load native libraries, which JDK 24 warns about on every
  load and a later JDK will block. The assembled jar grants the same access from its manifest
  (`Enable-Native-Access: ALL-UNNAMED`, `AssemblyManifestSpec`), so `java -jar Serenity.jar` needs no option.
- `--sun-misc-unsafe-memory-access=allow`. Scala's lazy vals (`scala.runtime.LazyVals`, unchanged in Scala 3.10) still call
  `sun.misc.Unsafe.objectFieldOffset`, which JDK 24 and later report on the first call. A manifest cannot carry this
  option, so `java -jar Serenity.jar` prints that warning on stderr until Scala drops the call; it is harmless.

`scripts/smoke-test-app-image.sh` fails when the packaged app prints either warning. The JetBrains Runtime 25 is not
supported yet: it fails 194 tests and garbles some text (#2380, #2390).

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
| `auto` (or unset) | Native Wayland on JBR in a Wayland session whose socket connects; the JVM's default otherwise |
| `wayland` | Native Wayland whenever the runtime has it and the socket connects, even without `WAYLAND_DISPLAY` |
| `x11` | X11, through XWayland on a Wayland desktop |

Before choosing native Wayland, Serenity connects to the compositor's socket once, the way libwayland does
(`$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY`, `wayland-0` when unset, an absolute `WAYLAND_DISPLAY` as given; a set
`WAYLAND_SOCKET` is trusted). If it does not connect, for example a `WAYLAND_DISPLAY` left over from a compositor that has
exited, the JVM's default toolkit is used instead, even with `SERENITY_TOOLKIT=wayland`, and the startup log says why:
`[TOOLKIT] PlatformDefault (WAYLAND_DISPLAY is set, but the socket does not connect (/run/user/1000/wayland-1: Connection refused))`.
Without the check the JetBrains Runtime would try `WLToolkit` anyway and end up headless.

Passing `-Dawt.toolkit.name=...` yourself always wins over both. The choice has to be made before any AWT class loads,
which is why it is an environment variable rather than a config setting. Under the native toolkit, `window.chrome = auto`
uses the compositor's decorations, since Wayland does not let an application move its own window from a custom title bar.

JBR's Vulkan renderer is opt-in and experimental; try it with `-Dsun.java2d.vulkan=true`. The AppCDS flags above work
on JBR too, but keep a separate archive per runtime or each switch will rewrite it.

### Renderer selection and Wayland

Serenity paints with Java2D, or with Skia (Skiko) where it can load. Each setting below can be given as a launch flag,
an environment variable or a config key; the flag wins over the variable, which wins over the file, which wins over the
default. An unrecognised flag value, or an unrecognised `SERENITY_RENDER_BACKEND`, `SERENITY_WAYLAND_RENDER` or
`SERENITY_TOOLKIT`, stops the launch with a message naming the variable, the value and the choices. An unrecognised
config value is reported by the usual config warning (log and start page) with the same choices, and the setting uses
its default.

| Config key | Values | Default | Environment variable | Flag |
| --- | --- | --- | --- | --- |
| `ui.render.backend` | `auto`, `skia`, `java2d` | `auto` | `SERENITY_RENDER_BACKEND` | `--renderer=<value>` |
| `ui.render.wayland` | `java2d`, `skia-raster`, `xwayland` | `skia-raster` | `SERENITY_WAYLAND_RENDER` | `--wayland-render=<value>` |

`auto` uses Skia's GPU layer wherever the native library loads: Direct3D on Windows, Metal on macOS, OpenGL on Linux with
an X11 or XWayland display (Skiko falls back to its own software redrawer where the GPU API cannot start). If the layer
itself cannot be made at run time (no GL context, a native failure) the window paints with the Skia CPU raster, and with
Java2D only if that cannot make a surface either. If the native library does not load, Java2D is used and the startup
log says why. `skia` and `auto` differ only in the reason logged; `java2d` forces Java2D. Terminal and safe-mode launches
never load Skia. The startup log records the outcome as `[RENDER] backend=<skia|skia-raster|java2d> reason=...`. A
windowed launch loads the native library to find out, which took about 0.33 s on the machine this was measured on.

Skia's window layer needs an X11 display, so on a Linux session where the native Wayland toolkit (`WLToolkit`) would be
used, or where there is no X11 display, `ui.render.wayland` decides what happens:

| Value | Effect |
| --- | --- |
| `skia-raster` (default) | Keep `WLToolkit`; Skia paints on the CPU into an image that Java2D presents. Falls back to Java2D with the reason `skia raster surface unavailable` while no raster surface is registered |
| `java2d` | Keep `WLToolkit` and use Java2D |
| `xwayland` | Switch to the X11 toolkit through XWayland so Skia can use the GPU. An explicit `SERENITY_TOOLKIT` or `-Dawt.toolkit.name` still wins, and the Skia raster is used instead |

The renderer and the toolkit are settled together before any AWT class loads, so a config change needs a restart.

#### Comparing the renderers

`serenity --render-benchmark` opens a window of its own for each renderer (Java2D, the Skia layer, then Skia's CPU
raster, which paints with Skia and presents through Java2D), draws the same built-in document through the same scripted
actions on each, prints the tables and saves them as
`render-benchmark-<timestamp>.md` and `.json` in the working directory, then quits without starting the editor. The
scenarios are a cold full frame, typing one character, scrolling one line, scrolling five lines, a page down and a
caret blink. A frame is timed from the moment the renderer is called to the last time the canvas painted (Java2D, raster)
or the layer drew the frame's published picture (Skia layer). Skiko draws the layer's picture on the GPU afterwards, on
its own thread, and gives the layer no GPU context to wait on, so the layer's figures cover recording the frame and
Skiko's frame pacing (with vsync on, a draw can wait up to a refresh), not the GPU finishing. Options: `--render-benchmark-size WIDTHxHEIGHT` (default
`1600x1000`), `--render-benchmark-frames` (timed frames per scenario, default 200) and `--render-benchmark-warmup`
(untimed frames first, default 30). It needs a display and always runs both renderers whatever `ui.render.backend` says.

The benchmark settles the toolkit and renderer like an editor launch does, so `SERENITY_TOOLKIT` and `-Dawt.toolkit.name`
apply to it too (on a runtime without `WLToolkit`, `SERENITY_TOOLKIT=wayland` falls back to the default toolkit). The report
records the toolkit class that drew and the launch's choice, how many Skia layers the window ended with, and what the
layer's draws cost: draws against published frames, the mean wait from publishing a frame to its first draw, the mean
time per draw adding the picture and filling the carets, whether a GPU wait was possible, and the size of the published
pictures. Besides the p50, p95 and max of each scenario the report has a tail table (p75, p90, p99, the frames over 33 ms
and the samples that ended before the render call returned) and a frames-to-present table: each time rounded up to whole
display refreshes, using the rate Java reports for the screen or an assumed 60 Hz that the report says it assumed, with
the samples that took more than one refresh. A window that ends with more than one layer gets no verdict, since each
frame would have waited behind the others' draws. The raster arm does not take part in the verdict.

The report names the render API Skia actually used. If Skiko had to fall back to a software redrawer, or the layer could
not be made, it prints `GPU NOT USED` or `Skia layer unavailable` instead of a verdict, so a software run is never read as a
GPU result. On a GPU the verdict is that Skia wins only if its p50 and p95 are no worse than Java2D's in every scenario
except the cold full frame and its p95 is better when scrolling and on a page down. The verdict is information about this machine: `auto` no longer depends on it.

The header also records the CPU model, the CPU frequency governor and whether the machine was on AC or battery (`unknown`
where the platform does not say: Linux reads `/proc/cpuinfo`, cpufreq and `/sys/class/power_supply`; Windows gives only the CPU
model, macOS the CPU model), so a shift between days can be told from a code change.

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
`Serenity.jar`, the three app-image zips, `Serenity.cdx.json` (the SBOM, below) and `SHA256SUMS`, with that version's CHANGELOG section as the notes. A plain
`X.Y.Z` is published as the latest release; `X.Y.Z-rc.N` is published as a pre-release. The run fails, and publishes
nothing, if the tag is not `vX.Y.Z[-pre]`, is not on `master`, has no non-empty CHANGELOG section, the release already
exists, or the tag, jpackage version and `--version` output disagree. Releases are never rewritten; fix forward with a
new tag.

The SBOM is a [CycloneDX](https://cyclonedx.org/) 1.6 JSON document listing every runtime dependency in the jar, with
versions, purls, hashes and the dependency tree, generated by [sbt-sbom](https://github.com/sbt/sbt-sbom) (pinned in
`project/plugins.sbt`). `sbt makeBom` writes it to `target/Serenity.cdx.json`; it carries no timestamp or serial number,
so an unchanged dependency graph gives byte-identical output. Its component set is the same runtime classpath that
`THIRD-PARTY-NOTICES.md` is rendered from, and `SbomSpec` fails if the two ever differ. A tag build fails, and publishes nothing, if the SBOM cannot be generated; the nightly build
publishes without one rather than stall. The SBOM is listed in `SHA256SUMS` like the other assets.

To verify a download, put it beside `SHA256SUMS` and run `sha256sum --check --ignore-missing SHA256SUMS`
(`shasum -a 256 -c` on macOS).

A scheduled run at 04:43 UTC every night also republishes the `desktop-latest` pre-release, titled "Nightly desktop
build", from the tip of `master`; the "Desktop Publish" workflow can also be run by hand from the Actions tab to publish
sooner. A push to `master` does not publish. It is a rolling channel, not a version: it is replaced in place and carries
the unreleased changes. macOS builds are signed
ad hoc, and Developer ID signing and notarization switch on when the `MACOS_*` secrets in `desktop-publish.yml` are set.
