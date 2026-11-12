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

### Fast local loop

Keep one sbt shell open while you work, so the compiler and the test classes stay warm, and run only the specs you are
changing. Measured on a 4-core Linux machine (#2557):

| What | Time |
|---|---:|
| `testOnly` of one spec in a warm sbt shell | about 4 s |
| The same from a fresh `sbt` after a one-line edit | about 23 s |
| A cold compile of main and test | about 12 min |
| The full `sbt test` | 15–26 min |

```text
sbt                                          # once; keep it open
> testOnly com.serenity.rope.RopeBTreeSpec   # one spec
> testOnly *RopeBTree* -- -z "split"         # tests whose name contains "split"
> testQuick                                  # specs that failed last time or whose code changed
> ~testOnly com.serenity.rope.RopeBTreeSpec  # re-run on every save
```

### Before a PR

Run the Scala checks before opening or updating a PR. The full `sbt test` is left to CI: the merge train runs the whole
suite on Linux, Windows and macOS for each batch before anything merges, so run the specs you changed (see the fast
local loop above) rather than the 15–26 minute full run:

```bash
sbt -v assembly architectureCheck
```

Run formatting and Scalafix before committing when changing Scala code:

```bash
sbt -v scalafmtAll "Compile / scalafix" "Test / scalafix"
```

### Turning Windows and macOS CI off (Linux only)

The repository variable `CROSS_OS_CI` is the one switch (Settings, Secrets and variables, Actions, Variables). Unset, or any
value but `off`, every job runs as usual. At `off` (#2618) the Windows and macOS jobs are skipped:

- Cross-OS Tests: its four jobs are skipped, so the required checks `Test (windows)` and `Test (macos)` report skipped,
  which branch protection counts as passing.
- The merge train gates on `ci.yml` alone (`scripts/merge_train.py`, `required_workflows`) and no longer dispatches or
  waits for Cross-OS Tests; Auto Squash Merge stops requiring the two Cross-OS checks.
- Desktop Package Check, Ligature Oracle, Skia Natives Probe and the macOS and Windows Skiko builds run Linux only.
- Desktop Publish skips the Cross-OS nightly gate, builds the Linux jar and zip, and publishes only those (the checksum
  file and the release notes name them); Windows and macOS assets already on a release are left as they are.

Delete the variable to turn everything back on; no job was removed. A change to a workflow file is merged by hand.

### Build layout

The build has an aggregate `root` project with no sources and an `app` project that holds the whole application (#2502).
Its sources are still under `src/`; they move into `modules/<name>/` in a later step. Every output keeps its path:
`target/scala-*/Serenity.jar`, `serenity-perf.jar`, `target/test-reports`, `target/Serenity.cdx.json`.

- Tasks (`test`, `compile`, `clean`, `assembly`, `makeBom`, `architectureCheck`, `moduleGraphCheck`, ...) run from `root`
  and reach `app` through aggregation.
- Input tasks do not aggregate, so name the project: `sbt app/run`, `sbt "app/testOnly com.serenity.SomeSpec"`,
  `sbt "app/Test/runMain ..."`, `sbt "export app/Test/fullClasspath"`. Run unqualified from `root`, they run nothing.
- A checkout from before the split has one root project; scripts that must handle both (`run-perf-benchmark.sh`,
  `bench-artifacts.yml`) look for `lazy val app` in `build.sbt`.

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
sbt "app/Test/runMain com.serenity.release.ChangelogNotes CHANGELOG.md 1.2.0 notes.md"
sbt "app/Test/runMain com.serenity.release.ChangelogNotes CHANGELOG.md --unreleased notes.md"
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

The Linux and Windows runtimes are built with `--generate-cds-archive`, which gives the JVM its base archive. macOS
cannot write an archive after install, because writing into a signed `.app` would break its signature. All three
packages ship the JDK 25 AOT cache described below instead of a dynamic AppCDS archive.

The runtime's base archive is dumped again after `jpackage` by `scripts/regenerate-base-cds-archive.sh`
(`java -Xshare:dump --enable-native-access=ALL-UNNAMED`). jlink dumps it without that option, but the launcher passes it,
and the JVM then logs `Mismatched values for property jdk.module.enable.native.access` (an error line from the second
start on) and drops its archived module graph. `scripts/smoke-test-app-image.sh` runs the image with `-Xlog:cds` and fails
on that line.

The **macOS bundle ships the same cache inside the signed `.app`** (`Contents/app/serenity.aot`). `desktop-publish.yml`
trains it after `jpackage` and before `scripts/sign-macos-app.sh`, so nothing writes into the bundle once it carries a
signature, then runs `scripts/check-aot-cache.sh` on the signed bundle and verifies the signature again. The bundled
runtime has no `bin/java`, which `-XX:AOTCacheOutput` forks to assemble the cache, so `scripts/train-aot-cache.sh` trains a
bundle in two launcher runs (`-XX:AOTMode=record`, then `-XX:AOTMode=create`). A CI spike on `macos-latest` (#2522,
comment 6625934280) showed a signed, hardened-runtime bundle opening a cache trained before signing, in place, from
`/Applications` and from a DMG (a bundle moved to another parent directory keeps a valid cache). That spike used an
ad-hoc signature and a stand-in app: a Developer ID signature and notarisation are checked on the first release build.
The scripts do not use `timeout(1)`, which macOS lacks.

The **Linux and Windows packages ship a JDK 25 AOT cache** (`$APPDIR/serenity.aot`, about 92 MB on disk and 22 MB
compressed on Linux) instead of the AppCDS flags. Windows used to write an AppCDS archive on its first exit, so its first
run was the slow one (3.38 s against 1.99 s, #2407); the cache is written at build time, so every run is a warm one.
The scripts take all three layouts (`bin/Serenity` + `lib/app` on Linux, `Serenity.exe` + `app` on Windows, where they
run under Git Bash, and the macOS bundle above). On Linux it starts about 12% faster (first painted frame: 1406 ms against 1606 ms on a CI runner,
#2380). `scripts/train-aot-cache.sh` writes it from one `--smoke-test` run of the finished image
(`-XX:AOTCacheOutput`) and then adds `-XX:AOTCache=$APPDIR/serenity.aot` to `Serenity.cfg`; the cache has to exist
before the option is added, and the JVM refuses `-XX:AOTCache` together with `-XX:SharedArchiveFile`, so an image gets
one or the other. The cache records the jar's size and modification time. If the jar no longer matches (a hand-patched
image), the JVM rejects the cache for good, prints `[error][aot]` lines and starts about 1.5 s slower than AppCDS would;
it does not regenerate. `scripts/check-aot-cache.sh` runs in the package workflows and proves that a valid cache is
opened, and that a missing or stale one still leaves an app that starts. To time start-up on a machine with a display:

```bash
python3 bench/ttfw.py dist/Serenity/bin/Serenity 10 aot
```

### Heap flags in the desktop packages

The packaged app runs with `-XX:+UseG1GC -XX:MaxRAMPercentage=25 -XX:G1PeriodicGCInterval=60000` on every OS. The heap
is capped at a quarter of physical memory, and G1 (named explicitly, since the JVM picks Serial on small machines) runs
a collection after 60 s idle so freed heap goes back to the OS. They are set beside the AppCDS flags in
`.github/workflows/desktop-publish.yml` and `desktop-release.yml`; `sbt app/run` is unaffected.

What the app actually holds, and what each flag does to it, is measured in [docs/memory-footprint.md](docs/memory-footprint.md);
`scripts/measure-footprint.sh` reproduces it against an assembled jar.

### Returning heap to the OS

At startup, unless `performance.memory.compact = false`, Serenity sets `MinHeapFreeRatio=10`, `MaxHeapFreeRatio=30` and
(under G1 only) `G1PeriodicGCInterval=30000` through `HotSpotDiagnosticMXBean.setVMOption`, in both window and terminal
modes. The packaged `-XX:G1PeriodicGCInterval=60000` is therefore replaced by 30 s once the app is up. With the setting
off the JVM keeps its launch values. A measured idle footprint dropped from about 520 MiB to about 317 MiB
(`docs/memory-footprint.md`, PR #2266). The log records what was applied on a `[MEMORY]` line.

### Skia natives per OS

Skiko ships one native library per OS and architecture, so each build carries only its own. `build.sbt` picks the
`skiko-awt-runtime-<os>-<arch>` artifact from the build host; set `SERENITY_SKIKO_TARGET` (or `-Dserenity.skiko.target`) to
one of `linux-x64`, `linux-arm64`, `macos-x64`, `macos-arm64`, `windows-x64` or `windows-arm64` to build another target. The
runnable jar keeps its natives. The app image does not: `scripts/SkikoNatives.java` moves them into `app/natives` (with
`icudtl.dat` on Windows), the image's jar is left without them, and the launcher passes
`-Dskiko.library.path=$APPDIR/natives`, so Skiko never unpacks anything into `~/.skiko` and macOS signs the dylib as part
of the bundle (`scripts/sign-macos-app.sh`, entitlements in `ci/macos.entitlements`). `scripts/SkikoProbe.java` loads the
library from that layout and renders a 1x1 raster surface; the Skia Natives Probe workflow runs it on Windows and on Linux
under Xvfb.

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

### SDL3 (the window library behind native Wayland, #2380)

`com.serenity.ui.sdl` binds SDL3 through the JDK's foreign-function API, with no binding library. Nothing selects it yet:
the Swing window is still the only one the application opens. `libSDL3.so.0` is built from the upstream tag pinned in
`ci/sdl3/pins.env` (`ci/sdl3/build-sdl3.sh`, in an `ubuntu:22.04` container so it asks no more of glibc than that release
does) and travels in the Linux packages as `app/natives/libSDL3.so.0`; the launcher passes
`-Dserenity.sdl3.library.path=$APPDIR/natives`. Without that property the binding asks the system loader for
`libSDL3.so.0`.

- `SdlLayouts` holds the byte offsets of the SDL structs the binding reads and the values of the constants it passes.
  `ci/sdl3/layout.c` prints the same from the pinned SDL headers; its output is checked in as
  `src/test/resources/sdl3/layout.txt`, `SdlLayoutsSpec` holds the Scala table against it, and the `SDL3 binding` CI job
  diffs the file against a fresh build of the program. When the pin moves, regenerate the file with
  `cc -I <SDL>/include -o layout ci/sdl3/layout.c && ./layout > src/test/resources/sdl3/layout.txt` and read the diff.
- `sbt test` leaves out the specs that load the real library. To run them: build it with
  `ci/sdl3/build-sdl3.sh work out` (needs the packages listed in the script), then
  `sbt -Dserenity.sdl3.library.path=$PWD/out sdlNativeTest`. They use SDL's `dummy` and `offscreen` video drivers, so no
  compositor is needed; the GL specs on `offscreen` need Mesa's EGL (`libegl1 libgl1 libgl1-mesa-dri libgles2`; Skiko's natives link `libGL.so.1`), and run on its
  software rasteriser when there is no GPU.
- `SdlGlWindow` is the window host with a GL context (phase 3): window, EGL context, event pump, resize and scale,
  clear and swap, all on one dedicated thread. Where SDL cannot make a context (the `dummy` driver, a machine without
  EGL) `SdlGlWindow.open` returns SDL's reason and leaves nothing behind; `PresentPath` turns that into
  `RasterFallback`, which is Skia's CPU raster into the window surface and a later phase. Nothing selects any of this yet.
- Skiko's GL entry point, `DirectContext.makeGL`, looks for a GLX context and fails on this EGL one. Phase 4 builds
  the context with `makeGLWithInterface` instead (`SkiaGlContext`), handing Skia an upcall (`SkiaGlProcs`) that
  answers each GL function from `SDL_GL_GetProcAddress`, so no native library of ours is involved. Skiko's natives
  still need `libGL.so.1` and `libX11` installed.
- `SkiaGlPresenter` (phase 4, in `ui/skia`) draws a GL window's frames straight onto the surface, with nothing selecting
  it yet. The renderer draws onto a persistent offscreen GPU `Surface` (`SkiaGlTargets`, `SkiaGlFrames`), so it can
  draw only what changed; a present draws that whole surface onto the window's framebuffer (a swap leaves the back
  buffer undefined), fills the carets over it, and swaps. Everything runs on the window's SDL thread in the order the
  calls were made (`SdlGlWindow.post`); drawing is never skipped, and a run of present requests that has not started
  is one present. A resize or a scale change makes new surfaces, which is the renderer's cue for a cold frame. The
  scroll move (`copyPixels`) does not yet work on a GPU surface, because `PixelMove` needs CPU pixels.
- `SkiaGlFrameBenchmarks` times the real renderer on that thread over the GL surface and over Skia's CPU raster surface
  (typing, idle and cold frames at 2x). It is not part of the perf gate: `sbt 'set app / Test / javaOptions +=
  "-Dserenity.sdl3.library.path=<directory>"' "app / Test / runMain com.serenity.perf.SkiaGlFrameBenchmarks"`; on the
  `offscreen` driver the GL is Mesa's software rasteriser, so the numbers are not a GPU's.

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

Passing `-Dawt.toolkit.name=...` yourself always wins over both. Only a flag on the JVM's command line counts: the
JetBrains Runtime presets the property to `XToolkit` itself, and that preset is ignored. The choice has to be made before any AWT class loads,
which is why it is an environment variable rather than a config setting. Under the native toolkit, `window.chrome = auto`
uses the compositor's decorations, since Wayland does not let an application move its own window from a custom title bar.

JBR's Vulkan renderer is opt-in and experimental; try it with `-Dsun.java2d.vulkan=true`. The AppCDS flags above work
on JBR too, but keep a separate archive per runtime or each switch will rewrite it.

### Renderer selection and Wayland

Serenity paints with Skia (Skiko). Each setting below can be given as a launch flag,
an environment variable or a config key; the flag wins over the variable, which wins over the file, which wins over the
default. An unrecognised flag value, or an unrecognised `SERENITY_RENDER_BACKEND`, `SERENITY_WAYLAND_RENDER` or
`SERENITY_TOOLKIT`, stops the launch with a message naming the variable, the value and the choices. An unrecognised
config value is reported by the usual config warning (log and start page) with the same choices, and the setting uses
its default. `java2d` is not a choice any more: a config file written before config version 3 that still says
`java2d` for either setting is read as that setting's default (`auto` for the renderer, `skia-raster` for the Wayland
render), with a migration note, and anywhere else it is an unrecognised value.

| Config key | Values | Default | Environment variable | Flag |
| --- | --- | --- | --- | --- |
| `ui.render.backend` | `auto`, `skia`, `skia-raster` | `auto` | `SERENITY_RENDER_BACKEND` | `--renderer=<value>` |
| `ui.render.wayland` | `skia-raster`, `xwayland` | `skia-raster` | `SERENITY_WAYLAND_RENDER` | `--wayland-render=<value>` |
| `ui.render.skia_vsync` | `true`, `false` | `false` | `SKIKO_VSYNC_ENABLED` (Skiko's own) | `-Dskiko.vsync.enabled=<value>` (Skiko's own) |

`ui.render.skia_vsync` makes Skia's GPU layer wait for the display's vertical blank before presenting. It is off by default: on
Hyprland the owner saw no tearing with it off and a frame is 4-8 ms quicker, and compositors on Windows, macOS and most
Linux desktops do not tear either. Turn it on if you see tearing, for example on X11 without a compositor. Serenity sets
Skiko's `skiko.vsync.enabled` property from the key at startup, so it needs a restart, and `SKIKO_VSYNC_ENABLED` or
`-Dskiko.vsync.enabled`, when given, win. The Skia CPU raster and Java2D ignore it.

`auto` uses Skia's GPU layer wherever the native library loads: Direct3D on Windows, Metal on macOS, OpenGL on Linux with
an X11 or XWayland display (Skiko falls back to its own software redrawer where the GPU API cannot start). If the layer
itself cannot be made at run time (no GL context, a native failure) the window paints with the Skia CPU raster, and with
Java2D only if that cannot make a surface either. If the native library does not load before the window opens, a launch
from a terminal runs the terminal interface instead, with a start-page notice and a `[RENDER]` log line that say why. A
launch with no terminal (from a desktop launcher), or one that asked for the window (`--gui`, `--smoke-test`,
`--startup-probe`), stops with an error dialog and a non-zero exit instead. `skia` and `auto` differ only in the reason
logged; `skia-raster` forces the Skia CPU raster on any display and toolkit (it still needs the native library, and
takes the same fallback, saying why, if the library does not load). The raster rasterises off the event thread and the event thread only copies the image to
the window, as Java2D does, so it is the way to compare against the GPU layer when that one keeps the event thread busy
(#2489); `ui.render.wayland` is then ignored. Terminal launches never load Skia; safe mode loads it like any
window launch, with the default render settings. The startup log records the outcome as
`[RENDER] backend=<skia|skia-raster|terminal> reason=...`. A
windowed launch loads the native library to find out, which took about 0.33 s on the machine this was measured on.

The layer is handed each frame as an image by default: the frame is painted on a Skia CPU surface off the event thread,
which keeps its pixels between frames so a typing frame repaints only the rows it changed, and the layer only draws the
resulting image. `ui.render.skia_frames = image|picture` sets this in the config file (read at startup, needs a restart); `-Dserenity.skia.layerFrames` beats it. With `picture` it is handed the frame as a recorded Skia `Picture`
instead, which it replays on the event thread; any other value keeps images. Pictures were the default until a timing
run on the reference laptop (#2618: key-to-pixels p50 about 32 ms for images against about 133 ms for pictures, the
event thread at 2% CPU against 20%) said otherwise. It is a property, not a setting.

Skia's window layer needs an X11 display, so on a Linux session where the native Wayland toolkit (`WLToolkit`) would be
used, or where there is no X11 display, `ui.render.wayland` decides what happens:

| Value | Effect |
| --- | --- |
| `skia-raster` (default) | Keep `WLToolkit`; Skia paints on the CPU into an image that Java2D presents. Falls back to the terminal if the native library does not load |
| `xwayland` | Switch to the X11 toolkit through XWayland so Skia can use the GPU. An explicit `SERENITY_TOOLKIT` or `-Dawt.toolkit.name` still wins, and the Skia raster is used instead |

The renderer and the toolkit are settled together before any AWT class loads, so a config change needs a restart.

#### Skia on NixOS

Skiko's native library links against system libraries, and NixOS has no global library directory, so the dynamic loader
cannot find them. The native probe then fails and Serenity runs in the terminal. The start page says so, naming the
missing library, and the log carries the failure:
`[RENDER] backend=terminal reason=ui.render.backend=skia could not load: LibraryLoadException: Failed to loade library .../libskiko-linux-x64.so`
(Skiko's own spelling). The log line names only Skiko's file; the library that is actually missing is in the
`UnsatisfiedLinkError` that Skiko wraps, which the start-page notice reads.

The loader reads `LD_LIBRARY_PATH` when the process starts, so it has to be set before the JVM launches; the app cannot
set it for itself. `ldd` on the extracted native (Skiko unpacks it into `~/.skiko/` on first load) lists what is missing:

```bash
ldd ~/.skiko/*/libskiko-linux-x64.so | grep 'not found'
```

On the Intel laptop (NixOS, Hyprland, Wayland session, #2570) it listed `libGL.so.1`, `libX11.so.6`,
`libfontconfig.so.1` and `libstdc++.so.6`. With these directories on `LD_LIBRARY_PATH` the same jar chose Skia and the
layer reported `renderApi=OPENGL`:

| Directory | Package (nixpkgs) | Provides |
| --- | --- | --- |
| `/run/opengl-driver/lib` | the system graphics driver (`hardware.graphics`) | `libGL.so.1` |
| `libx11`, `libxext`, `libxrender`, `libxtst`, `libxi` | same names | `libX11.so.6` and the X11 libraries |
| `libglvnd` | `libglvnd` | the GL dispatch libraries |
| `fontconfig.lib` | `fontconfig` (`lib` output) | `libfontconfig.so.1` |
| `stdenv.cc.cc.lib` | the compiler's runtime (`gcc` `lib` output) | `libstdc++.so.6` |

Caution, here be imagine dragons: that set is confirmed to work together, but `ldd` flagged only the four libraries
above. Which of `libxext`, `libxrender`, `libxtst`, `libxi` and `libglvnd` are needed on their own was not tested.

**A launcher script (recommended; no root).** Put a small script on your `PATH` that builds the library directory from
nixpkgs each time it starts, then launches the jar. This is what the laptop does, with `SERENITY_JAR` overriding the
default jar; there, a launch through it logs `backend=skia`, `native probe passed` and `renderApi=OPENGL`, and the cached
library-path computation costs about 1.4 s per launch. A sketch of the pattern (not run as written; `<nixpkgs>` has to
resolve on your system, otherwise name your pinned nixpkgs):

```bash
#!/usr/bin/env bash
set -euo pipefail
libs=$(nix-build --no-out-link -E '
  with import <nixpkgs> {};
  symlinkJoin {
    name = "serenity-skia-libs";
    paths = [ libx11 libxext libxrender libxtst libxi libglvnd fontconfig.lib stdenv.cc.cc.lib ];
  }')
export LD_LIBRARY_PATH="/run/opengl-driver/lib:$libs/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
exec java --enable-native-access=ALL-UNNAMED -jar "${SERENITY_JAR:-target/scala-3.9.0/Serenity.jar}" "$@"
```

Build the directory at launch rather than writing store paths into the script. A store path is not a garbage-collection
root, so `nix-collect-garbage` removes it and the script then breaks; rebuilt on demand, the first launch after a
collection pays the rebuild and later ones hit the cache. To make the result survive collections too, give `nix-build`
an `--out-link` in place of `--no-out-link`: that symlink is a GC root, at the price of a link to look after.

**Without a script.** The same libraries can be put on the path for one command, which needs no files but holds the same
store paths only for as long as `nix-shell` keeps them. This line is assembled from the table and has not been run as
written:

```bash
nix-shell -p libx11 libxext libxrender libxtst libxi libglvnd fontconfig.lib stdenv.cc.cc.lib \
  --run 'export LD_LIBRARY_PATH="/run/opengl-driver/lib:$(printf "%s/lib:" $buildInputs)$LD_LIBRARY_PATH"; java -jar target/scala-3.9.0/Serenity.jar'
```

Skia loaded with these libraries on Java 21.0.12 (the diagnostic, #2570) and on Java 25.0.4.1 (the benchmark runs,
#2489), both from nixpkgs; the JDK this document expects is Temurin 25 (see "Java version and launch options"). On
Wayland, the JetBrains Runtime's native toolkit needs its own libraries on the same path (#2479).

#### Comparing the renderers

`serenity --render-benchmark` opens a window of its own for each renderer (Java2D, the Skia layer, then Skia's CPU
raster, which paints with Skia and presents through Java2D), draws the same built-in document through the same scripted
actions on each, prints the tables and saves them as
`render-benchmark-<timestamp>.md` and `.json` in the working directory, then quits without starting the editor. The
scenarios are a cold full frame, a warm full frame (the same state redrawn whole with warm caches), typing one
character, scrolling one line, scrolling five lines, a page down, a caret blink and the first frame after a window resize
(the window alternates between its size and three quarters of it; the toolkit's own resize is outside the timing). The
warm frame and the resize are reported but take no part in the verdict. The report also gives the scale the Skia layer
drew at beside Java2D's. A frame is timed from the moment the renderer is called to the last time the canvas painted (Java2D, raster)
or the layer drew the frame's published picture (Skia layer). Skiko draws the layer's picture on the GPU afterwards, on
its own thread, and gives the layer no GPU context to wait on, so the layer's figures cover recording the frame and
Skiko's frame pacing (with vsync on, which is now opt-in, a draw can wait up to a refresh), not the GPU finishing. Options: `--render-benchmark-size WIDTHxHEIGHT` (default
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
the three per-OS jars (`Serenity-<os>-<arch>.jar`), the three app-image zips, `Serenity.cdx.json` (the SBOM, below) and `SHA256SUMS`, with that version's CHANGELOG section as the notes. A plain
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
