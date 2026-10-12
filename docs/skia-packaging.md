# Packaging Skia (Skiko) natives for 1.0

Status: investigation for #2028 (owner decision: 1.0 ships on Skia). Nothing here changes the renderer or the build.
Statements marked "Caution, here be imagine dragons:" are inference, not something read in a source or measured.

## Summary

1. Skiko ships one native library per OS/arch inside a Maven jar. Version used by the #1812 spike: **0.150.1** [1]. Central has no 0.150.2 (404); its latest is 0.153.0 [2].
2. At runtime Skiko extracts the library to `~/.skiko/` unless `skiko.library.path` points at a directory [3]. That default is wrong for a signed macOS app (section 2).
3. Our notarisation will almost certainly reject natives hidden inside `Serenity.jar`; natives must be unpacked at build time, signed with our identity and loaded from the bundle.
4. Linux is X11/GLX only: Skiko needs `XToolkit` (XWayland on Wayland). `ToolkitSelection` currently prefers `WLToolkit` on a JetBrains Runtime and must be overridden [4].
5. Added size is 10 to 18 MB compressed per OS, about 63 MB if one universal jar carries every target. Fall back to Java2D when loading fails.

## 1. Artifacts

Coordinates: group `org.jetbrains.skiko`, version 0.150.1. `skiko-awt` is the Kotlin/JVM API (1,261,676 bytes, 705 classes, no natives). Each `skiko-awt-runtime-<os>-<arch>` jar depends on `skiko-awt [0.150.1]` and holds the natives [5]. Sizes below are the jar sizes read from Central and the jar contents (extracted and inspected in a scratch directory).

| Target | Artifact | Jar bytes | Contents (uncompressed) |
| --- | --- | --- | --- |
| Linux x64 | `skiko-awt-runtime-linux-x64` | 12,346,293 | `libskiko-linux-x64.so` 29.6 MB + `.sha256` |
| Linux arm64 | `skiko-awt-runtime-linux-arm64` | 12,014,904 | `libskiko-linux-arm64.so` 28.6 MB + `.sha256` |
| macOS arm64 | `skiko-awt-runtime-macos-arm64` | 18,311,913 | **both** `libskiko-macos-arm64.dylib` 21.4 MB and `libskiko-macos-x64.dylib` 22.2 MB |
| macOS x64 | `skiko-awt-runtime-macos-x64` | 18,311,912 | the same two dylibs (identical contents) |
| Windows x64 | `skiko-awt-runtime-windows-x64` | 10,623,998 | `skiko-windows-x64.dll` 14.1 MB + `icudtl.dat` 10.5 MB |
| Windows arm64 | `skiko-awt-runtime-windows-arm64` | 9,785,410 | `skiko-windows-arm64.dll` 11.7 MB + `icudtl.dat` 10.5 MB |

Findings from the jars:
- The two macOS jars are the same payload, so either serves both CPUs. An arm64-only app image still carries the 22 MB x64 dylib unless we strip it.
- Only Windows has a separate `icudtl.dat` (Skiko's `Library` references it [6]). Linux and macOS libraries embed ICU data (2,242 `icudt` string hits in the Linux library). Caution, here be imagine dragons: ICU is therefore statically linked on Linux/macOS.
- Every native has a `.sha256` sibling resource that the loader requires [3].

Transitive runtime dependencies of `skiko-awt` (from its POM [5]): `kotlin-stdlib` 2.3.20 (1,804,720 B), `kotlinx-coroutines-core-jvm` 1.8.0 (1,548,360 B), `org.jetbrains.runtime:jbr-api` 1.5.0 (runtime scope; size not measured, Central rate-limited the HEAD), and `org.jetbrains:annotations` 13.0 (17,536 B) via Kotlin. These are Kotlin-compiled JVM jars; they add no build-tool requirement to an sbt project.

Targets Serenity ships today: `linux-x64`, `windows-x64`, `macos-arm64` only (matrix in `desktop-publish.yml` [7]). Linux arm64, macOS x64 and Windows arm64 have no CI job; adding any is separate runner work. Windows arm64 has no OpenGL in Skiko (ANGLE, Direct3D, then software) [8].

How the library loads [3][8]:
1. `skiko.library.path` set: load `<dir>/<platform file name>`.
2. Else a matching file under `java.home` (`bin` on Windows, `lib` elsewhere).
3. Else extract the jar resource to `${skiko.data.path}/<name>-<sha256>/` (default `~/.skiko/`), write via temp file plus atomic move under a lock, then `System.load`. Directories unused for 31 days are purged (`skiko.data.cleanup.days`). It never consults `java.library.path`.
4. A "already loaded in another classloader" error is handled by copying to a temp dir.

Render API choice is `SKIKO_RENDER_API` (env) or `skiko.renderApi`. Defaults: macOS Metal, Linux OpenGL, Windows Direct3D. Fallback queues: Linux OpenGL, `SOFTWARE_FAST`, `SOFTWARE_COMPAT`; macOS Metal, `SOFTWARE_COMPAT`; Windows ANGLE, Direct3D, OpenGL, software [8].

Compose Desktop uses the same artifacts. Its macOS guide says natives loaded from JVM code "must be loaded directly from the app bundle" and cannot be extracted from a jar first because of signing and the sandbox [9]. Caution, here be imagine dragons: Compose's Gradle plugin points `skiko.library.path` at the bundled copy (I did not verify this). We copy the idea by hand because we use jpackage and sbt, not that plugin.

## 2. Signing and notarisation

### macOS

Current workflow [7]: ad-hoc `codesign --force --deep --sign -` when no certificate secret exists (line 317); otherwise `codesign --force --deep --options runtime --timestamp` (line 332), `--verify --deep --strict` (333), `notarytool submit` of a ditto zip (379), `stapler staple`/`validate` (384-385). There is no entitlements file in the repository and the signing command passes none.

What changes with Skiko:

1. **Library validation.** Hardened runtime refuses `dlopen` of a library signed by a different Team ID unless `com.apple.security.cs.disable-library-validation` is granted. The dylib in the Skiko jar has a code signature (an `LC_CODE_SIGNATURE` load command with an Apple Developer ID certificate chain; I did not decode the signer, since there is no `codesign` on this machine). Extracted to `~/.skiko/` it is outside our bundle and has JetBrains' signature, not ours. Caution, here be imagine dragons: loading it from there under our hardened runtime fails without the entitlement. Two options:
   - **Preferred:** extract the dylib at build time into `Serenity.app/Contents/app/natives/`, re-sign it with our identity (`--options runtime --timestamp`) and launch with `-Dskiko.library.path=$APPDIR/natives` (jpackage already expands `$APPDIR` in `--java-options`; `serenity.jsa` uses it [7]). The loader then never writes to `~/.skiko` on macOS [3]. No library-validation exemption.
   - Fallback: grant `disable-library-validation` and keep runtime extraction. Weaker (any dylib can be injected) and Compose's own sample lists the entitlement as needed for TestFlight [9], so it is not Skiko-specific evidence.
2. **Natives inside jars fail notarisation.** Caution, here be imagine dragons: Apple's notary service opens jars and zips and inspects Mach-O files inside; my recollection of common failures is "binary is not signed with a valid Developer ID certificate / does not have the hardened runtime enabled" for dylibs in jars. Not tested here. Serenity.jar would contain both Skiko dylibs, and already contains the JNA and JLine macOS natives. Mitigation: build the macOS app image from a jar with all natives removed (`assemblyExcludedJars` or a merge-strategy discard) and put each needed dylib on disk, signed, in the bundle. JNA/JLine need the same treatment (separate issue; the existing job may already be exposed to this).
3. **Replace `--deep`** (also asked in #2018 [10]). Sign inside-out: every Mach-O under `Contents` (find by `file`, not by extension), then the runtime's `libjli`/`bin/*`, then the launcher, then the bundle:
   ```
   find dist/Serenity.app -type f -exec sh -c 'file -b "$1" | grep -q Mach-O' _ {} \; -print |
     sort -r | while read -r f; do codesign --force --options runtime --timestamp --entitlements ci/macos.entitlements --sign "$IDENTITY" "$f"; done
   codesign --force --options runtime --timestamp --entitlements ci/macos.entitlements --sign "$IDENTITY" dist/Serenity.app
   codesign --verify --strict --verbose=4 dist/Serenity.app
   ```
   (sketch, not run). After stapling: `spctl -a -vv dist/Serenity.app` and `codesign -dvv` on the dylib must show our Team ID.
4. **Entitlements.** A new `ci/macos.entitlements` with `com.apple.security.cs.allow-jit` and `com.apple.security.cs.allow-unsigned-executable-memory`; add `disable-library-validation` only if a CI run proves it is needed (JNA/JLine extract natives to a temp directory). Caution, here be imagine dragons: HotSpot on Apple Silicon maps JIT memory with `MAP_JIT`, which needs `allow-jit` under hardened runtime. The current signing passes no entitlements, and the ad-hoc path (no `--options runtime`) is what CI exercises when secrets are absent, so the hardened path may already be unproven independently of Skia. Test it first (step A in section 5).
5. Notarisation itself is unchanged: submit the zip, staple the app [9].

Metal is the default API; OpenGL is off on macOS unless `skiko.macos.opengl.enabled=true` [8]. The dylib links AppKit, Metal, QuartzCore, CoreText and OpenGL system frameworks (string table), all in the OS.

### Windows

- `Serenity.exe` and the launcher DLLs need Authenticode regardless of Skia (#2018 [10]). Windows does not enforce library validation, so an unsigned `skiko-windows-x64.dll` loads; SmartScreen reputation attaches to the exe.
- Recommended anyway: unpack `skiko-windows-x64.dll` and `icudtl.dat` into `app/natives/` at build time, sign the DLL with our certificate, and set `-Dskiko.library.path=$APPDIR\natives`. Caution, here be imagine dragons: antivirus products flag unsigned DLLs freshly written to `%USERPROFILE%\.skiko`; a signed, installed DLL avoids that. Not tested.
- Caution, here be imagine dragons: the DLL needs the Visual C++ runtime. I did not read its import table. Check with `dumpbin /dependents` in CI; jpackage's runtime ships `vcruntime140.dll` beside `java.dll`, which may cover it.
- Windows arm64 is a Skiko target but not a Serenity one today.

### Linux

- Nothing to sign (checksums and an optional detached signature are #2018's scope). Natives need no extraction at all if shipped in `app/natives/` with `skiko.library.path`; otherwise they land in `~/.skiko`.
- `libskiko-linux-x64.so` has `NEEDED` entries for `libGL.so.1`, `libX11.so.6`, `libfontconfig.so.1` and `libstdc++.so.6`, and requires GLIBC 2.17 and GLIBCXX 3.4.22 (read with `readelf`/`objdump`). It also `dlopen`s `libjawt` from the runtime's `lib/` (string `%s/lib/libjawt%s`), so the jlinked runtime must include `java.desktop`. A system without `libGL.so.1` fails with `UnsatisfiedLinkError` before any fallback queue runs: that is the Java2D fallback case (section 4).
- **X11 only.** #1812 records that `SkiaLayer` is X11/GLX only [11]; the spike ran with `XToolkit`, `force_zero_scaling=true` and 2.0x content scale on XWayland [12]. A community report and a fork that draws EGL into Wayland surfaces both say it cannot work without a patched JBR [13]. Consequences:
  - `ToolkitSelection` (`src/main/scala/com/serenity/app/ToolkitSelection.scala`) selects `WLToolkit` automatically on JBR in a Wayland session [4]. With the Skia backend it must choose `XToolkit` (or Skia must be disabled when `WLToolkit` is forced).
  - Native Wayland will wait for a later host (#1812 names JetBrains' kotlin-desktop-toolkit) [11].
  - XWayland at fractional scale blurs; the spike needed the compositor setting `force_zero_scaling` (Hyprland) to get crisp 2x text [12]. GNOME/KDE equivalents are untested.
  - Software fallback: `SOFTWARE_FAST` (CPU raster into the layer) then `SOFTWARE_COMPAT`, via `SKIKO_RENDER_API=SOFTWARE_FAST` or the default queue [8]. Spike numbers for it were from a run the author later declared invalid (window scene at the wrong size) [12], so there is no trustworthy software-mode figure yet; measure in CI (section 5, job D).

## 3. Notices (#2019, now GPL-3.0-or-later)

`THIRD-PARTY-NOTICES.md` is generated from the resolved runtime classpath and the registry in `third-party/`; a module without a row makes rendering fail and CI red [14]. Adding Skiko to the classpath therefore forces these rows:

`third-party/modules.tsv` (columns: key, licence, text ids, copyright, url), one row per resolved module:

| Module | Licence | Texts | Copyright |
| --- | --- | --- | --- |
| `org.jetbrains.skiko:skiko-awt` | Apache-2.0 | Apache-2.0, NOTICE-skiko | JetBrains s.r.o. and Skiko contributors |
| `org.jetbrains.skiko:skiko-awt-runtime-<os>-<arch>` (each one on the classpath) | Apache-2.0 for Skiko; see assets for the embedded libraries | Apache-2.0, NOTICE-skiko | same |
| `org.jetbrains.kotlin:kotlin-stdlib` | Apache-2.0 | Apache-2.0 | JetBrains s.r.o. and Kotlin contributors |
| `org.jetbrains.kotlinx:kotlinx-coroutines-core-jvm` | Apache-2.0 | Apache-2.0 | JetBrains s.r.o. and Kotlin contributors |
| `org.jetbrains:annotations` | Apache-2.0 | Apache-2.0 | JetBrains s.r.o. |
| `org.jetbrains.runtime:jbr-api` | Apache-2.0 (its POM says so [5]) | Apache-2.0 | JetBrains s.r.o. |

`third-party/assets.tsv` (name, version, licence, text files, copyright, url), for code compiled into the native library. Strings in the Linux binary confirm Skia, HarfBuzz (`hb_shape`, `SkShaper_harfbuzz.cpp`), FreeType (`FT_Init_FreeType`), expat (`XML_Parse`), libpng, libjpeg, WebP, Wuffs, skcms, ICU (`ucnv_open`, `icudt`) and zlib/inflate:

| Component | Licence | Text to bundle |
| --- | --- | --- |
| Skia (version of Skiko's Skia milestone, read from the release notes) | BSD-3-Clause | `BSD-3-Clause-skia.txt` ("Copyright (c) 2011 Google Inc. All rights reserved.") |
| Skiko 0.150.1 native library | Apache-2.0 | `Apache-2.0` and Skiko's NOTICE if it has one |
| HarfBuzz | MIT ("Old MIT") | text from HarfBuzz `COPYING` |
| FreeType | FreeType Licence (FTL) selected from FTL OR GPL-2.0-only | `FTL.txt` |
| ICU (C++, the data is also separate on Windows as `icudtl.dat`) | Unicode-3.0 | existing `Unicode-3.0-icu4j.txt` covers the text; add the ICU4C copyright line |
| libpng, zlib, expat, libjpeg-turbo, libwebp, Wuffs, skcms | libpng-2.0, Zlib, MIT, IJG/BSD-3/Zlib, BSD-3-Clause, Apache-2.0, BSD-3-Clause | one text each |

Caution, here be imagine dragons: the licence of each embedded library comes from my knowledge of Skia's `third_party` tree, not from a licence file I read in the Skiko release. The exact list, versions and texts must come from Skiko's tag (its `skia` submodule `LICENSE` and `third_party/*/LICENSE`) before the rows are committed. Do not ship the table above as-is. Apache-2.0 and the permissive licences are compatible with GPL-3.0-or-later; FreeType's FTL is compatible with GPLv3 (it is incompatible with GPLv2-only, which does not apply here).

Skiko must also be added to the SBOM path (`makeBom`) automatically (it reads the same classpath [7]); if natives are unpacked and removed from the jar, the SBOM still lists the Maven artifact, which is right.

## 4. Size, startup and the Java2D fallback

Size (compressed bytes read from Central; the app image zips compress the same files again, so treat these as upper bounds):

| Artifact | Added to the zip |
| --- | --- |
| Linux x64 image | 12.3 MB runtime + 1.3 MB skiko-awt + 3.4 MB Kotlin stdlib/coroutines = about 17 MB |
| Windows x64 image | 10.6 MB + 4.7 MB = about 15 MB (includes `icudtl.dat`) |
| macOS arm64 image | 18.3 MB + 4.7 MB = about 23 MB (13 MB less if the x64 dylib is stripped) |
| Universal `Serenity.jar` (all five natives) | about 63 MB natives + 4.7 MB = about 68 MB |

The current `Serenity.jar` size was not measured here. Recommendation: keep the universal jar only if the owner wants a single download; otherwise the app images carry just their own OS natives and the jar documents that Skia needs the matching `skiko-awt-runtime-*` on the class path. The fat jar needs `MergeStrategy.first` or `deduplicate` for the identical `icudtl.dat` and macOS dylib entries that appear in several runtime jars (the existing `build.sbt` assembly strategy [15] has no rule for root-level natives).

Startup: the spike measured a first frame of 373 ms (OpenGL, direct pipeline, UHD 620) and 40 ms for the picture pipeline on `SOFTWARE_FAST` in the earlier invalid window run [12]. Nothing measured isolates native loading or the first-run extraction (writing about 30 MB and hashing `.sha256`, once). AppCDS does not cover Skiko classes until the archive is regenerated on exit. Measure: cold start with and without Skiko on the classpath, first launch (extraction) and second launch, using the existing `--smoke-test` path [16].

Java2D fallback (the Java2D renderer stays one release behind the `ui.render.backend` toggle in #1812's plan [11]). Per platform, fall back when:
1. **Native load fails** (`UnsatisfiedLinkError`/`LibraryLoadException`: no `libGL.so.1`/`libX11`/`libfontconfig` on Linux; missing VC++ runtime or blocked DLL on Windows; library validation or quarantine on macOS). Probe once before any window is created: call `org.jetbrains.skiko.Library.load()`, make a 1x1 raster `Surface`, and catch `Throwable` inside `IO.attempt`.
2. **Toolkit is `WLToolkit`** on Linux, or there is no X11 display: use Java2D (the X11-only rule).
3. **No usable GPU context**: let Skiko walk its own queue (OpenGL, `SOFTWARE_FAST`, `SOFTWARE_COMPAT`; macOS Metal then `SOFTWARE_COMPAT`) [8]. Caution, here be imagine dragons: I did not verify that context-creation failure inside `SkiaLayer` raises something catchable after the window exists; a probe that creates an off-screen `SkiaLayer` would confirm it.
4. Record the choice (`backend=skia/java2d reason=...`) in `~/.serenity/serenity.log` and let `ui.render.backend = auto | skia | java2d` and an environment variable override it.
Skiko classes must stay unloaded in TUI/headless modes (`LaunchOptions` already decides the frontend before AWT [4]); a lazy holder keeps the Kotlin jars off that path.

## 5. CI proof plan and owner inputs

Spike branch only (not master): a branch `spike/skia-packaging` that adds Skiko to `build.sbt` per OS and a 30-line `SkikoProbe` main that loads the library, renders a 1x1 raster surface and prints `SKIKO_PROBE_OK <renderApi>`.

| Job | Runs on | Proves |
| --- | --- | --- |
| A. Entitlements baseline | `macos-latest` (no Skiko) | Today's hardened-runtime signing launches the app: sign with `--options runtime` and the proposed entitlements, run `--smoke-test`. Separates the existing risk from Skiko's. Requires the Developer ID secrets, so only on the owner's fork or a tagged dry run |
| B. macOS natives | `macos-latest`, ad-hoc and Developer ID | Dylib unpacked to `Contents/app/natives`, re-signed, `-Dskiko.library.path` set; `codesign -dvv` shows our Team ID on the dylib; probe prints OK; notarisation with the natives inside the jar vs outside (confirms or refutes the jar-scanning claim); `spctl -a -vv` passes after stapling |
| C. Windows natives | `windows-latest` | DLL and `icudtl.dat` in `app/natives`, `signtool verify /pa` on exe, launcher DLLs and the Skiko DLL; probe OK; `dumpbin /dependents` records the runtime dependencies |
| D. Linux natives | `ubuntu-latest` under `xvfb-run` with `libgl1`, `libfontconfig1`, `mesa` software GL | Probe OK on OpenGL (llvmpipe) and with `SKIKO_RENDER_API=SOFTWARE_FAST`; second run with `libGL` hidden proves the Java2D fallback; `XToolkit` forced |
| E. Notices | Linux | `checkThirdPartyNotices` red without Skiko rows, green with them; app image contains the notices [7] |
| F. Fallback matrix | all three | Probe with the library deleted, with a corrupt `.sha256`, and with `SKIKO_RENDER_API=BOGUS`; Serenity starts on Java2D each time |
| G. Size and start | all three | Zip sizes per image against the table in section 4; cold and warm `--smoke-test` times |
| H. arm64 (optional) | `ubuntu-24.04-arm`, `windows-11-arm` | Linux/Windows arm64 natives load (Windows arm64 has no OpenGL [8]); only if those targets are adopted |

The owner must supply:
- **Apple Developer ID Application certificate** (`.p12` plus password), `MACOS_CODESIGN_IDENTITY`, and a keychain password: the existing secrets in `desktop-publish.yml` [7]; whether they are set is unknown (#2018 [10]).
- **Notarisation credentials**: Apple ID, an app-specific password and the Team ID (`MACOS_NOTARY_*`) [7][9].
- **Windows code-signing certificate** (OV or EV; EV gives immediate SmartScreen reputation) and a signing method for CI (a `.pfx` secret or a cloud signing service) (#2018 [10]).
- A decision on: single universal jar vs per-OS jars; which extra architectures (Linux arm64, macOS x64, Windows arm64) are in scope for 1.0; the Skiko version (0.150.1 as spiked, or the current 0.153.0, which would need the spike rerun) [2].

## Biggest risk

macOS: Skiko's pre-signed dylib, loaded from `~/.skiko` or scanned inside a jar, is likely to fail library validation and notarisation, and the current signing path passes no entitlements and has never been exercised in hardened mode with the secrets absent. Jobs A and B settle this before anything else is built. Second: Linux is X11-only, so Wayland-only systems (no XWayland) cannot use the Skia backend and need the Java2D fallback.

## Sources

[1] https://github.com/BarMal/Serenity/issues/1812#issuecomment-5982892864 and later spike comments (Skiko 0.150.1, spike d50b209c; 4-5 Oct 2026)
[2] https://repo1.maven.org/maven2/org/jetbrains/skiko/skiko-awt/maven-metadata.xml (release 0.153.0; versions 0.150.0, 0.150.1, 0.152.0-alpha02, 0.153.0) and 404 for 0.150.2
[3] https://raw.githubusercontent.com/JetBrains/skiko/master/skiko/src/jvmMain/kotlin/org/jetbrains/skiko/LibraryLoader.kt
[4] `src/main/scala/com/serenity/app/ToolkitSelection.scala`
[5] https://repo1.maven.org/maven2/org/jetbrains/skiko/skiko-awt/0.150.1/skiko-awt-0.150.1.pom and the six `skiko-awt-runtime-*` 0.150.1 jars and POMs
[6] `org/jetbrains/skiko/Library.class` in `skiko-awt-0.150.1.jar` (references `icudtl.dat`, `skiko-`)
[7] `.github/workflows/desktop-publish.yml` (matrix 28-40; app image 144-196; macOS sign 308-333; notarise 379-385)
[8] https://raw.githubusercontent.com/JetBrains/skiko/master/skiko/src/jvmMain/kotlin/org/jetbrains/skiko/SkikoProperties.kt
[9] https://raw.githubusercontent.com/JetBrains/compose-multiplatform/master/tutorials/Signing_and_notarization_on_macOS/README.md
[10] https://github.com/BarMal/Serenity/issues/2018
[11] https://github.com/BarMal/Serenity/issues/1812 (body: GPU stack; comment 4 Oct: decision and plan steps 2-12)
[12] https://github.com/BarMal/Serenity/issues/1812 (comments 4 Oct 18:07, 5 Oct 09:53 and 09:57)
[13] https://slack-chats.kotlinlang.org/t/520566/does-skiko-and-by-extension-compose-desktop-support-wayland- and https://klibs.io/project/brahmkshatriya/SkikoWayland (unofficial)
[14] `project/ThirdPartyNotices.scala`, `third-party/modules.tsv`, `third-party/assets.tsv`, `THIRD-PARTY-NOTICES.md`; https://github.com/BarMal/Serenity/issues/2019
[15] `build.sbt` lines 275-293 (`assemblyMergeStrategy`)
[16] `scripts/smoke-test-app-image.sh`
