# Skia natives and non-free licences

Status: research for #2028. The owner wants no non-open-source or non-free licence in the shipped natives. Nothing here changes the build.
Statements marked "Caution, here be imagine dragons:" are inference or memory, not something read in a source or measured.
Companion to [skia-packaging.md](skia-packaging.md); the licence rows are on the `ccr-157ed55f-u6khhf-skia-deps` branch (PR #2256).

## Summary

1. The Adobe DNG SDK is compiled into the Linux and macOS Skiko 0.153.0 natives. It is not in the Windows one. Its licence (Adobe's own "DNG SDK License Agreement") is not an OSI-approved licence [1][2][3].
2. The cause is a GN default, not a choice: `skia_use_dng_sdk = !is_wasm && skia_use_libjpeg_turbo_decode && skia_use_zlib`, which is true on desktop. JetBrains' build passes no override [4][5]. The code is only reachable through Skia's RAW image decoder, which Serenity does not use today [6][7].
3. A custom build is possible: build JetBrains/skia at the same tag with `skia_use_dng_sdk=false`, then build Skiko 0.153.0 against it with `SKIA_DIR`. Only linux-x64 and macos-arm64 need it; Windows has no DNG code already [8][9].
4. I found no upstream issue or discussion about DNG in JetBrains/skiko or JetBrains/skia [10].
5. Besides DNG, three licences in the natives are not OSI-approved per the SPDX list: FreeType FTL (an alternative is available), IJG and libpng-2.0. All three are permissive; only DNG has a restriction that makes it a real concern [11].

## 1. What JetBrains' prebuilt Skia is built with

Skiko 0.153.0 does not build Skia. `gradle.properties` pins `dependencies.skia=m153-e63f5d6129`, "a tag from https://github.com/JetBrains/skia/releases", and the build downloads `https://github.com/JetBrains/skia/releases/download/m153-e63f5d6129/Skia-m153-e63f5d6129-<os>-Release-<arch>.zip` [8][12]. There is no submodule.

The prebuilts are built by the workflow `.github/workflows/build_for_skiko.yml` inside JetBrains/skia, which runs `tools/skia_release/build.py` and `archive.py` [13][4]. (The older JetBrains/skia-pack repository has no m153 tag; its latest tag in the list I read was `m99-f85ab491eb-2` [14].) The tag `m153-e63f5d6129` resolves to commit `e63f5d612988fa982c5a75496c7cfc19103eae64` [15].

`build.py` assembles these GN args for every desktop target [4]:

```
is_official_build=true            (is_debug=true for Debug)
target_cpu="<machine>"
skia_use_system_expat=false
skia_use_system_libjpeg_turbo=false
skia_use_system_libpng=false
skia_use_system_libwebp=false
skia_use_system_zlib=false
skia_use_system_freetype2=false
skia_use_system_harfbuzz=false
skia_pdf_subset_harfbuzz=true
skia_use_system_icu=false
skia_enable_skottie=true
extra_cflags=[]   extra_cflags_cc=[]
extra_cflags+=["-fvisibility=default"]      (clang-cl form on Windows)
skia_icu_data_filter=...  skia_icu_data_filter_patch=...   (desktop: Skiko's trimmed ICU data)
macOS:   skia_use_fonthost_mac=true  skia_use_vulkan=true  skia_use_metal=true  extra_cflags_cc+=["-frtti"]
Linux:   skia_use_vulkan=true  extra_cflags_cc+=["-fno-exceptions","-fno-rtti","-D_GLIBCXX_USE_CXX11_ABI=0"]
Windows: skia_use_vulkan=true  skia_use_direct3d=true  extra_cflags+=["-DSK_FONT_HOST_USE_SYSTEM_SETTINGS"]  is_trivial_abi=false
all:     skia_gpu_as_extension=true  skia_enable_graphite=true  (Dawn on macOS/Windows)
```

`skia_use_dng_sdk` and `skia_use_piex` are not in that list on any desktop target, so they take the defaults in `gn/skia.gni` [5]:

```
skia_use_piex    = !is_win && !is_canvaskit
skia_use_dng_sdk = !is_wasm && skia_use_libjpeg_turbo_decode && skia_use_zlib
```

and `BUILD.gn` defines the RAW codec as [16]:

```
optional("raw") {
  enabled = skia_use_dng_sdk && skia_use_libjpeg_turbo_decode && skia_use_piex
  public_defines = [ "SK_CODEC_DECODES_RAW" ]
  deps = [ "//third_party/dng_sdk", "//third_party/libjpeg-turbo:libjpeg", "//third_party/piex" ]
  sources = [ "src/codec/SkRawCodec.cpp" ]
}
```

So `skia_use_dng_sdk` is on for Linux and macOS (PIEX is on too). On Windows PIEX is off, so the RAW codec, DNG SDK and PIEX are all left out. The shipped binaries agree [17]:

| String count in the 0.153.0 native | Linux x64 | macOS arm64 | Windows x64 |
| --- | --- | --- | --- |
| `dng_` | 1785 | 1328 | 0 |
| `piex` | 225 | 270 | 0 |

Skiko's own link lists name `piex` and `dng_sdk` as static libraries for macOS JVM and for native targets [18].

## 2. Third-party components in the natives, with licences

Built from the args above, the `skiko/build.gradle.kts` link lists [18], the licence files in the Skia tag, and strings in the three natives [17]. A licence is "OSI" if the SPDX list (v3.29.0) marks it OSI-approved, "FSF" if it marks it FSF libre [11].

| Component | In which natives | Licence | OSI / FSF | Source of the licence |
| --- | --- | --- | --- | --- |
| Skia, skcms | all | BSD-3-Clause | yes / yes | Skia tag `LICENSE` [19] |
| HarfBuzz | all | "Old MIT" | yes (MIT family) | COPYING at the DEPS commit [20] |
| ICU4C and embedded data | all | Unicode-3.0 | yes / not rated | upstream `main` LICENSE (not the pinned commit) |
| zlib | all | Zlib | yes / yes | upstream master (not the pinned commit) |
| libpng | all | libpng-2.0 | no / not rated | LICENSE at the DEPS commit |
| libjpeg-turbo | all | BSD-3-Clause, IJG, Zlib | BSD-3 and Zlib yes; IJG no, FSF libre | upstream `main` (not the pinned commit) |
| libwebp | all | BSD-3-Clause (+ patent grant file) | yes / yes | COPYING at the DEPS commit |
| Expat | all | MIT | yes / yes | COPYING at the DEPS commit |
| Wuffs | all | Apache-2.0 (or MIT) | yes / yes | LICENSE at the DEPS commit |
| ETC1 decoder | Linux, Windows (1 string hit each) | Apache-2.0 | yes / yes | `third_party/etc1/LICENSE` in the Skia tag [21] |
| FreeType | Linux only (`FT_Init_FreeType` appears only there; macOS and Windows use platform fonts) | FTL or GPL-2.0-or-later | FTL: no OSI, FSF libre; GPL-2.0-or-later: yes / yes | FTL.TXT at the DEPS commit |
| Vulkan Memory Allocator | Linux (208 `Vma` hits; 0 on macOS and Windows) | MIT | yes / yes | Caution, here be imagine dragons: from my knowledge of VMA; not read |
| PIEX | Linux, macOS | Apache-2.0 | yes / yes | google/piex `LICENSE` and `src/piex.h` header, upstream master [22] |
| **Adobe DNG SDK** | **Linux, macOS** | **Adobe "DNG SDK License Agreement"** | **no (no SPDX id)** | LineageOS mirror of the AOSP `dng_sdk` NOTICE [2] |
| D3D12 Memory Allocator | Windows (a static lib in Skiko's link list) | MIT | yes / yes | LICENSE.txt at the DEPS commit |
| PartitionAlloc, raw_ptr | Windows (a static lib in the link list, m151+) | BSD-3-Clause (Chromium) | yes / yes | Caution, here be imagine dragons: top-level Chromium LICENSE, not the partition_allocator repository |
| SPIR-V code (SPIRV-Cross or Headers) | all (`SPIRV` 186, 161, 55 hits) | Caution, here be imagine dragons: Apache-2.0 / MIT, from memory; not read |

Not built into the JVM natives, from the args and strings: Dawn, ANGLE (a separate pack), brotli/woff2 (wasm only), libgrapheme, libjxl, libavif (all off by default), Lua (dev builds only) [5].

Flags:
- **Adobe DNG SDK: not OSI-approved.** Its text grants use, copying, modification and distribution "for any purpose", but adds an indemnification clause for anyone who distributes it in a commercial product (section 5), a trademark restriction (section 6), and "all rights not granted are reserved by Adobe" (section 2) [2]. SPDX has no identifier for it [11]. A user report on the Adobe community forum says Adobe's download page no longer states any terms for the SDK, and Adobe support could not supply any [23]. That is an unofficial report.
- **FreeType FTL, IJG, libpng-2.0:** not OSI-approved per SPDX, though FTL and IJG are FSF libre [11]. FreeType is dual-licensed, so choosing the GPL-2.0-or-later option, which Serenity's GPL-3.0-or-later can use, would put it under an OSI licence; the notices row currently names FTL (the plan's choice). IJG and libpng-2.0 are permissive licences with no obvious restriction. Caution, here be imagine dragons: I did not check them against the DFSG or the owner's definition of "free"; whether they pass is the owner's decision.

## 3. Can we build Skia and Skiko without the DNG SDK?

Yes in principle. Nothing here was run.

**Skia.** Check out JetBrains/skia at the tag `m153-e63f5d6129` and run the same script with one extra arg: `skia_use_dng_sdk=false`. That alone switches off the `raw` target (all three conditions must hold), so SkRawCodec, the DNG SDK and PIEX are no longer linked [16]. Passing `skia_use_piex=false` as well is a second way to get the same result. `skia_use_dng_sdk` sits in a `declare_args()` block, so the command line can override it [5]. The workflow's own matrix shows how JetBrains builds each target [13]:

| Target | JetBrains runner | Extra setup |
| --- | --- | --- |
| linux-x64 | `ubuntu-latest` in a `container: ubuntu:20.04`, `prepare_linux.sh` (gcc-10 from the toolchain PPA, ninja, fontconfig and GL dev packages, python3.9), `PATH=/usr/lib/binutils-2.26/bin` | the older glibc is why they build in 20.04 |
| macos-arm64 | `macos-15` | Xcode on the runner |
| windows-x64 | `windows-2022` | MSVC dev command prompt, MSYS2 clang, Python 3.9 (not needed, see below) |

Windows needs no custom build: it has neither DNG nor PIEX already.

**Skiko.** `skiko.skiaDir` reads, in order, the `SKIA_DIR` environment variable, the `skia.dir` system property and the `-Pskia.dir` Gradle property, and fails if the path is not a directory [24]. With it set, Skiko skips the download and reads headers from that tree and static libraries from `<skia.dir>/out/<buildType>-<target>` (for example `out/Release-linux-x64`) [25]. Skiko's own `DEVELOPMENT.md` and `build-with-local-skia.sh` document this route [26]. `dependencies.skia` (and `dependencies.skia.<platform>`) only picks a release tag of `github.com/JetBrains/skia`, so it cannot point at our own repository [8][27].

One source change is likely needed: `skiko/build.gradle.kts` lists `staticSkiaLibs("piex", "dng_sdk")` for macOS JVM and for native targets [18]. With the RAW codec off, those two archives may not be built, and the link would fail. Caution, here be imagine dragons: I have not run the build; removing those two names for macOS is the likely patch. Linux does not list them and gets the code from the Skia archive [18][17], so no patch is expected there. A search of the Skiko tree finds no other use: the only references are those link lists and the `EncodedImageFormat.DNG` enum constant [18][28].

**Estimates.** Caution, here be imagine dragons: I did not measure any of this, and these are experience-based guesses. I could not read run times for JetBrains' workflow.
- Skia, per OS, on a hosted runner: roughly 30 to 90 minutes cold (fetching about 40 DEPS repositories, then building Ganesh, Graphite and the modules); the matrix builds Debug and Release, so we would build Release only.
- Skiko JVM natives after that: roughly 10 to 25 minutes per OS.
- Runners: 4 vCPU Linux or macOS-15 hosted runners are the same class JetBrains uses [13][29]. Disk is the limit to watch (Skia checkout plus `out/`).
- Caching: `actions/cache` for `third_party/externals` (keyed on the Skia commit), for `out/`, and for the Gradle home. The ninja outputs are reusable only if the toolchain and flags are unchanged. A tagged re-run of the same inputs could skip building entirely by keeping the natives as a release asset.

**Publishing.** Two options.
- *Plain natives.* Keep the stock `skiko-awt` 0.153.0 jar from Maven Central for the Kotlin side, and replace only the native library. The release workflow uploads `libskiko-linux-x64.so` and `libskiko-macos-arm64.dylib` (plus Windows from the stock jar) as assets; the packaging step on the packaging branch already puts them in `app/natives` and sets `-Dskiko.library.path` (the stock Windows DLL and `icudtl.dat` come from the Maven jar as now). Nothing in Serenity's dependency list changes except that the OS runtime jars are no longer consumed for Linux and macOS. Caution, here be imagine dragons: the native must be built from the unmodified v0.153.0 Skiko sources so its JNI signatures match the Maven jar. With `skiko.library.path` the `.sha256` file is not needed [30].
- *Own Maven artifacts.* `./gradlew publishToMavenLocal` builds the usual `skiko-awt-runtime-*` jars, and we would publish them to GitHub Packages. This also gives a normal dependency in `build.sbt` but adds a package registry and credentials to manage. It brings no benefit over option one for a single app.

I recommend the first option if the owner decides to build.

## 4. Does Serenity need the RAW codec?

No. As of master, `src/main` has no reference to `org.jetbrains.skia` or `makeFromEncoded`; Markdown preview images are decoded with `javax.imageio.ImageIO` (`MarkdownPreviewImageResources.scala`) [31]. Caution, here be imagine dragons: the build adds no ImageIO plugin (no WebP or TIFF reader), so, unlike the brief, I believe WebP does not decode today; the formats are whatever the JDK reads (PNG, JPEG, GIF, BMP, WBMP). I did not run it.

If the Skia surface (S3) hands encoded bytes to Skia, `SkCodec` adds `SkRawDecoder` to its decoder list when `SK_CODEC_DECODES_RAW` is defined [32], so a stock build would sniff and decode DNG and other RAW files. A build without the DNG SDK drops that decoder; PNG, JPEG and WebP use libpng, libjpeg-turbo and libwebp, which are separate [5]. Nothing in Serenity needs RAW.

## 5. Upstream

I found nothing:
- No result for "dng", "piex", "licence/license" or "RAW" among issues in JetBrains/skiko, apart from two unrelated closed issues (#887, #478) [10].
- In JetBrains/skia the only licence-related hit is the merged PR #35 "Create custom Skiko icu.dat for desktop targets", which is about ICU data [10].
- I could not search discussions (404) and the search tool is not exhaustive. Caution, here be imagine dragons: absence of results is not proof that JetBrains has never discussed it.
- Skiko's JVM binding has no DNG-specific code beyond the enum constant [28]. I found no sign that JetBrains turns DNG off for any target; the only target where it is off is Windows, and that is Skia's `!is_win` PIEX default [5].

A good upstream request would be a JetBrains/skia PR or issue asking `build.py` to pass `skia_use_dng_sdk=false` (and `skia_use_piex=false`) for desktop. I cannot file it from this environment.

## Recommendation

Do not ship the stock Linux and macOS natives if the DNG SDK licence is unacceptable. Cheapest path that meets the rule:
1. Ask JetBrains upstream to build desktop Skia without the RAW codec. It costs nothing and helps everyone, but the timing is theirs.
2. Meanwhile build `linux-x64` and `macos-arm64` natives ourselves (Skia at `m153-e63f5d6129` with `skia_use_dng_sdk=false`, then Skiko v0.153.0 with `SKIA_DIR`), publish them as release assets, and feed them through `skiko.library.path`. Windows keeps the stock DLL.
3. Until those exist, ship Java2D only on Linux and macOS (the packaged-image fallback already covers this).
4. While rebuilding, consider switching FreeType to its GPL-2.0-or-later option so the Linux native lists only OSI licences, and decide whether IJG and libpng-2.0 are acceptable.

Update `third-party/assets.tsv` and the notices in the same change: remove the "Adobe DNG SDK" and "PIEX" rows (PIEX goes with the RAW codec) when the custom natives replace the stock ones.

## Sources

[1] `third-party/assets.tsv` row "Adobe DNG SDK", branch `ccr-157ed55f-u6khhf-skia-deps`
[2] https://raw.githubusercontent.com/LineageOS/android_external_dng_sdk/lineage-21.0/NOTICE (the DNG SDK License Agreement; the text is identical to the AOSP NOTICE at the DEPS commit `dbe0a676`, which could not be reached directly)
[3] Strings count in `libskiko-linux-x64.so`, `libskiko-macos-arm64.dylib`, `skiko-windows-x64.dll` from the 0.153.0 runtime jars on Maven Central
[4] https://github.com/JetBrains/skia/blob/m153-e63f5d6129/tools/skia_release/build.py
[5] https://github.com/JetBrains/skia/blob/m153-e63f5d6129/gn/skia.gni (lines 88, 180)
[6] https://github.com/JetBrains/skia/blob/m153-e63f5d6129/src/codec/SkCodec.cpp (`SkRawDecoder` under `SK_CODEC_DECODES_RAW`)
[7] `src/main/scala/com/serenity/markdown/MarkdownPreviewImageResources.scala`
[8] https://github.com/JetBrains/skiko/blob/v0.153.0/skiko/gradle.properties (`dependencies.skia=m153-e63f5d6129`)
[9] this document, section 1 table
[10] https://github.com/JetBrains/skiko/issues?q=dng, https://github.com/JetBrains/skiko/issues?q=is%3Aissue+license+OR+licence+OR+piex+OR+%22RAW%22+OR+%22binary+size%22, https://github.com/JetBrains/skia/issues?q=license+OR+licence+OR+dng+OR+piex
[11] https://raw.githubusercontent.com/spdx/license-list-data/main/json/licenses.json (list version 3.29.0: `isOsiApproved`, `isFsfLibre`)
[12] https://github.com/JetBrains/skiko/blob/v0.153.0/skiko/buildSrc/src/main/kotlin/SkikoProjectContext.kt (lines 105-125)
[13] https://github.com/JetBrains/skia/blob/m153-e63f5d6129/.github/workflows/build_for_skiko.yml
[14] `git ls-remote --tags https://github.com/JetBrains/skia-pack` (latest tags m93 to m99)
[15] `git ls-remote https://github.com/JetBrains/skia` (`refs/tags/m153-e63f5d6129` is `e63f5d612988fa982c5a75496c7cfc19103eae64`)
[16] https://github.com/JetBrains/skia/blob/m153-e63f5d6129/BUILD.gn (`optional("raw")`, line 1589)
[17] `strings -a` over the extracted natives
[18] https://github.com/JetBrains/skiko/blob/v0.153.0/skiko/build.gradle.kts (lines 30-170)
[19] https://github.com/JetBrains/skia/blob/m153-e63f5d6129/LICENSE
[20] https://raw.githubusercontent.com/harfbuzz/harfbuzz/9cb1fee51069b206effb4736e443b038d230789d/COPYING
[21] https://github.com/JetBrains/skia/blob/m153-e63f5d6129/third_party/etc1/LICENSE and `README.google`
[22] https://raw.githubusercontent.com/google/piex/master/LICENSE and https://raw.githubusercontent.com/google/piex/master/src/piex.h (Apache-2.0, "Copyright 2015 Google Inc."; upstream master, not the pinned AOSP commit `bb217acd`)
[23] https://community.adobe.com/questions-563/adobe-dng-sdk-terms-of-use-licensing-182173
[24] https://github.com/JetBrains/skiko/blob/v0.153.0/skiko/buildSrc/src/main/kotlin/properties.kt (lines 205-225)
[25] https://github.com/JetBrains/skiko/blob/v0.153.0/skiko/buildSrc/src/main/kotlin/tasks/configuration/JvmTasksConfiguration.kt (line 185)
[26] https://github.com/JetBrains/skiko/blob/v0.153.0/DEVELOPMENT.md and `skiko/build-with-local-skia.sh`
[27] https://github.com/JetBrains/skiko/blob/v0.153.0/skiko/buildSrc/src/main/kotlin/SkikoProjectContext.kt (`skiaVersion(target)`)
[28] `git grep -i "dng|piex|rawcodec"` over the Skiko v0.153.0 tree
[29] https://github.com/JetBrains/skiko/blob/v0.153.0/.github/workflows/publish-dry-run.yml and `skiko/docker/linux-compat/Dockerfile` (Amazon Linux 2, GLIBC 2.26)
[30] https://github.com/JetBrains/skiko/blob/v0.153.0/skiko/src/jvmMain/kotlin/org/jetbrains/skiko/LibraryLoader.kt (`skiko.library.path` branch)
[31] `build.sbt`, `src/main/scala` on master (no `org.jetbrains.skia`, no ImageIO plugin)
[32] https://github.com/JetBrains/skia/blob/m153-e63f5d6129/src/codec/SkCodec.cpp (lines 73-137)
