#!/usr/bin/env bash
# Builds the DNG-free Skiko natives for one runner group and leaves them as a Maven repository tree (#2028).
#
#   ci/skiko/build-natives.sh <linux-x64|linux-arm64|macos|windows> <work dir> <output repository dir> <phase>
#
# Phases, each a step of the workflow so its time and timeout are its own:
#   fetch-skiko        clone Skiko at SKIKO_TAG and patch it
#   fetch-skia         clone Skia at SKIA_TAG and patch it (skipped when the cached Skia archives are restored)
#   skia-linux-inside  run inside ubuntu:20.04, as upstream's Skia workflow does: toolchain, build, archive
#   skia               macOS and Windows, on the runner: build and archive
#   skiko              build the Skiko runtime jars from the Skia archives (Linux: inside Skiko's own build image)
#   collect            copy the Maven tree to the output directory
#   all                every phase but skia-linux-inside, in order (Linux needs the container, so use the workflow)
#
# The Skia archives are what Skiko itself downloads from JetBrains/skia releases; building Skiko from them, and caching
# them, means a failure in the Skiko step costs minutes, not another Skia build. Steps follow JetBrains/skia's
# .github/workflows/build_for_skiko.yml at SKIA_TAG and JetBrains/skiko's publish-dry-run workflow at SKIKO_TAG, with
# ci/skiko/*.patch applied; see pins.env.
set -euo pipefail

# Nothing here may wait for input: an installer that asks a question blocks a CI job until its timeout.
export DEBIAN_FRONTEND=noninteractive

group="${1:?runner group}"
work="${2:?work dir}"
out="${3:?output repository dir}"
phase="${4:?phase}"
here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=pins.env
. "$here/pins.env"

case "$group" in
  linux-x64)   os=linux;   machines=(x64) ;;
  linux-arm64) os=linux;   machines=(arm64) ;;
  macos)       os=macos;   machines=(x64 arm64) ;;
  windows)     os=windows; machines=(x64 arm64) ;;
  *) echo "unknown runner group: $group" >&2; exit 2 ;;
esac
titled_os="$(tr '[:lower:]' '[:upper:]' <<<"${os:0:1}")${os:1}"

skia="$work/skia"
skiko="$work/skiko"
zips="$work/skia-zips"
skia_dir="$work/skia-dir"
mkdir -p "$work" "$zips"

# A container runs as root over a checkout the runner user made; git refuses that unless told it is expected.
trust_checkouts() {
  if [ "$(id -u)" = 0 ] && command -v git >/dev/null; then git config --global --add safe.directory '*'; fi
}

# The flags upstream's Skia workflow passes for this target: Dawn is built everywhere but Linux.
skia_flags() {
  flags=(--gpu-as-extension --enable-graphite)
  if [ "$os" != linux ]; then flags+=(--enable-graphite-dawn); fi
}

fetch_skiko() {
  git clone --quiet --depth 1 --branch "$SKIKO_TAG" https://github.com/JetBrains/skiko "$skiko"
  git -C "$skiko" apply --verbose "$here/skiko-no-dng.patch" "$here/skiko-coordinates.patch"
}

fetch_skia() {
  git clone --quiet --depth 1 --branch "$SKIA_TAG" https://github.com/JetBrains/skia "$skia"
  git -C "$skia" apply --verbose "$here/skia-no-dng.patch"
}

# Inside ubuntu:20.04 (the image upstream builds Skia in): the packages a bare image lacks, upstream's own prepare script,
# then the build and the archive.
skia_linux_inside() {
  apt-get update -y
  apt-get install -y --no-install-recommends tzdata git curl ca-certificates unzip zip python3
  trust_checkouts
  if [ "$group" = linux-arm64 ]; then
    (cd "$skia" && ./tools/skia_release/prepare_linux_arm.sh)
  else
    (cd "$skia" && ./tools/skia_release/prepare_linux.sh)
  fi
  build_skia
  archive_skia
}

build_skia() {
  skia_flags
  for machine in "${machines[@]}"; do
    echo "::group::Build Skia for $os-$machine"
    (cd "$skia" && PATH="/usr/lib/binutils-2.26/bin:$PATH" python3 tools/skia_release/build.py \
      --skia-dir . --build-type Release --target "$os" --machine "$machine" "${flags[@]}")
    echo "::endgroup::"
  done
}

# What Skiko downloads for a stock build: headers and the static libraries, as one zip per target.
archive_skia() {
  archive_flags=()
  if [ "$os" != linux ]; then archive_flags+=(--enable-graphite-dawn); fi
  for machine in "${machines[@]}"; do
    echo "::group::Archive Skia for $os-$machine"
    (cd "$skia" && python3 tools/skia_release/archive.py --skia-dir . --version "$SKIA_TAG" \
      --build-type Release --target "$os" --machine "$machine" "${archive_flags[@]}")
    echo "::endgroup::"
  done
  mv "$skia"/Skia-*.zip "$zips/"
  ls -l "$zips"
}

# Skiko takes the archives as one directory. On Windows its own unzip step drops the "lib" prefix some libraries carry.
unpack_skia() {
  rm -rf "$skia_dir"
  mkdir -p "$skia_dir"
  for zip in "$zips"/Skia-*.zip; do unzip -q -o "$zip" -d "$skia_dir"; done
  if [ "$os" = windows ]; then
    for lib in "$skia_dir"/out/Release-windows-*/lib*.lib; do
      [ -e "$lib" ] || continue
      mv "$lib" "$(dirname "$lib")/$(basename "$lib" | sed 's/^lib//')"
    done
  fi
}

build_skiko() {
  trust_checkouts
  unpack_skia
  for machine in "${machines[@]}"; do
    titled_machine="$(tr '[:lower:]' '[:upper:]' <<<"${machine:0:1}")${machine:1}"
    echo "::group::Build the Skiko runtime jar for $os-$machine"
    (cd "$skiko" && ./gradlew --no-daemon --stacktrace \
      -Pskia.dir="$skia_dir" \
      -Pskiko.awt.enabled=true \
      -Pskiko.arch="$machine" \
      -Pdeploy.release=true \
      -Pdeploy.version="$NATIVES_VERSION" \
      ":skiko:publishSkikoJvmRuntime${titled_os}${titled_machine}PublicationToBuildRepoRepository")
    echo "::endgroup::"
  done
}

collect() {
  mkdir -p "$out"
  cp -R "$skiko/build/repo/." "$out/"
  find "$out" -type f | sort
}

case "$phase" in
  fetch-skiko)       fetch_skiko ;;
  fetch-skia)        fetch_skia ;;
  skia-linux-inside) skia_linux_inside ;;
  skia)              build_skia; archive_skia ;;
  skiko)             build_skiko ;;
  collect)           collect ;;
  all)               fetch_skiko; fetch_skia; build_skia; archive_skia; build_skiko; collect ;;
  *) echo "unknown phase: $phase" >&2; exit 2 ;;
esac
