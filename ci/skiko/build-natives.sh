#!/usr/bin/env bash
# Builds the DNG-free Skiko natives for one runner group and leaves them as a Maven repository tree (#2028).
#
#   ci/skiko/build-natives.sh <linux-x64|linux-arm64|macos|windows> <work dir> <output repository dir> [phase]
#
# phase is one of fetch, prepare, skia, skiko, collect, or omitted to run them all in order. The workflow runs each as its
# own step, so a step's time and timeout are its own. macos and windows build both architectures on the one runner, as
# upstream's own Skia workflow does. The steps follow JetBrains/skia's .github/workflows/build_for_skiko.yml at SKIA_TAG,
# with ci/skiko/*.patch applied; see pins.env.
set -euo pipefail

# Nothing here may wait for input: an installer that asks a question blocks a CI job until its timeout.
export DEBIAN_FRONTEND=noninteractive

group="${1:?runner group}"
work="${2:?work dir}"
out="${3:?output repository dir}"
phase="${4:-all}"
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

mkdir -p "$work" "$out"
skia="$work/skia"
skiko="$work/skiko"

fetch() {
  if [ "$os" = linux ] && [ "$(id -u)" = 0 ]; then
    apt-get update -y
    apt-get install -y --no-install-recommends tzdata git curl ca-certificates unzip zip g++ openjdk-17-jdk
  fi
  git clone --quiet --depth 1 --branch "$SKIA_TAG" https://github.com/JetBrains/skia "$skia"
  git clone --quiet --depth 1 --branch "$SKIKO_TAG" https://github.com/JetBrains/skiko "$skiko"
  git -C "$skia" apply --verbose "$here/skia-no-dng.patch"
  git -C "$skiko" apply --verbose "$here/skiko-no-dng.patch" "$here/skiko-coordinates.patch"
}

prepare() {
  if [ "$group" = linux-x64 ]; then
    (cd "$skia" && ./tools/skia_release/prepare_linux.sh)
  elif [ "$group" = linux-arm64 ]; then
    (cd "$skia" && ./tools/skia_release/prepare_linux_arm.sh)
  fi
  # What Skiko's own native link needs on Linux (its DEVELOPMENT.md) and Skia's prepare script does not install: the
  # build asks pkg-config for dbus-1 and links X11, Xrandr, GL and, on arm64, EGL.
  if [ "$os" = linux ]; then
    apt-get install -y --no-install-recommends pkg-config libdbus-1-dev libx11-dev libxrandr-dev libgl1-mesa-dev libegl1-mesa-dev
  fi
}

build_skia() {
  for machine in "${machines[@]}"; do
    echo "::group::Build Skia for $os-$machine"
    flags=(--gpu-as-extension --enable-graphite)
    if [ "$os" != linux ]; then flags+=(--enable-graphite-dawn); fi
    (cd "$skia" && PATH="/usr/lib/binutils-2.26/bin:$PATH" python3 tools/skia_release/build.py \
      --skia-dir . --build-type Release --target "$os" --machine "$machine" "${flags[@]}")
    echo "::endgroup::"
  done
}

build_skiko() {
  for machine in "${machines[@]}"; do
    titled_machine="$(tr '[:lower:]' '[:upper:]' <<<"${machine:0:1}")${machine:1}"
    echo "::group::Build the Skiko runtime jar for $os-$machine"
    (cd "$skiko" && ./gradlew --no-daemon \
      -Pskia.dir="$skia" \
      -Pskiko.arch="$machine" \
      -Pdeploy.release=true \
      -Pdeploy.version="$NATIVES_VERSION" \
      ":skiko:publishSkikoJvmRuntime${titled_os}${titled_machine}PublicationToBuildRepoRepository")
    echo "::endgroup::"
  done
}

collect() {
  cp -R "$skiko/build/repo/." "$out/"
  find "$out" -type f | sort
}

case "$phase" in
  fetch)   fetch ;;
  prepare) prepare ;;
  skia)    build_skia ;;
  skiko)   build_skiko ;;
  collect) collect ;;
  all)     fetch; prepare; build_skia; build_skiko; collect ;;
  *) echo "unknown phase: $phase" >&2; exit 2 ;;
esac
