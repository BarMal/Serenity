#!/usr/bin/env bash
# Builds the DNG-free Skiko natives for one runner group and leaves them as a Maven repository tree (#2028).
#
#   ci/skiko/build-natives.sh <linux-x64|linux-arm64|macos|windows> <work dir> <output repository dir>
#
# macos and windows build both architectures on the one runner, as upstream's own Skia workflow does. The steps follow
# JetBrains/skia's .github/workflows/build_for_skiko.yml at SKIA_TAG, with ci/skiko/*.patch applied; see pins.env.
set -euo pipefail

group="${1:?runner group}"
work="${2:?work dir}"
out="${3:?output repository dir}"
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

if [ "$os" = linux ] && [ "$(id -u)" = 0 ]; then
  apt-get update -y
  apt-get install -y git curl ca-certificates unzip zip g++ openjdk-17-jdk-headless
fi

echo "::group::Fetch Skia $SKIA_TAG and Skiko $SKIKO_TAG"
git clone --quiet --depth 1 --branch "$SKIA_TAG" https://github.com/JetBrains/skia "$skia"
git clone --quiet --depth 1 --branch "$SKIKO_TAG" https://github.com/JetBrains/skiko "$skiko"
echo "::endgroup::"

echo "::group::Apply the patches that leave the DNG SDK out"
git -C "$skia" apply --verbose "$here/skia-no-dng.patch"
git -C "$skiko" apply --verbose "$here/skiko-no-dng.patch" "$here/skiko-coordinates.patch"
echo "::endgroup::"

if [ "$group" = linux-x64 ]; then
  (cd "$skia" && ./tools/skia_release/prepare_linux.sh)
elif [ "$group" = linux-arm64 ]; then
  (cd "$skia" && ./tools/skia_release/prepare_linux_arm.sh)
fi

for machine in "${machines[@]}"; do
  echo "::group::Build Skia for $os-$machine"
  flags=(--gpu-as-extension --enable-graphite)
  if [ "$os" != linux ]; then flags+=(--enable-graphite-dawn); fi
  (cd "$skia" && PATH="/usr/lib/binutils-2.26/bin:$PATH" python3 tools/skia_release/build.py \
    --skia-dir . --build-type Release --target "$os" --machine "$machine" "${flags[@]}")
  echo "::endgroup::"
done

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

cp -R "$skiko/build/repo/." "$out/"
find "$out" -type f | sort
