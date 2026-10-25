#!/usr/bin/env bash
# Checks the natives a runner group built (#2028): exactly the expected jars exist, none carries the DNG SDK, and the scan is
# proved on stock libraries first, so a scanner that has stopped seeing DNG cannot pass a build.
#
#   ci/skiko/scan-natives.sh <linux-x64|linux-arm64|macos|windows> <repository dir>
set -euo pipefail

group="${1:?runner group}"
repo="${2:?repository dir}"
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
# shellcheck source=pins.env
. "$here/pins.env"
# shellcheck source=natives-version.env
. "$here/natives-version.env"

case "$group" in
  linux-x64)   targets=(linux-x64);            control=(linux-x64) ;;
  linux-arm64) targets=(linux-arm64);          control=(linux-arm64) ;;
  macos)       targets=(macos-x64 macos-arm64); control=(macos-x64 macos-arm64) ;;
  # The stock Windows natives are clean (Skia builds its RAW codec only where PIEX is, and PIEX is off on Windows), so they
  # cannot prove the scanner; the stock Linux library does, on this runner and this Python.
  windows)     targets=(windows-x64 windows-arm64); control=(linux-x64) ;;
  *) echo "unknown runner group: $group" >&2; exit 2 ;;
esac

found=()
expected=()
for target in "${targets[@]}"; do
  name="skiko-awt-runtime-$target"
  expected+=("$name-$NATIVES_VERSION.jar")
done
while IFS= read -r jar; do found+=("$(basename "$jar")"); done < <(find "$repo" -name "skiko-awt-runtime-*-$NATIVES_VERSION.jar" | sort)
if [ "$(printf '%s\n' "${found[@]:-}" | sort | tr '\n' ' ')" != "$(printf '%s\n' "${expected[@]}" | sort | tr '\n' ' ')" ]; then
  echo "expected jars: ${expected[*]}" >&2
  echo "found jars:    ${found[*]:-none}" >&2
  exit 1
fi

stock="$(mktemp -d)"
controls=()
for target in "${control[@]}"; do
  file="skiko-awt-runtime-$target-$SKIKO_VERSION.jar"
  curl --fail --silent --show-error --location --retry 8 --retry-all-errors --retry-delay 20 \
    -o "$stock/$file" \
    "https://repo1.maven.org/maven2/org/jetbrains/skiko/skiko-awt-runtime-$target/$SKIKO_VERSION/$file"
  controls+=("$stock/$file")
done

echo "::group::Positive control: the scanner must see DNG in the stock libraries"
python3 "$root/scripts/check_no_dng.py" --expect-dng "${controls[@]}"
echo "::endgroup::"

echo "::group::The libraries built here must have none"
jars=()
while IFS= read -r jar; do jars+=("$jar"); done < <(find "$repo" -name "skiko-awt-runtime-*-$NATIVES_VERSION.jar" | sort)
python3 "$root/scripts/check_no_dng.py" "${jars[@]}"
echo "::endgroup::"
