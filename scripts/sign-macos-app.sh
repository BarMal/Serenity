#!/usr/bin/env bash
# Signs a macOS app image inside-out: every Mach-O file first (deepest paths first), then the embedded runtime bundle,
# then the app itself. `codesign --deep` is deliberately not used: it applies one identity and one set of entitlements
# to everything and signs nested code in an order Apple documents as unreliable, which matters now that the Skiko
# dylib sits in Contents/app/natives and must carry our own signature (#2028).
#
#   scripts/sign-macos-app.sh dist/Serenity.app -                                  ad-hoc: no certificate needed
#   scripts/sign-macos-app.sh dist/Serenity.app "Developer ID Application: ..."    hardened runtime, timestamp, entitlements
set -euo pipefail

app="${1:?usage: $0 path/to/Serenity.app identity-or-dash}"
identity="${2:?usage: $0 path/to/Serenity.app identity-or-dash}"
entitlements="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/ci/macos.entitlements"

if [[ ! -d "$app" ]]; then
  echo "Not an app bundle: $app" >&2
  exit 1
fi

sign_args=(--force --sign "$identity")
if [[ "$identity" != "-" ]]; then
  sign_args+=(--options runtime --timestamp --entitlements "$entitlements")
fi

macho_files=()
while IFS= read -r -d '' candidate; do
  if file -b "$candidate" | grep -q 'Mach-O'; then
    macho_files+=("$candidate")
  fi
done < <(find "$app" -type f -print0)

# A path with more components is nested deeper, so sorting by slash count puts contained code before its container.
sorted="$(for f in "${macho_files[@]}"; do
  slashes="${f//[^\/]/}"
  printf '%s\t%s\n' "${#slashes}" "$f"
done | sort -t$'\t' -k1,1nr -k2,2 | cut -f2-)"

while IFS= read -r binary; do
  [[ -n "$binary" ]] || continue
  codesign "${sign_args[@]}" "$binary"
done <<<"$sorted"

runtime="$app/Contents/runtime"
if [[ -d "$runtime" ]]; then
  codesign "${sign_args[@]}" "$runtime"
fi
codesign "${sign_args[@]}" "$app"

codesign --verify --deep --strict --verbose=2 "$app"
echo "Signed ${#macho_files[@]} Mach-O files and the bundle with identity '$identity'."
