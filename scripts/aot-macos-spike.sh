#!/usr/bin/env bash
# Answers #2522 item 1c, option A on a macOS runner: can an AOT cache trained before signing be used by the signed,
# hardened-runtime bundle, from where it was built, from /Applications and from a DMG? Writes a table to the step summary
# and exits non-zero if any case fails. Stand-in app (ci/aot-macos-spike/Hello.java), the real jpackage, codesign and hdiutil.
#   scripts/aot-macos-spike.sh WORKDIR
set -uo pipefail

work="$(cd "${1:?usage: $0 workdir}" && pwd)"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
entitlements="$here/ci/macos.entitlements"
app="$work/dist/Serenity.app"
cfg="$app/Contents/app/Serenity.cfg"
cache="$app/Contents/app/serenity.aot"
summary="${GITHUB_STEP_SUMMARY:-/dev/stdout}"
failed=0

row() { printf '| %s | %s | %s |\n' "$1" "$2" "$3" >>"$summary"; [[ "$2" == PASS ]] || failed=1; }

launch_log() { JAVA_TOOL_OPTIONS="-Xlog:aot" "$1/Contents/MacOS/Serenity" 2>&1; }

# A case passes when the cache is opened, no [error][aot] line appears and the app printed its ready line.
expect_cache_used() {
  local label="$1" bundle="$2" log
  log="$(launch_log "$bundle")"
  if grep -q 'Opened AOT cache' <<<"$log" && ! grep -Fq '[error][aot]' <<<"$log" && grep -q SPIKE_READY <<<"$log"; then
    row "$label" PASS "$(grep -m1 'Opened AOT cache' <<<"$log" | cut -c1-110)"
  else
    row "$label" FAIL "$(grep -E 'aot|SPIKE|rror' <<<"$log" | head -3 | tr '\n' ' ' | cut -c1-200)"
  fi
}

{
  echo "## macOS AOT cache spike (#2522, item 1c, option A)"
  echo "JDK: $(java -version 2>&1 | head -1)"
  echo
  echo "| Case | Result | Evidence |"
  echo "|---|---|---|"
} >>"$summary"

mkdir -p "$work/classes" "$work/input" "$work/dist"
javac -d "$work/classes" "$here/ci/aot-macos-spike/Hello.java" || exit 1
printf 'Main-Class: Hello\n' >"$work/manifest.txt"
jar cfm "$work/input/Serenity.jar" "$work/manifest.txt" -C "$work/classes" . || exit 1
jpackage --type app-image --name Serenity --input "$work/input" --main-jar Serenity.jar --main-class Hello \
  --mac-package-identifier com.barmal.serenity --dest "$work/dist" \
  --java-options '-XX:+UseG1GC' --java-options '--enable-native-access=ALL-UNNAMED' || exit 1

# The bundled runtime may carry no bin/java, and -XX:AOTCacheOutput forks one to assemble the cache, so show what the
# runtime has and record and assemble through the app launcher instead (JEP 483 two steps: record, then create).
echo "Runtime layout: $(find "$app/Contents/runtime" -maxdepth 4 \( -name 'java*' -o -name 'libjli*' \) | sed "s|$app/||" | tr '\n' ' ')"
config="$app/Contents/app/serenity.aotconf"
JAVA_TOOL_OPTIONS="-XX:AOTMode=record -XX:AOTConfiguration=$config" "$app/Contents/MacOS/Serenity" >"$work/record.log" 2>&1
JAVA_TOOL_OPTIONS="-XX:AOTMode=create -XX:AOTConfiguration=$config -XX:AOTCache=$cache" "$app/Contents/MacOS/Serenity" >"$work/create.log" 2>&1
rm -f "$config"
if [[ -s "$cache" ]]; then
  row "Train before signing (record, then create, through the app launcher)" PASS "$(du -h "$cache" | cut -f1) at Contents/app/serenity.aot"
else
  cat "$work/record.log" "$work/create.log"
  row "Train before signing (record, then create, through the app launcher)" FAIL "$(tail -2 "$work/create.log" | tr '\n' ' ' | cut -c1-200)"
  exit 1
fi
awk '{ print } /^\[JavaOptions\]$/ { print "java-options=-XX:AOTCache=$APPDIR/serenity.aot" }' "$cfg" >"$work/cfg" && cat "$work/cfg" >"$cfg"
grep -q 'AOTCache' "$cfg" || { row "Name the cache in Serenity.cfg" FAIL "option missing"; exit 1; }

# Sign inside-out with the hardened runtime and the real entitlements. Ad hoc has no Developer ID, but the runtime flag
# is the part that could stop the JVM mapping a file.
while IFS= read -r -d '' f; do
  file -b "$f" | grep -q 'Mach-O' && printf '%s\t%s\n' "$(tr -cd / <<<"$f" | wc -c)" "$f"
done < <(find "$app" -type f -print0) | sort -t$'\t' -k1,1nr | cut -f2- | while IFS= read -r f; do
  codesign --force --sign - --options runtime --entitlements "$entitlements" "$f"
done
codesign --force --sign - --options runtime --entitlements "$entitlements" "$app/Contents/runtime"
codesign --force --sign - --options runtime --entitlements "$entitlements" "$app"
if codesign --verify --deep --strict --verbose=2 "$app" 2>"$work/verify.log"; then
  row "codesign --verify --deep --strict (cache inside the bundle)" PASS "$(codesign -dv "$app" 2>&1 | grep -E 'flags' | head -1)"
else
  row "codesign --verify --deep --strict (cache inside the bundle)" FAIL "$(tr '\n' ' ' <"$work/verify.log" | cut -c1-200)"
fi

expect_cache_used "Run in place, signed, hardened runtime" "$app"

ditto "$app" /Applications/Serenity.app && expect_cache_used "Run from /Applications (ditto copy)" /Applications/Serenity.app
codesign --verify --deep --strict /Applications/Serenity.app 2>/dev/null \
  && row "Signature valid after the copy" PASS "" || row "Signature valid after the copy" FAIL ""

hdiutil create -quiet -volname SerenitySpike -srcfolder "$app" -format UDZO "$work/spike.dmg" || exit 1
mount_point="$(hdiutil attach -nobrowse -readonly "$work/spike.dmg" | awk -F'\t' '/\/Volumes\// { print $NF }' | head -1)"
if [[ -n "$mount_point" ]]; then
  expect_cache_used "Run from the mounted DMG" "$mount_point/Serenity.app"
  rm -rf /Applications/Serenity.app
  ditto "$mount_point/Serenity.app" /Applications/Serenity.app
  xattr -w com.apple.quarantine "0081;00000000;Safari;" /Applications/Serenity.app 2>/dev/null || true
  expect_cache_used "Copied out of the DMG to /Applications, quarantined" /Applications/Serenity.app
  hdiutil detach -quiet "$mount_point" || true
else
  row "Mount the DMG" FAIL "hdiutil attach printed no mount point"
fi

exit "$failed"
