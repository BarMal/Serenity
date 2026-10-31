#!/usr/bin/env bash
# Rebuilds the runtime's base class-data archive of a Linux or Windows app image with the launcher's module options:
# scripts/regenerate-base-cds-archive.sh dist/Serenity
# jlink's --generate-cds-archive dumps classes.jsa without --enable-native-access, which the launcher passes at run time.
# The JVM then logs "Mismatched values for property jdk.module.enable.native.access", drops its archived module graph and,
# from the second start on, prints it as an error. Dumping again with the same option removes the disagreement. Fails if
# the dump fails or the runtime still reports a mismatch afterwards.
set -euo pipefail

image="${1:?usage: $0 path/to/app-image}"
native_access="--enable-native-access=ALL-UNNAMED"

java="$(find "$image" -type f \( -path '*/runtime/bin/java' -o -path '*/runtime/bin/java.exe' \) -print -quit)"
if [[ -z "$java" ]]; then
  echo "No jlink runtime (runtime/bin/java) under $image" >&2
  exit 1
fi

if ! dump_log="$("$java" -Xshare:dump "$native_access" 2>&1)"; then
  echo "$dump_log" >&2
  echo "Dumping the base class-data archive failed." >&2
  exit 1
fi

check_log="$("$java" -Xlog:cds "$native_access" -version 2>&1 || true)"
if grep -q 'Mismatched values for property' <<<"$check_log"; then
  echo "$check_log" >&2
  echo "Mismatched values for property: the regenerated base archive still disagrees with the launcher options." >&2
  exit 1
fi

echo "Base class-data archive regenerated with $native_access: $java"
