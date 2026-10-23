#!/usr/bin/env bash
# Throwaway: download the newest jbr-release-${SERIES:-25}* jbrsdk for $PLAT into $1 and print its JAVA_HOME. Needs GH_TOKEN.
set -euo pipefail
dest="${1:?dest}"; plat="${PLAT:-linux-x64}"
tag="$(gh release list -R JetBrains/JetBrainsRuntime --limit 300 --json tagName,publishedAt \
  --jq '[.[] | select(.tagName | startswith("jbr-release-${SERIES:-25}"))] | sort_by(.publishedAt) | last | .tagName')"
ver="$(echo "$tag" | sed -E 's/^jbr-release-([0-9.]+)b([0-9.]+)$/\1/')"
build="$(echo "$tag" | sed -E 's/^jbr-release-([0-9.]+)b([0-9.]+)$/\2/')"
name="jbrsdk-${ver}-${plat}-b${build}.tar.gz"
mkdir -p "$dest"
curl -fsSL -o "$dest/$name" "https://cache-redirector.jetbrains.com/intellij-jbr/$name"
tar -xzf "$dest/$name" -C "$dest"
echo "$tag" > "$dest/TAG"
dirname "$(dirname "$(find "$dest" -maxdepth 6 -type f \( -name java -o -name java.exe \) -path '*bin*' | head -n1)")"
