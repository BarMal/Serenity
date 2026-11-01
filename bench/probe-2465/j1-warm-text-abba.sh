#!/usr/bin/env bash
# j1-warm-text-abba.sh <artifact-dir> <out-dir> [rounds=A B B A]
# A: the cold full frame as in #2382. B: the same cold frame (layout and all other caches fresh) but with Skia's fonts and
# shapings already warm from an earlier frame of the same document. B minus A is the most that pre-shaping can return.
set -euo pipefail
ART="${1:?artifact dir}"; OUT="${2:?out dir}"; shift 2
if [[ $# -gt 0 ]]; then ORDER=("$@"); else ORDER=(A B B A); fi
mkdir -p "$OUT"; i=0
for arm in "${ORDER[@]}"; do
  i=$((i+1)); d="$OUT/run$i-$arm"; mkdir -p "$d/home"
  extra=(); [[ "$arm" == B ]] && extra=(-Dserenity.probe.warmText=true)
  (cd "$d" && java -Duser.home="$d/home" "${extra[@]}" --enable-native-access=ALL-UNNAMED -jar "$ART/Serenity.jar" \
     --render-benchmark --render-benchmark-size 1600x1000 --render-benchmark-frames 100 --render-benchmark-warmup 20 >"$d/log.txt" 2>&1)
  grep -hE "^Window|cold full frame|^Cold frame" "$d"/render-benchmark-*.md | sed "s/^/run$i-$arm: /"
done
