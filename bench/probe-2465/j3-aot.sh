#!/usr/bin/env bash
# j3-aot.sh <artifact-dir> <work-dir> [rounds=5]   (JDK 25, Linux)
# Wall time of `--smoke-test` (launch to first painted frame, plus exit) for: Java2D with no cache; Skia with no cache;
# the AOT cache as shipped (trained on --smoke-test --safe-mode) with Java2D and with Skia; and a cache trained on a Skia
# launch, with Skia and with Java2D (the common launch). Train and run on the same jar: a rebuilt jar makes the JVM reject its
# cache, and the times then look like no cache. The first round is a warm-up and is not printed.
set -euo pipefail
ART="${1:?artifact dir}"; W="${2:?work dir}"; ROUNDS="${3:-5}"; mkdir -p "$W"
JAR="$ART/Serenity.jar"; common=(--enable-native-access=ALL-UNNAMED)
for kind in safe skia; do
  rm -f "$W/$kind.aot"; h="$(mktemp -d -p "$W")"; args=(--smoke-test --safe-mode); [[ $kind == skia ]] && args=(--smoke-test --renderer=skia)
  (cd "$h" && java -Duser.home="$h" "${common[@]}" -XX:AOTCacheOutput="$W/$kind.aot" -jar "$JAR" "${args[@]}" >"$W/train-$kind.log" 2>&1)
  ls -l "$W/$kind.aot"; rm -rf "$h"
done
one() { local label=$1 cache=$2; shift 2; local h t0 t1; h="$(mktemp -d -p "$W")"; t0=$(date +%s%N)
  (cd "$h" && java -Duser.home="$h" "${common[@]}" $cache -jar "$JAR" "$@" >/dev/null 2>&1) || true
  t1=$(date +%s%N); echo "$label $(( (t1-t0)/1000000 ))"; rm -rf "$h"; }
for round in $(seq 0 "$ROUNDS"); do
  for arm in "j2d-none|-Dx=1|--smoke-test --safe-mode" "skia-none|-Dx=1|--smoke-test --renderer=skia" \
             "j2d-aot-safe|-XX:AOTCache=$W/safe.aot|--smoke-test --safe-mode" "skia-aot-safe|-XX:AOTCache=$W/safe.aot|--smoke-test --renderer=skia" \
             "skia-aot-skia|-XX:AOTCache=$W/skia.aot|--smoke-test --renderer=skia" \
             "j2d-aot-skia|-XX:AOTCache=$W/skia.aot|--smoke-test --safe-mode"; do
    IFS='|' read -r label cache args <<<"$arm"
    out=$(one "$label" "$cache" $args); [[ $round -gt 0 ]] && echo "$out"
  done
done
