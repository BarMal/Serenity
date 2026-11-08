#!/usr/bin/env bash
# Records how much memory a macOS runner has free and how much it has pushed to swap while the tests run, so a slow
# run can be told apart from one that paged (#2484). Usage:
#   sample-os-memory.sh once                      one tab-separated record on stdout
#   sample-os-memory.sh loop DIR SECONDS [COUNT]  appends a record to DIR/os-mem.log every SECONDS, COUNT times or
#                                                 until the job ends
# A record is: epoch, page bytes, free pages, pages held by the compressor, swapouts, swap used in MB, free memory %.
# On a machine without vm_stat, sysctl and memory_pressure it records nothing. It always exits 0 because a diagnostic
# must not fail the job.
set -uo pipefail

has_macos_tools() {
  command -v vm_stat > /dev/null 2>&1 && command -v sysctl > /dev/null 2>&1 && command -v memory_pressure > /dev/null 2>&1
}

sample() {
  has_macos_tools || return 0
  local paging swap pressure
  paging="$(vm_stat | awk '
    /page size of/ { match($0, /[0-9]+ bytes/); page = substr($0, RSTART, RLENGTH - 6) }
    /^Pages free:/ { free = $3; sub(/\./, "", free) }
    /^Pages occupied by compressor:/ { compressed = $5; sub(/\./, "", compressed) }
    /^Swapouts:/ { swapouts = $2; sub(/\./, "", swapouts) }
    END { printf "%s\t%s\t%s\t%s", page, free, compressed, swapouts }
  ')"
  swap="$(sysctl -n vm.swapusage | awk '{
    for (i = 1; i <= NF; i++) if ($i == "used") {
      value = $(i + 2); unit = substr(value, length(value), 1); sub(/[KMG]$/, "", value)
      if (unit == "K") value = value / 1024; else if (unit == "G") value = value * 1024
      printf "%.2f", value
    }
  }')"
  pressure="$(memory_pressure | awk '/System-wide memory free percentage:/ { value = $NF; sub(/%/, "", value); print value }')"
  printf '%s\t%s\t%s\t%s\n' "$(date +%s)" "$paging" "$swap" "$pressure"
}

case "${1:-}" in
  once)
    sample
    ;;
  loop)
    dir="${2:?directory}"
    seconds="${3:-30}"
    count="${4:-0}"
    if has_macos_tools; then
      mkdir -p "$dir"
      taken=0
      while [ "$count" -eq 0 ] || [ "$taken" -lt "$count" ]; do
        sample >> "$dir/os-mem.log"
        taken=$((taken + 1))
        if [ "$count" -ne 0 ] && [ "$taken" -ge "$count" ]; then break; fi
        sleep "$seconds"
      done
    fi
    ;;
  *)
    echo "usage: sample-os-memory.sh once | loop DIR SECONDS [COUNT]" >&2
    ;;
esac
exit 0
