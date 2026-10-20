#!/usr/bin/env bash
# Prints a Markdown table of the slowest test suites in the JUnit reports of DIR (default target/test-reports), so a
# run that nears its time limit shows which suites took the time without anyone downloading the artifact. The suite's
# own time attribute is read, not its test cases'. Plain POSIX awk/sort so it runs on the Linux, macOS (BSD awk) and
# Windows (Git Bash) runners; it always exits 0 because a diagnostic must not fail the job.
#   summarize-test-durations.sh [DIR] [COUNT]
set -uo pipefail

dir="${1:-target/test-reports}"
count="${2:-30}"
tab="$(printf '\t')"

echo "## Slowest test suites"
echo

shopt -s nullglob
reports=("$dir"/*.xml)
if [ "${#reports[@]}" -eq 0 ]; then
  echo "No JUnit reports in $dir."
  exit 0
fi

suites="$(mktemp)"
awk '
  FNR == 1 { collecting = 0; done = 0; opening = "" }
  done { next }
  /<testsuite([ >]|$)/ { collecting = 1 }
  collecting { opening = opening " " $0 }
  collecting && />/ {
    name = ""; time = ""; tests = ""
    if (match(opening, / name="[^"]*"/)) name = substr(opening, RSTART + 7, RLENGTH - 8)
    if (match(opening, / time="[^"]*"/)) time = substr(opening, RSTART + 7, RLENGTH - 8)
    if (match(opening, / tests="[^"]*"/)) tests = substr(opening, RSTART + 8, RLENGTH - 9)
    if (name != "" && time != "") printf "%s\t%s\t%s\n", time, tests, name
    collecting = 0; done = 1
  }
' "${reports[@]}" > "$suites"

total_suites="$(grep -c . "$suites" || true)"
total_time="$(awk -F "$tab" '{ sum += $1 } END { printf "%.3f", sum }' "$suites")"
echo "$total_suites suites, $total_time s in total (suite times overlap where suites run in parallel)."
echo
echo "| Time s | Tests | Suite |"
echo "|---:|---:|---|"
sort -t "$tab" -k1,1 -rn "$suites" | head -n "$count" | awk -F "$tab" '{ printf "| %s | %s | %s |\n", $1, $2, $3 }'
rm -f "$suites"
exit 0
