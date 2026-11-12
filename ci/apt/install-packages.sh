#!/usr/bin/env bash
# Installs apt packages the way a CI job needs them: from a cache of the downloaded .deb files when there is one, and
# otherwise from the mirrors with retries and timeouts, because a runner that cannot resolve a mirror for a few minutes
# should not fail the job (#2618, [ci-apt]).
# usage: ci/apt/install-packages.sh ARCHIVE_DIR CACHE_HIT PACKAGE...
#   ARCHIVE_DIR  where the .deb files are kept between runs (the directory actions/cache restores)
#   CACHE_HIT    "true" when the cache was restored; anything else is a miss
# SUDO and APT_RETRY_SLEEP exist for the script's own tests.
set -uo pipefail

archive="$1"
cache_hit="$2"
shift 2
packages="$*"
privileged="${SUDO-sudo}"
pause="${APT_RETRY_SLEEP:-15}"
attempts=3

mkdir -p "$archive/partial"

options=(
  -o "Dir::Cache::archives=$archive"
  -o Acquire::Retries=5
  -o Acquire::http::Timeout=20
  -o Acquire::https::Timeout=20
)

# A restored cache holds every package the last fetch needed, so dpkg can install the set without the network or the
# package lists (a fresh runner may have none, which `apt-get install --no-download` needs).
if [[ "$cache_hit" == "true" ]]; then
  shopt -s nullglob
  cached=("$archive"/*.deb)
  shopt -u nullglob
  if ((${#cached[@]} > 0)); then
    echo "cache hit: installing ${#cached[@]} cached packages" >&2
    if $privileged dpkg -i "${cached[@]}"; then
      exit 0
    fi
    echo "the cached packages did not install; fetching $packages instead" >&2
  fi
fi

echo "cache miss: fetching $packages" >&2
for attempt in $(seq 1 "$attempts"); do
  # shellcheck disable=SC2086 # the package list is meant to split
  if $privileged apt-get update -q "${options[@]}" &&
    $privileged apt-get install -y -q --no-install-recommends "${options[@]}" $packages; then
    exit 0
  fi
  echo "apt attempt $attempt of $attempts failed for: $packages" >&2
  if ((attempt < attempts)); then
    sleep "$pause"
  fi
done

echo "could not install after $attempts attempts: $packages" >&2
exit 1
