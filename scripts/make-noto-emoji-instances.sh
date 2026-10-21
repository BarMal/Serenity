#!/usr/bin/env bash
# Rebuilds the bundled monochrome Noto Emoji static fonts from the upstream variable font.
# Java2D cannot select a variable font's weight axis, so bold would otherwise be synthetic emboldening that also
# widens the advance; each weight is cut as its own file instead (#1866). Needs `pip install fonttools`.
set -euo pipefail

# fontTools stamps head.modified with the clock unless this is set; a fixed value keeps the output byte-identical.
export SOURCE_DATE_EPOCH=1760918400

SOURCE_URL='https://raw.githubusercontent.com/google/fonts/main/ofl/notoemoji/NotoEmoji%5Bwght%5D.ttf'
SOURCE_SHA256='de6c18832938afc99caf132b39d6a30a19bac7f2e812e28db2535b4608d27551'
OUT="$(cd "$(dirname "$0")/.." && pwd)/src/main/resources/fonts"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

curl -fsSL -o "$WORK/NotoEmoji[wght].ttf" "$SOURCE_URL"
echo "$SOURCE_SHA256  $WORK/NotoEmoji[wght].ttf" | sha256sum -c -

fonttools varLib.instancer "$WORK/NotoEmoji[wght].ttf" wght=400 --update-name-table -o "$OUT/NotoEmoji-Regular.ttf"
fonttools varLib.instancer "$WORK/NotoEmoji[wght].ttf" wght=700 --update-name-table -o "$OUT/NotoEmoji-Bold.ttf"
