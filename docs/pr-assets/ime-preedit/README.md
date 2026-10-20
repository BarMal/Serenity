Frames from the real GUI renderer (`ImePreEditScreenshotSpec`, Java2D surface, Monaspace Neon 18pt, IPAGothic as the
configured font fallback). Regenerate with `IME_SCREENSHOTS_DIR=<dir> sbt "testOnly *ImePreEditScreenshotSpec"`.
Review aid for the PR only; dropped before merge.

- `0-no-composition.png`: the buffer, caret at column 11.
- `1-unconverted.png`: `かんじ` composing at the caret: underlined, text after it pushed right, caret at the end.
- `2-converting.png`: `漢字変換` with the first clause converting (selection background) and the second underlined.
- `3-text-moved.png`: the same frame as 2, kept as the "text moved right" assertion's evidence.
