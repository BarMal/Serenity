# SkiaInkCache text change, before and after (#2383)

Review aid only: this directory is removed before merge.

Skia raster surface (`SkiaRasterSurfaceFactory`), 900x600 logical window, Linux/Xvfb. Each image is a crop of the top left of the window (280x100 logical pixels), enlarged with nearest-neighbour: 4x at 1x scale, 2x at 2x scale, so single device pixels stay visible.

- `*-before.png`: glyphs drawn with a plain paint colour (the code of #2378).
- `*-after.png`: glyphs drawn with the colour as a shader (`SkiaInkCache`, commit `4a99a0c6`).
- Named `{prose|code}-{light|dark}-{1x|2x}-{before|after}.png`.
- Only the glyph paint differs between each pair; the rest of the renderer is the same build.
- Measured over these crops, no channel differs by more than 1 of 255 (`SkiaGlyphInkSpec` bounds the difference at 3 of 255).
- "code" is a Scala buffer in the code font; "prose" is the benchmark's lorem-ipsum buffer in the text font.
