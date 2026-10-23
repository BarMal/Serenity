Raster/Xvfb evidence for #2378 (Skia raster surface behind a real `SwingWindow`, Linux, Xvfb). Review aid only: this directory is removed before merge.

- `prose-java2d.png` / `prose-skia-raster.png`, `code-java2d.png` / `code-skia-raster.png`: the same state through each factory.
- `title-bar-java2d-over-skia-raster.png` (if present): the pane title strip, Java2D above Skia raster, enlarged 2x. The title text differs in typeface; the body text matches.
- Produced by `SkiaRasterWindowSpec`; no GPU path, HiDPI or real display was exercised.
