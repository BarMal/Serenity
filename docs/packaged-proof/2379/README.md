Before/after evidence for #2379 (logical fonts resolved to one physical face), Linux, Xvfb. Review aid only: this directory is removed before merge.

- `before-*` come from `SkiaRasterWindowSpec` on #2378's head, `after-*` from the same spec with this change merged in (scratch merge, not pushed).
- Java2D `before-*-java2d.png` and `after-*-java2d.png` are pixel-identical (AWT's SansSerif here was already Inter).
- Skia differs only in the pane title strip (pixel bbox x 396-504, y 2-13): `title-strip-java2d-before-after-skia-before-after.png` stacks, top to bottom, Java2D before, Java2D after, Skia before, Skia after, enlarged 2x.
- No GPU path, HiDPI or real display was exercised.
