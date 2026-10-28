# #2395 context-menu sizing: before and after

Rendered by CI through the real GUI renderer (`ContextMenuScreenshotSpec`), on the Linux runner under Xvfb. They are not
screenshots of the packaged application, so the owner's check on a real desktop (XToolkit and WLToolkit, scale 1 and 2)
is still wanted.

- `before-*`: master's sizing (the menu takes the whole pane's width).
- `after-*`: this branch (the menu is its widest row plus padding and border, clamped to the pane).
- `*-scale1.png`: 18 pt font. `*-scale2.png`: the same layout with a 36 pt font, so every pixel dimension doubles. It is
  not a HiDPI surface.
- `before-layout.txt`, `after-layout.txt`: the menu's measured size in cells for each scene.
- The CJK scene was re-rendered with Noto Sans CJK installed on the runner (a temporary CI step), so it shows real
  glyphs; `*-layout.txt` lists the CJK families the runner had.

Remove this directory before merge.
