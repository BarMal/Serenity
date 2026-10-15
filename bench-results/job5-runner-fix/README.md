# Job 5 — Confirmation A/B for the white command-runner fix

**master**: `1b22ee31` (originally reproduced at `3b88daeb`; re-verified at the then-current
tip since only an unrelated commit sat in between)
**fix branch**: `ccr-157ed55f-u6khhf-runner-fix`, tested at `97d818eb` (tip at test time;
core fix is `74289700` "Stop accessibility proxies painting an opaque back buffer over the
directly presented canvas", plus a scalafix lint pass and a master merge on top)

Both builds launched with a scratch `-Duser.home` so the owner's real `~/.serenity/config.conf`
was never touched. Session: Wayland (Hyprland), both builds ran under XWayland
(`PlatformDefault` toolkit — no WLToolkit on a stock JDK).

## Results

| check | master | fix branch |
|---|---|---|
| (a) command runner, type text | **White block.** Text flashes briefly then settles white | Text visible, correct |
| (b) find/replace | **White block**, same flicker-then-white pattern; holding backspace makes correct content flicker through between repaints | Both Find (Ctrl+F) and Replace (Ctrl+H) work; theme/settings navigable via the command runner |
| (c) menu bar + tooltip | Works correctly | Works correctly |
| (d) resize/minimize/restore with runner open | Flickers | Correct, no flicker |
| (e) flicker overall | Present throughout (a), (b), (d) | None observed |

**Mechanism confirmed**: the flash-then-settle-white behavior on `master` (content renders
correctly for a frame, then gets painted over) matches the fix commit's description exactly.
The bug is not command-runner-specific — find/replace showed the identical symptom.

**Separate, unrelated finding**: `Ctrl+O` (Open File) reliably crashes both builds with
`GLib-GIO-ERROR: Settings schema 'org.gtk.Settings.FileChooser' is not installed` → SIGABRT
(exit 134). This is a missing GTK GSettings schema in this NixOS dev environment, not a
Serenity code issue. It happened once directly and once indirectly (blind command-runner
navigation while the runner was unreadable landed on a command that triggered the file
picker) — itself a real usability consequence of the bug under test.

**Logs**: no warnings on either build outside the two `Ctrl+O` crashes on `master`; the fix
branch logged zero errors or warnings across the whole session.

## Screenshots

- `master-a.png`, `master-a2.png` — command runner white block on master
- `master-b-find.png` — find/replace white block on master
- `master-c-menu.png` — menu bar/tooltip working correctly on master
- `master-d-resize.png` — runner flicker after resize/minimize/restore on master
- `probe-a.png` — command runner working on the fix branch
- `probe-b.png` — find/replace working on the fix branch (idle state after dialogs closed)
- `probe-c-menu.png` — menu bar/tooltip on the fix branch
- `probe-d-resize.png` — clean resize/minimize/restore on the fix branch, no flicker
