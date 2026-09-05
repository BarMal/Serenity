# Changelog

## 2026-09-05

- Stopped a click on the floating cursor info bar from placing the caret in the hidden text behind it: the bar is derived per frame rather than stored, so the guard that keeps clicks out of a floating surface never saw it, and the bar sits exactly where the reader is working.
- Made Shift+Home and Shift+End select to the same place Home and End move to -- the cursor's own visual row under word wrap, rather than the whole logical line.
- Added Shift+PageUp and Shift+PageDown, which select a screenful of visual rows; the keys decoded with their modifier but had no binding at all, so they did nothing.
- Kept screen cells and buffer columns apart when grouping an animated run by colour, so a colour group after a wide glyph starts on the right cell and a surrogate pair is never split across two groups.

## 2026-09-04

- Property-tested the config format over generated settings, and fixed what it found: an explicit text-scale mode of "off" being overridden to "manual" by the multiplier, blur radius and background style never being written at all, and percentage settings coming back a floating-point hair away from what was saved.
- Applied broader config settings before the narrower ones that refine them, rather than in whatever order the key names happened to sort, which is what kept a motion preset from wiping the per-family settings saved alongside it.
- Moved the three config keys that were both a value and a path (`character.animation`, `ui.motion`, and each motion family's `animation`) onto leaves of their own, so none of them depends on being quoted to survive; the old spellings are still read.
- Measured the TUI's cell grid in display cells rather than characters, so the caret, word wrap, mouse hit-testing and cursor movement agree with the two cells a wide glyph is actually painted across.
- Widened Transport and Map Symbols and Symbols and Pictographs Extended-A to two cells, so a rocket or a plaster no longer takes one cell in the app's arithmetic and two on screen.
- Kept a terminal drag's selection when the mouse button is released, matching the Swing path, instead of collapsing it with a click synthesised from the release.
- Kept Home and End on the wrapped row the cursor is on, so End no longer leaves the caret at the far left of the row below and Home returns to the row's own start.
- Led the inert-in-TUI settings hints with their annotation, so it stays legible instead of being elided with the rest of a long hint.
- Moved PageUp and PageDown by a screenful of visual rows rather than logical lines, so a page through wrapped prose is a page of what is on screen, and left the viewport to follow the cursor at the effect boundary instead of the reducer scrolling on its own.
- Kept each render surface's previous-frame state against that surface rather than sharing one copy between all of them, and restored parallel test execution.
- Fixed a saved configuration with two or more cursor info bar segments writing a file that could not be read back, which silently reset every other setting to its default on the next launch.
- Refused to write a config file that cannot be parsed, so a formatting fault in one setting can no longer cost the user the rest of them.
- Persisted line numbers, gutter, word count, comment display mode, minimum pane width, command-runner key hints and the cursor-peek settings, none of which had a config key, and parsed `display.visual_line_navigation`, which had one but was never read.
- Kept an unreadable config file aside and said so on the start page, instead of a log line the TUI discards.
- Added a configurable mouse-wheel scroll distance (`input.wheel_scroll_lines`, default 3) and made the wheel scroll at all: both shells decoded wheel reports and dropped them.
- Repainted editor content on every damaged frame while the window sitter is active, so typed characters and their wrapped reflow appear immediately instead of trailing the cursor by the sitter's activity window.
- Anchored the vertical-navigation geometry window on the cursor's own visual row, so Up/Down and Home/End keep stepping by visual row deep inside a paragraph longer than a screenful instead of falling back to logical-line movement.
- Added a Visual Line Navigation toggle to the Text Display settings group, alongside the existing `display.visual_line_navigation` config key and command-palette toggle.
- Added a cell-level TUI behaviour suite driving real terminal sessions end to end (startup, editing, files, settings, unicode width, redraw, wrapped navigation, typing latency).
- Ran test suites serially, since the renderer's previous-frame state is keyed by pane id alone and concurrent painting suites overwrote each other's frame history.

## 2026-07-06

- Added a storage-location classifier that recognizes local paths, local `file:` URIs, and remote URI-backed document locations.
- Documented that remote storage is discoverable but not yet openable or saveable through the current local file IO.
- Blocked remote URI open/save-as workflow submissions before filesystem path parsing, keeping the modal open with a clear unsupported-storage status.
- Added parent breadcrumbs to nested settings search result rows in the command runner so preset settings are discoverable from search.
- Hardened Swing input shutdown observation tests so loaded Windows release jobs do not fail before idle streams can terminate.
- Changed the default window chrome to native OS chrome while keeping Serenity custom chrome available as an opt-in mode.
- Added release-cycle notes so the latest desktop release includes a human-readable changelog section alongside recent commits and downloadable assets.
- Kept stacked command-runner submenus contained inside the active editor content area in tiny viewports.
- Added a tested custom-chrome canvas fallback snapshot so editor layout receives post-title-bar viewport dimensions.
- Hid command-runner panel order controls unless multiple pinned panels share the same edge.
- Kept the selected theme-creator field inside the visible list window when editing near the end of the theme settings.
- Added an in-app UI outline thickness setting for panel and command-runner surface borders.
- Allowed pasted clipboard text to populate command-runner search and focused setting input rows.
- Made root settings search open matched submenus filtered to the matching nested setting row.
- Highlighted settings submenu breadcrumb ancestors and dimmed inactive command-runner panels while a child panel has focus.
- Preserved custom character animation duration and step settings across config reloads.
- Made quit-scope "Close anyway" discard the current dirty buffer before completing shutdown.
- Kept fast rendering active when text input arrives during a previous render phase shutdown, reducing cursor flicker.
- Restored maximized custom-chrome windows when title-bar dragging begins, allowing monitor-to-monitor drags to continue.
- Published visible cursor full frames atomically so cursorless base frames do not flash between overlay updates.
- Preserved rich-text inline formatting when editing inside formatted words.
- Matched custom-chrome window controls to platform placement and order on macOS versus Windows/Linux.
- Rendered Markdown preview images at device scale on Hi-DPI displays to avoid blurry split and inline-lens previews.
- Hid impossible panel action commands from settings when no panel is pinned on that edge.
- Clipped measured overlay text and carets to the framed content rectangle.
- Filled the full Markdown preview raster with the selected preview background for short documents.
- Split preset rename controls from preset actions in the command runner settings flow.
- Added a configurable command-runner reveal choreography setting backed by the semantic transition planner.
