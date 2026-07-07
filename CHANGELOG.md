# Changelog

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
