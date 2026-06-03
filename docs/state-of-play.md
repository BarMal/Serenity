# Serenity State Of Play

Generated on 2026-06-02 against the current `lsp-integration` branch state.

Status labels used here:
- `Implemented`: present in code and backed by direct implementation or tests.
- `Partial`: present, but narrower than the product spec or obviously incomplete.
- `Missing`: no direct implementation evidence found in the current codebase.

## 1. Core editor model

- `Implemented`: Serenity uses ropes as its primary text model. `Buffer.content` is a `Rope`, and core editor mutations go through rope operations such as `insert`, `delete`, `searchAll`, and `replaceAll`.[1][2][3]
- `Implemented`: Buffers already carry editor state that matters for real use: file path, dirty flag, language, selection, viewport, multiple cursor positions, and per-buffer find state.[2]
- `Partial`: Multiple cursors exist in the model and renderer, but most editing reducers still operate on `buffer.cursors.headOption` and then rewrite the list as `newCursor :: buffer.cursors.tail`. That means the first cursor is authoritative and multi-cursor editing workflows are not complete yet.[2][3]

## 2. Panes and split views

- `Implemented`: Serenity supports multiple editor panes with ordered pane management and split insertion. `PaneOrderSpec` exercises pane creation, insertion, closing, and `splitPaneHorizontal` ordering.[4]
- `Implemented`: Layout calculation already accounts for multiple panes, minimum pane widths, focus-aware visible windows, and pinned side panels around the editor workspace.[5]
- `Partial`: The current implementation is a horizontal multi-pane model, not a general tiling/window-tree system. That satisfies the basic split-pane requirement, but it is narrower than a fully general pane system.[4][5]

## 3. Standard text editor functionality

- `Implemented`: Text entry, newline, backspace/delete, line navigation, page navigation, select-all, copy, cut, paste, go-to-line, find, and find-next are all handled in the main editor reducer.[3]
- `Implemented`: Selection highlighting is rendered in the editor surface and covered by renderer tests.[6]
- `Implemented`: File-oriented commands such as `open`, `save`, `save-as`, `new`, `close`, `close-all`, and `close-others` are exposed through the command registry.[7]
- `Implemented`: Replace workflow exists and currently behaves as a replace-all operation over the focused buffer; the state-manager spec verifies replacing every match and dismissing the modal.[8]
- `Implemented`: Cursor centering logic exists both for ordinary cursor movement and explicit viewport centering calculations.[3][5]
- `Partial`: Find is implemented as per-buffer query state with next-result navigation, but the current modal workflow is still quite small compared with the full spec phrasing of `find`, `replace`, `find all`, `replace all`, and combined batch flows.[2][3][8]
- `Partial`: Multiple-cursor rendering is present, but multiple-cursor editing behavior is still incomplete for the same reason noted above.[2][3]

## 4. Sessions, startup, and restoration

- `Implemented`: Serenity has a startup surface offering `Start a new session`, `Restore an existing session`, and `Open an existing file or directory`.[9]
- `Implemented`: Session persistence supports save, save-as, load, rename, delete, clear, history pruning, and a current-session index.[10][11]
- `Implemented`: Session state persists buffers, pane layout, config, theme name, recent files, cursor positions, viewport state, unsaved content, and per-buffer find state.[11]
- `Partial`: Session restoration currently restores editor-pane focus only. Surface focus is intentionally not persisted, so UI overlays do not come back across launches.[11]

## 5. Keybindings and configurability

- `Partial`: There is a stable hotkey layer for common editor operations, command runner toggle, file search, and tab navigation.[12]
- `Missing`: Keymappings are not entirely configurable or overrideable. The active hotkey map is hard-coded in `TextHotkeyConverters`, while `ConfigManager` only parses two simple config keys: character animation and syntax highlighting.[12][13]

## 6. Mouse support

- `Partial`: Mouse click support is real. Swing input converts AWT mouse clicks into editor events, and click handling supports cell-based and pixel-aware hit testing for proportional text.[14][15]
- `Missing`: No direct support was found for drag selection, mouse hover behavior, right-click context menus, or richer mouse gestures. The current mouse event model is a single `MouseClick` event rather than a broader pointer interaction system.[14][15]

## 7. Fonts and content-aware layout

- `Implemented`: Serenity supports separate code and text font families, font size, and ligature configuration through `AppConfig` and command-runner settings.[16][17]
- `Implemented`: Runtime typography switches between code and text fonts based on current content context. Markdown uses text typography; non-Markdown buffers use code typography.[18][3]
- `Implemented`: Wrapped-line layout now distinguishes monospaced and proportional text correctly, with fixed-advance behavior for monospaced fonts and measured layout for proportional fonts.[19]
- `Partial`: The content-aware switch is currently language-based, effectively `Markdown` versus everything else. That is useful, but it is simpler than a richer `code vs prose` classification model.[18][3]

## 8. Command runner and nested floating panels

- `Implemented`: Serenity has a floating command runner that opens beneath the cursor and restores focus when dismissed.[20][21]
- `Implemented`: Settings are grouped into nested panels with preview and focused submenu states rather than flattened rows.[17][20][21]
- `Implemented`: The command runner already exposes settings for animation mode, cursor mode, background style, code font, text font, ligatures, blur radius, animation timing, animation steps, and font size.[16][17]
- `Partial`: The command runner is already the main floating settings surface, but it is still more of a structured command/settings palette than a complete universal settings shell for every system in the product spec.[7][17]

## 9. Themes, effects, and animation

- `Implemented`: Theme support exists, including syntax colors and theme-aware rendering.[22][23]
- `Implemented`: Theme selection supports live preview while moving through the theme picker and restores the original theme on cancel.[24][25]
- `Implemented`: Theme interpolation exists for colour transitions between themes.[26]
- `Implemented`: Background styles include `Solid`, `Transparent`, `Frosted`, and `GlassLike`, with blur/sheen material behavior in the renderer.[16][27]
- `Implemented`: Overlay and character animation systems are present and configurable through app config and command-runner settings.[16][17][26]
- `Partial`: The aesthetic surface is strong, but the current spec language about “a variety of effects on UI elements” is broader than the implemented set. The code clearly supports several effects already, but not an obviously large preset library.[16][17][27]

## 10. Window interactions

- `Implemented`: Serenity handles resize events and recomputes viewport size from the Swing canvas.[14][28]
- `Implemented`: Custom window chrome exists with drag, manual edge resize, minimise, maximise/restore, and close controls when `WindowChromeMode.Custom` is enabled.[16][28]
- `Implemented`: Graceful quit and dirty-buffer close workflow are covered by dedicated tests.[29]
- `Partial`: Window behavior is solid in Swing, but there is no evidence of a cross-platform abstraction for advanced window management beyond the current desktop implementation.[28][29]

## 11. Pinned panels and layouts

- `Implemented`: Serenity supports pinned panels on the top, bottom, left, and right via `SurfacePresentation.Pinned` and `PanelPosition`.[21][30]
- `Implemented`: Different panel types can be pinned, resized, focused, replaced in-position, or unpinned.[21][30][31]
- `Implemented`: Multiple pinned panels can coexist when they occupy different sides, and layout reflows around them.[5][30]
- `Partial`: Only one pinned surface is kept per side at a time. The product spec calls for multiple panels per side, which is broader than the current replacement-based behavior.[30][31]
- `Partial`: Floating surfaces such as directory listing, terminal, outline, and diagnostics can be pinned into side panels, but not every floating surface type is pinnable.[31]

## 12. Panel removal, expansion, presets, and UI density modes

- `Implemented`: Panels can be removed with `unpin`, and focus falls back to the editor when appropriate.[31]
- `Missing`: No direct implementation evidence was found for expanding a side panel into a central view.
- `Missing`: No direct implementation evidence was found for standard preset panel configurations.
- `Missing`: No direct implementation evidence was found for an explicit minimalist/maximalist interface mode system.

## 13. Information bars and floating info near the cursor

- `Implemented`: Serenity already has above-cursor and below-cursor floating surface placement, including quick-info, symbol-definition, command runner, submenu, file search, and modal overlays.[5][20][32]
- `Partial`: Floating information surfaces under and above the cursor are real, but there is no clear dedicated implementation of a configurable pinned information bar beneath the cursor as a first-class feature separate from the general floating-surface system.[5][20][32]

## 14. IDE and language-aware features

- `Implemented`: Syntax highlighting exists, but the current parser is intentionally simple rather than language-complete.[13][22][23]
- `Implemented`: LSP wiring exists for server resolution, connection startup, workspace-root detection, and `didOpen` / `didChange` / `didClose` notifications.[33]
- `Implemented`: LSP diagnostics flow back into app state and can be rendered in diagnostics surfaces.[33]
- `Partial`: Language detection and workspace-root markers exist, but the shipped LSP path is still configured through `LspUserConfig.empty`, which limits user control.[33]
- `Missing`: No direct implementation evidence was found for build, compilation, test running, debugging, or dependency resolution workflows.
- `Missing`: Surface types for quick info and symbol definitions exist, but no direct request path was found for hover/completion/definition/intellisense features driven from the current LSP layer. Current LSP event handling is diagnostics-focused.[32][33]

## 15. Mode switching and architecture

- `Missing`: No explicit `text editing` versus `development/code writing` mode-switch feature was found. The closest current behavior is typography/layout switching based on buffer language.[18][3]
- `Implemented`: The architecture is strongly event-driven and reducer-oriented. `AppState` is the central model, features are split into focused reducers such as `EditorEventReducer`, `ModalEventReducer`, and `PanelStateReducer`, and async/runtime concerns live around Cats Effect state management and streams.[3][20][31][33]
- `Partial`: The overall architecture is principled, but some core files are already carrying a lot of responsibility. That is not a correctness problem today, but it is likely to become a maintenance pressure point as more of the missing spec surface is added. This is an architectural inference rather than a failing test result.

## Summary

- Strongest implemented areas today: rope-backed editing, nested command runner overlays, session restore/save, theming and animation infrastructure, typography switching, and pinned-panel basics.
- Largest functional gaps against the product spec: fully configurable keymaps, rich mouse interactions, true multi-cursor editing, multi-panel-per-side layouts, preset workspace layouts, explicit UI density modes, and deeper IDE features beyond syntax colouring plus diagnostics-oriented LSP plumbing.
- Best near-term clarification targets: what “find/replace family” should mean in UX terms, how ambitious pane layouts need to be beyond horizontal splits, and whether typography switching should remain language-driven or become a richer prose/code mode system.

## Sources

[1] `src/main/scala/com/serenity/rope/Rope.scala`
[2] `src/main/scala/com/serenity/state/models/Buffer.scala`
[3] `src/main/scala/com/serenity/state/reducers/EditorEventReducer.scala`
[4] `src/test/scala/com/serenity/PaneOrderSpec.scala`
[5] `src/main/scala/com/serenity/ui/layout/LayoutEngine.scala`
[6] `src/test/scala/com/serenity/EditorSelectionRenderingSpec.scala`
[7] `src/main/scala/com/serenity/command/CommandRegistry.scala`
[8] `src/test/scala/com/serenity/ReplaceWorkflowStateManagerSpec.scala`
[9] `src/main/scala/com/serenity/app/AppStartup.scala`
[10] `src/main/scala/com/serenity/session/SessionManager.scala`
[11] `src/main/scala/com/serenity/session/SessionState.scala`
[12] `src/main/scala/com/serenity/keystroke/translators/TextHotkeyConverters.scala`
[13] `src/main/scala/com/serenity/config/ConfigManager.scala`
[14] `src/main/scala/com/serenity/input/SwingInputHandler.scala`
[15] `src/test/scala/com/serenity/MouseClickSpec.scala`
[16] `src/main/scala/com/serenity/config/AppConfig.scala`
[17] `src/main/scala/com/serenity/command/CommandRunner.scala`
[18] `src/main/scala/com/serenity/app/RuntimeDisplayState.scala`
[19] `src/main/scala/com/serenity/ui/layout/TextLayoutSnapshot.scala`
[20] `src/main/scala/com/serenity/state/models/AppState.scala`
[21] `src/test/scala/com/serenity/UIHotkeysAndPanelsSpec.scala`
[22] `src/main/scala/com/serenity/ui/theme/ThemeManager.scala`
[23] `src/test/scala/com/serenity/ThemeSupportSpec.scala`
[24] `src/main/scala/com/serenity/state/models/ThemePickerState.scala`
[25] `src/test/scala/com/serenity/ThemePickerSpec.scala`
[26] `src/main/scala/com/serenity/animation/ThemeInterpolator.scala`
[27] `src/main/scala/com/serenity/ui/renderer/SurfaceMaterials.scala`
[28] `src/main/scala/com/serenity/ui/terminal/SwingWindow.scala`
[29] `src/test/scala/com/serenity/GracefulWindowCloseSpec.scala`
[30] `src/main/scala/com/serenity/state/models/UiSurface.scala`
[31] `src/main/scala/com/serenity/state/reducers/PanelStateReducer.scala`
[32] `src/main/scala/com/serenity/ui/renderer/SurfaceContentResolver.scala`
[33] `src/main/scala/com/serenity/lsp/LspManager.scala`
