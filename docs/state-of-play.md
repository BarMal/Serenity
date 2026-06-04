# Serenity State Of Play

Generated on 2026-06-04 against the current branch state.

This document now serves two purposes:
- a current implementation status report
- the working backlog/tick list for feature completion

Status labels used here:
- `[x]` Implemented in code and backed by direct implementation and/or tests
- `[~]` Partial: real, but narrower than the intended product shape
- `[ ]` Missing: no direct implementation evidence found in the current codebase

Priority labels used here:
- `P0` Editing fundamentals and correctness
- `P1` UX polish and workflow quality
- `P2` IDE/deeper language tooling

## Since The Last Refresh

- `[x]` Plain-text buffers now use the text font path rather than falling back to the code font.[2][37][38]
- `[x]` Runtime typography now distinguishes buffer font size from UI font size.[16][17][18]
- `[x]` The command runner now exposes a language submenu, separate typography controls, submenu scrolling, and word-delete behavior for text-entry rows.[17][32][40][41]
- `[x]` The old untyped `CommandIntent.Custom` escape hatch has been removed, so command execution stays on the typed event/effect path.[7][35][36]
- `[x]` `StateManager` has been split into composed pipeline/facade traits instead of remaining one monolithic implementation file.[34][35][36]
- `[x]` Markdown editor rendering now has structural styling for headings, list markers, blockquotes, inline code, and links, including underline-capable style propagation through the renderer.[22][23][37]
- `[x]` Global hotkeys now support config-backed overrides, and runtime startup now actually boots from `ConfigManager.loadConfig()` rather than always using `AppConfig.default`.[12][13][46][47][48]
- `[x]` Focused keymaps are now configurable through config-backed editor, command-runner, modal, panel, and peek binding groups, and trigger parsing now covers non-character keys such as enter, escape, arrows, home/end, and page navigation.[12][13][46][47][49]
- `[x]` Markdown block-lens rendering now keeps the active Markdown block as raw source while surrounding Markdown lines use Markdown presentation styling. Renderer capabilities and limitations are documented separately.[19][22][37][50]

## 1. Core Editor Model

- `[x]` Serenity uses ropes as its primary text model. `Buffer.content` is a `Rope`, and core editor mutations go through rope operations such as `insert`, `delete`, `searchAll`, and `replaceAll`.[1][2][3]
- `[x]` Buffers carry real editor state: file path, dirty flag, language, viewport, multiple cursors, selection(s), per-buffer find state, and typography/navigation hints such as preferred column and preferred pixel x.[2]
- `[~][P0]` Multi-cursor editing is only partially complete. The reducer now has dedicated multi-cursor and multi-selection paths; distinct-cursor whole-line copy/cut, overlapping multi-cursor word deletions, tab/delete-forward parity, repeated vertical navigation, and global actions such as select-all/find/modal-open are now covered, but many single-cursor branches still rely on `buffer.cursors.headOption` and write back via `newCursor :: buffer.cursors.tail`, so the primary cursor still dominates a lot of editing behavior.[3][45]

## 2. Panes And Split Views

- `[x]` Serenity supports multiple editor panes with ordered pane management and split insertion. `PaneOrderSpec` exercises pane creation, insertion, closing, and `splitPaneHorizontal` ordering.[4]
- `[x]` Layout calculation already accounts for multiple panes, minimum pane widths, focus-aware visible windows, and pinned side panels around the editor workspace.[5]
- `[~][P1]` The pane model is still a horizontal multi-pane system, not a general tiling/window-tree layout engine.[4][5]

## 3. Editing Fundamentals

- `[x]` Text entry, newline, backspace/delete, word delete, line navigation, page navigation, select-all, copy, cut, paste, go-to-line, and find-next are all handled in the editor reducer.[3]
- `[x]` Selection highlighting is rendered in the editor surface and covered by renderer tests.[6][37]
- `[x]` File-oriented commands such as `open`, `save`, `save-as`, `new`, `close`, `close-all`, and `close-others` are exposed through the command registry.[7]
- `[x]` Cursor centering and cursor visibility logic exist for both ordinary movement and viewport-aware repositioning.[3][5]
- `[~][P0]` The find/replace family is still narrower than a full editor workflow. Find is per-buffer query state with next-result navigation, and replace now supports both `Replace Next` and `Replace All` across the whole buffer or the active selection, but there is still no broader `find all` result set, project-wide scope, or richer result-navigation workflow yet.[2][3][8]
- `[x][P0]` Core mouse-driven text editing now works: click-to-place, shift-click range extension, drag selection, shift-drag extension, double-click word selection, and triple-click line selection all work, including proportional hit testing.[14][15]

## 4. Markdown And Prose Editing/Rendering

- `[x]` Markdown is a first-class buffer language and is selectable through the command runner language submenu.[17][42]
- `[x]` Markdown buffers participate in LSP server resolution through the built-in Marksman mapping, and buffer-language changes refresh the LSP binding for file-backed buffers.[36][42][43][44]
- `[x]` Markdown and plain-text buffers use the text-font rendering path rather than the code-font path.[2][37][38]
- `[x]` Markdown/prose layout uses measured caret stops, proportional wrapping, proportional selection rendering, and pixel-aware cursor placement where appropriate.[19][37][39]
- `[~][P0]` Markdown rendering is still editor-first rather than preview-first. The editor now applies markdown-aware styling for headings, list markers, blockquotes, inline code, and links, and block-lens rendering keeps the active Markdown block raw while surrounding lines use presentation styling. There is still no separate preview surface or richer renderer for rendered tables, images, mixed heading sizes, or document-preview presentation.[19][22][23][32][37][50]
- `[~][P1]` Markdown editing has the language/runtime plumbing it needs, but there is no markdown-specific command surface yet for preview toggles, structured block operations, or document-style editing affordances.[17][32][36]

## 5. Sessions, Startup, And Restoration

- `[x]` Serenity has a startup surface offering `Start a new session`, `Restore an existing session`, and `Open an existing file or directory`.[9]
- `[x]` Session persistence supports save, save-as, load, rename, delete, clear, history pruning, and a current-session index.[10][11]
- `[x]` Session state persists buffers, pane layout, config, theme name, recent files, cursor positions, viewport state, unsaved content, and per-buffer find state.[11]
- `[~][P1]` Session restoration still restores editor-pane focus only. UI overlays and transient surface focus are intentionally not restored.[11]

## 6. Keybindings And Configurability

- `[x]` There is a stable hotkey layer for common editor operations, command runner toggle, file search, and tab navigation.[12]
- `[x]` Focus-aware input routing now cleanly distinguishes editor, modal, command-runner, submenu, panel, and peek input behavior.[40][41]
- `[~][P0]` Key mappings are now broadly config-driven across the focused input surfaces. Global hotkeys plus editor, command-runner, modal, panel, and peek-local bindings can all be overridden through config-backed bindings, but there is still no user-facing keymap editor in the command runner and the binding model is still limited to existing actions rather than an open-ended command surface.[12][13][40][46][47][49]

## 7. Command Runner And Floating Surfaces

- `[x]` Serenity has a floating command runner that opens beneath the cursor and restores focus when dismissed.[20][21]
- `[x]` Settings are grouped into nested panels with preview and focused submenu states rather than flattened rows.[17][20][32][40]
- `[x]` The command runner exposes settings for animation mode, cursor mode, background style, code font, text font, ligature shaping, blur radius, animation timing, animation steps, buffer font size, UI font size, and buffer language.[16][17][40]
- `[x]` Text-entry rows in the command runner support immediate typing, cancel/restore semantics, and word deletion.[17][40][41]
- `[x]` Long language/settings submenus now scroll so the selected item stays visible.[32]
- `[~][P1]` The command runner is now a strong unified settings/command surface, but it is still not a complete universal shell for every subsystem in the product vision.[7][17][32]

## 8. Fonts, Typography, And Content-Aware Layout

- `[x]` Serenity supports separate code and text font families, ligature configuration, buffer font size, and UI font size through `AppConfig` and command-runner settings.[16][17][18]
- `[x]` Runtime typography switches between code and text fonts based on content context. Today that means plain text and Markdown use the text font path; code-oriented languages use the code font path.[2][17][18][37][38]
- `[x]` Wrapped-line layout distinguishes monospaced and proportional text correctly, with fixed-advance behavior for true monospaced layout and measured layout when proportional advances or ligatures demand it.[19][37][39]
- `[~][P1]` Content-aware typography is still essentially a language-based split, not a richer semantic `code vs prose vs preview` model.[2][18][19]

## 9. Themes, Effects, And Animation

- `[x]` Theme support exists, including syntax colors and theme-aware rendering.[22][23]
- `[x]` Theme selection supports live preview while moving through the theme picker and restores the original theme on cancel.[24][25]
- `[x]` Theme interpolation exists for colour transitions between themes.[26]
- `[x]` Background styles include `Solid`, `Transparent`, `Frosted`, and `GlassLike`, with blur/sheen material behavior in the renderer.[16][27]
- `[x]` Overlay and character animation systems are present and configurable through app config and command-runner settings.[16][17][26]
- `[~][P1]` The aesthetic surface is strong, but the implemented effects library is still narrower than a broad preset system of UI materials and motion styles.[16][17][27]

## 10. Window Interactions

- `[x]` Serenity handles resize events and recomputes viewport size from the Swing canvas.[14][28]
- `[x]` Custom window chrome exists with drag, manual edge resize, minimise, maximise/restore, and close controls when `WindowChromeMode.Custom` is enabled.[16][28]
- `[x]` Graceful quit and dirty-buffer close workflow are covered by dedicated tests.[29]
- `[~][P1]` Window behavior is solid in Swing, but there is still no evident cross-platform abstraction for more advanced window management beyond the current desktop implementation.[28][29]

## 11. Pinned Panels And Layouts

- `[x]` Serenity supports pinned panels on the top, bottom, left, and right via `SurfacePresentation.Pinned` and `PanelPosition`.[21][30]
- `[x]` Different panel types can be pinned, resized, focused, replaced in-position, or unpinned.[21][30][31]
- `[x]` Multiple pinned panels can coexist when they occupy different sides, and layout reflows around them.[5][30]
- `[~][P1]` Only one pinned surface is kept per side at a time. The broader product shape of multiple panels per side is still not implemented.[30][31]
- `[~][P1]` Floating surfaces such as directory listing, outline, and diagnostics can be pinned into side panels, but not every floating surface type is pinnable.[31]

## 12. Panel Presets, Expansion, And Density Modes

- `[x]` Panels can be removed with `unpin`, and focus falls back to the editor when appropriate.[31]
- `[ ][P1]` No direct implementation evidence was found for expanding a side panel into a central editor-space view.[30][31]
- `[ ][P1]` No direct implementation evidence was found for standard preset panel/workspace configurations.[30][31]
- `[ ][P1]` No direct implementation evidence was found for an explicit minimalist/maximalist interface mode system.[16][17][30]

## 13. Information Bars And Floating Info Near The Cursor

- `[x]` Serenity already has above-cursor and below-cursor floating surface placement, including quick-info, symbol-definition, command runner, submenu, file search, and modal overlays.[5][20][32]
- `[~][P1]` Floating information surfaces above and below the cursor are real, but there is still no dedicated configurable pinned information bar beneath the cursor as a first-class feature separate from the general floating-surface system.[5][20][32]

## 14. IDE And Language-Aware Features

- `[x]` Syntax highlighting exists, but the parser remains intentionally simple rather than language-complete.[13][22][23]
- `[x]` LSP wiring exists for server resolution, connection startup, workspace-root detection, and `didOpen` / `didChange` / `didClose` notifications.[33][43]
- `[x]` LSP diagnostics flow back into app state and can be rendered in diagnostics surfaces.[31][33]
- `[~][P2]` Language detection and workspace-root markers exist, but the shipped LSP path is still configured through `LspUserConfig.empty`, which limits user control compared with a fuller IDE configuration surface.[33][43]
- `[ ][P2]` No direct implementation evidence was found for build, compilation, test running, debugging, or dependency resolution workflows.[33]
- `[ ][P2]` Surface types for quick info and symbol definitions exist, but no direct request path was found for hover/completion/definition/intellisense flows driven from the current LSP layer. Current LSP event handling is still diagnostics-oriented.[32][33]

## 15. Architecture And Maintainability

- `[x]` The architecture is strongly event-driven and reducer-oriented. `AppState` is the central model, features are split into focused reducers, and runtime concerns live around Cats Effect state management and streams.[3][20][33]
- `[x]` `StateManager` has now been split into composed behavior traits covering event pipeline, effects, workflows, viewport logic, file façade, editor façade, and surface façade responsibilities.[34][35][36]
- `[~][P1]` The architecture is much healthier than before, but there is still room to push more behavior into precise types and smaller algebras, especially around multi-cursor editing, richer workflows, and IDE request/response paths.[3][34][36]

## Prioritized Tick List

### P0: Editing Fundamentals

- `[~]` Finish true multi-cursor editing semantics across the remaining primary-cursor branches. Distinct-cursor whole-line copy/cut, overlapping word-delete parity, tab/delete-forward coverage, repeated vertical preferred-column / preferred-x preservation, and explicit select-all/find/modal-open semantics are now implemented; the remaining gap is broader parity across the rest of the single-cursor command set.[3][45]
- `[~]` Expand the find/replace family into a fuller workflow with clearer single-step and all-step behavior. `Replace Next` and `Replace All` now support both current-buffer and active-selection scope; the remaining gaps are broader result management and larger-scope workflows.[3][8]
- `[x]` Add richer mouse-driven text editing. Click placement, range extension, drag selection, double-click word selection, and triple-click line selection are now implemented.[14][15]
- `[~]` Continue the markdown direction intentionally. Editor-side structural styling and block-lens raw-source editing now exist; the remaining decision is whether to add a separate preview/document presentation behavior beyond the current same-metric editor lens.[19][22][23][32][37][50]
- `[~]` Continue the keymap direction beyond the new config-backed focused bindings. The runtime now supports editor/modal/panel/overlay-local overrides in addition to global hotkeys; the remaining gap is a user-facing keymap editing surface and any broader remapping of higher-level commands.[12][13][40][46][47][49]

### P1: UX Polish

- `[ ]` Add panel presets / workspace presets.[30][31]
- `[ ]` Support multiple panels per side rather than single replacement per side.[30][31]
- `[ ]` Add interface density / minimal-vs-maximal UI modes if still desired.[16][17][30]
- `[ ]` Revisit the command runner as a broader unified control surface once the editing fundamentals are settled.[17][32]
- `[ ]` Add hover affordances and context-menu style mouse workflows if those are still desired as part of broader UI polish.[14][15]

### P2: IDE Features

- `[ ]` Add user-facing LSP configuration and language tooling controls beyond the current built-in registry plus `LspUserConfig.empty` path.[33][43]
- `[ ]` Add request/response driven IDE interactions such as hover, completion, definition, and symbol navigation.[32][33]
- `[ ]` Add build/test/run/debug/dependency workflows if Serenity is meant to grow into a fuller IDE shell.[33]

## Summary

- Strongest implemented areas today: rope-backed editing, session restore/save, typography/layout correctness, nested command-runner overlays, theming/animation, and pinned-panel basics.[1][5][11][17][19][27]
- Biggest current gaps remain: fully finished multi-cursor editing, richer find/replace workflows, fuller keymap configurability beyond the current global override layer, UI-level mouse polish beyond editing interactions, multi-panel-per-side layouts, panel presets, and deeper IDE features beyond diagnostics-oriented LSP plumbing.[3][12][14][30][33][46][47]
- Biggest current gaps remain: fully finished multi-cursor editing, richer find/replace workflows, a user-facing keymap editing surface beyond the new config-backed bindings, richer Markdown preview/document rendering beyond the same-metric block lens, UI-level mouse polish beyond editing interactions, multi-panel-per-side layouts, panel presets, and deeper IDE features beyond diagnostics-oriented LSP plumbing.[3][12][14][30][33][40][46][47][49][50]
- Recommended implementation order from here: finish editing fundamentals first, then polish workflow/UX, then deepen IDE behavior.[3][14][17][30][33]

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
[34] `src/main/scala/com/serenity/state/manager/StateManagerRuntimeSupport.scala`
[35] `src/main/scala/com/serenity/state/manager/StateManager.scala`
[36] `src/main/scala/com/serenity/state/manager/StateManagerEffectBehavior.scala`
[37] `src/main/scala/com/serenity/ui/renderer/Renderer.scala`
[38] `src/test/scala/com/serenity/RendererFontIsolationSpec.scala`
[39] `src/test/scala/com/serenity/RendererProportionalRenderingSpec.scala`
[40] `src/main/scala/com/serenity/state/reducers/CommandRunnerReducer.scala`
[41] `src/test/scala/com/serenity/FocusedInputTranslatorSpec.scala`
[42] `src/main/scala/com/serenity/lsp/config/LanguageId.scala`
[43] `src/main/scala/com/serenity/lsp/config/LspServerRegistry.scala`
[44] `src/test/scala/com/serenity/lsp/LspQueueSpec.scala`
[45] `src/test/scala/com/serenity/CopyPasteSpec.scala`
[46] `src/main/scala/com/serenity/config/HotkeyConfig.scala`
[47] `src/main/scala/com/serenity/keystroke/translators/TextHotkeyConverters.scala`
[48] `src/main/scala/Main.scala`
[49] `src/main/scala/com/serenity/config/FocusedKeymapConfig.scala`
[50] `docs/renderer-capabilities.md`
