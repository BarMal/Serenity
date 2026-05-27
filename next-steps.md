# Serenity - Current State and Next Steps

Last updated: 2026-05-26

This document is meant to reflect the repository as it exists now.
It is not a historical summary. If the code and this file disagree, the code wins and this
file should be updated.

## Current build status

Latest verified command:

`sbt test`

Current result:

- main source compile succeeds
- test source compile succeeds
- 16 tests are pending

The build is green.

## What is implemented

### Main loop and rendering

- The unified idle/fast render loop is already implemented in `Main.scala`
- Idle mode uses a 500 ms cursor-only render tick
- Fast mode uses a 16 ms full-render tick
- Resize checks are integrated into the loop through `RenderController.handleResize`
- `Renderer.renderCursorOnly` exists and is used during idle cursor blinking

### Input flow

- `ScreenInputHandler` blocks on `screen.readInput()`
- `InputRouter` translates keystrokes through the active translator
- `TextEntryTranslator` handles text input, navigation, and hotkeys
- EOF is translated to `Quit`

### Core editor state

- `StateManager` owns the application `Ref` and routes events by focus
- Startup state creates one pane and one empty buffer
- Buffer ordering exists through `AppState.bufferOrder`
- Global next/previous buffer navigation exists
- Multi-buffer assignment to panes based on available width exists

### Rendering and UI

- Pane headers render active/inactive state
- Wrapped line rendering exists
- Line numbers exist and are configurable
- Bottom gutter exists and is configurable
- Floating UI surfaces are rendered through a shared surface/content resolver path
- Command runner is a floating surface, not a separate renderer path
- Command runner search row, selected row highlight, descriptions, footer metadata, and blinking cursor all render through the shared floating overlay model
- Command runner supports category-tab browsing when search is empty
- Typing in the command runner switches to global search across all command categories
- Settings-style inline option rows exist for command-runner surfaces, including animation mode
- Tab and Shift+Tab navigate command-runner categories when search is empty
- Left and right adjust inline settings options inside the command runner
- Editor cursor is suppressed when focus is not on an editor pane

### Command runner

- Command runner activation and dismissal work
- Search/filter and selection movement work
- Escape and successful command execution remove the command-runner surface entirely rather than leaving an inactive shell behind
- Left/right category browsing works when the command-runner search box is empty
- Animation mode is exposed as an inline settings row rather than separate runner commands
- Toggle UI commands are wired through `StateManager`
- `toggle-line-numbers` works
- `toggle-gutter` works

### Modal behaviors that exist

- Go to line modal has concrete behavior
- Find modal has concrete behavior for initial search
- Find next behavior exists through `FindState`

## What is only partially implemented

### Surface interactions

The shared UI-surface architecture is in place, but interactive surfaces are still incomplete.

Current limitations:

- direct hotkeys for opening specific command categories are not implemented yet
- command-runner key handling covers search, movement, enter, escape, category switching, and inline option adjustment, but not richer per-surface navigation beyond that yet
- some command intents are still placeholders rather than real editor actions

### Command system

The command runner shell exists, and several core commands are now real, but some default commands are still placeholders.

Examples:

- `save`
- `quit`
- `find`
- `goto-line`
- animation mode commands

Current reality:

- `save` works
- `save-as` opens a real file workflow modal and can complete a save
- `open` opens a real file workflow modal and can load a file
- `close`, `close-all`, `close-others`, and `quit` now route through an unsaved-changes workflow when needed
- `find` works
- `replace` works as a replace-all workflow in the focused buffer
- `goto-line` works
- `toggle-theme` and `reload-theme` work
- some commands such as formatting-related actions are still placeholders

### Modal and workflow surfaces

Some workflow surfaces have real logic, but the broader workflow layer is still incomplete.

Still incomplete:

- file search / quick open behavior
- richer custom-surface interaction flows

### Test modernization

Some tests have been refactored toward composed `IO` programs with a single
`unsafeRunSync()` at the end, but this is nowhere near complete.

Current rough signal:

- there are still hundreds of `unsafeRunSync()` call sites across the test tree

The methodology exists, but the refactor is not complete.

## What is not implemented

### File workflow

- recent files persistence
- richer file-browser style navigation and quick-open behavior

### Explorer and panels

- navigable directory tree
- panel interaction and navigation
- panel refresh/update behavior
- panel resize behavior
- drag file to directory behavior

### Editor features

- select all
- richer search UX
- line wrapping toggle
- explicit viewport movement commands
- snap viewport to cursor
- snap cursor to viewport
- configurable single-pane versus auto-multi-pane editor mode
- font size commands

### Session features

- session serialization
- session restore

## Known design and architecture issues

### 1. Interactive surface depth

The shared surface model now exists for floating and pinned UI, but richer interactive
behaviors still need to be layered onto it consistently.

### 2. StateManager is doing too much

`StateManager` currently acts as:

- event dispatcher
- state store
- validation boundary
- buffer service
- pane service
- tab orchestration layer
- resize coordinator
- host for many placeholder APIs

This makes it hard to reason about responsibilities and makes partial implementation easy
to hide.

### 3. Layout and rendering are still tightly coupled

`LayoutEngine` is separate from `Renderer`, which is good, but layout still depends heavily
on full `AppState`, and some presentation decisions remain split between layout and render
code.

### 4. Hard-coded ANSI colors remain widespread

RGB parsing and interpolation support exist, and bundled themes already use hex values in
theme files, but much of the renderer and internal theme defaults still use `TextColor.ANSI`
directly.

### 5. Dead code and placeholders remain

There are still many TODOs, placeholder methods, and print-based stand-ins across the main
codebase. This should be treated as active debt, not hidden future work.

## Known behavioral issues

### 1. Line numbers do not model wrapped visual lines correctly

Line numbers are based on buffer line count, while rendering supports wrapped visual lines.
This means wrapped content can visually drift from line-number expectations.

### 2. Command surfaces are still behavior-light

The command runner now renders correctly again, but several command intents and richer
surface interactions remain incomplete.

## Recommended next steps

### Priority 1 - finish real command behavior

Implement actual command actions for:

- save
- find
- goto-line
- editor mode / UI commands beyond line numbers and gutter
- richer inline option-style command surfaces
- direct category hotkeys for command-runner entry points

Now mostly remaining in this area:

- formatting/editor actions that still log or no-op
- direct category hotkeys

### Priority 2 - complete file and workflow surfaces

Implement:

- file browser / quick open
- search behaviors that keep command runner open where intended
- richer file-modal navigation polish and suggestion behavior

### Priority 3 - keep tightening the architecture

- continue reducing `StateManager`
- remove adapter vocabulary when the shared surface model fully replaces it
- move more colors to semantic theme-driven values
- keep layout geometry separate from surface content resolution

## Notes for future updates

When updating this file:

- do not write "all tests pass" unless the full sbt command was run successfully
- do not call something "fully implemented" if core branches still log or no-op
- distinguish between "wired", "partially implemented", and "behavior complete"
- prefer short factual status over long retrospective narrative
