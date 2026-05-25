# Serenity - Current State and Next Steps

Last updated: 2026-05-25

This document is meant to reflect the repository as it exists now.
It is not a historical summary. If the code and this file disagree, the code wins and this
file should be updated.

## Current build status

Latest verified command:

`sbt test`

Current result:

- main source compile succeeds
- test source compile succeeds
- 402 tests run
- 394 tests pass
- 8 tests fail
- 16 tests are pending
- 69 test warnings are reported, mostly unused imports

Current failing suites and issues:

- `GutterAndLineNumbersSpec`
  - path separator expectation mismatch: Windows-style `\` vs Unix-style `/`
- `CommandRunnerBehaviorSpec`
  - selected command execution is not producing the expected state change
- `MultiFileTabSpec`
  - session-path expectation mismatch: Windows-style `\` vs Unix-style `/`
- `ResizeHandlingSpec`
  - resize expectations no longer match current calculated widths/heights
- `LayoutEngineSpec`
  - expected pane widths/positions do not match current layout engine behavior

This means the build is not green, but the current problem is concentrated in a small set
of behavioral mismatches rather than broad compile failure.

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
- Command runner overlay exists and is rendered at a fixed top-center position
- Editor cursor is suppressed when focus is not on an editor pane

### Command runner

- Command runner activation and dismissal work
- Search/filter and selection movement work
- Toggle UI commands are wired through `StateManager`
- `toggle-line-numbers` works
- `toggle-gutter` works

### Modal behaviors that exist

- Go to line modal has concrete behavior
- Find modal has concrete behavior for initial search
- Find next behavior exists through `FindState`

## What is only partially implemented

### Per-buffer versus per-pane versus app-level state

This area is not settled yet.

Current duplication:

- `Buffer` stores `cursors`, `viewport`, and `animations`
- `EditorPane` also stores `cursors` and `viewport`
- `AppState` also stores `screenAnimations`

Current consequence:

- renderer reads cursor and viewport from `Buffer`
- resize logic updates `EditorPane.viewport`
- insert-character logic updates both `buffer.animations` and `state.screenAnimations`
- fast render loop termination logic still checks `state.screenAnimations`
- animation advancement advances `buffer.animations`, not `state.screenAnimations`

This is the main architectural inconsistency in the codebase right now.

### Command system

The command runner shell exists, but most default commands are still placeholders.

Examples:

- `save`
- `save-as`
- `open`
- `quit`
- `find`
- `replace`
- `goto-line`
- animation mode commands

Most of these currently just print messages rather than performing real editor actions.

### Modal system

Some modals have real logic, but the general modal framework is incomplete.

Still incomplete:

- command-palette modal text flow
- file search modal behavior
- quick open modal behavior
- generic modal navigation and action execution
- replace workflow

### Test modernization

Some tests have been refactored toward composed `IO` programs with a single
`unsafeRunSync()` at the end, but this is nowhere near complete.

Current rough signal:

- there are still hundreds of `unsafeRunSync()` call sites across the test tree

The methodology exists, but the refactor is not complete.

## What is not implemented

### File workflow

- open-file browser
- save-as flow
- recent files persistence
- unsaved-changes confirmation flow

### Explorer and panels

- navigable directory tree
- panel interaction and navigation
- panel refresh/update behavior
- panel resize behavior
- drag file to directory behavior

### Editor features

- replace workflow
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

### 1. State duplication

The biggest issue is duplicated ownership of cursor, viewport, and animation state across
`Buffer`, `EditorPane`, and `AppState`.

This should be reduced so the renderer, resize handling, and animation ticking all use one
authoritative source.

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

### 2. Animation ownership is inconsistent

The visible render path uses `buffer.animations`, while loop coordination still reasons
about `state.screenAnimations`.

### 3. Build is currently red

The immediate blocker is now a focused set of 8 failing tests across 5 suites.

## Recommended next steps

### Priority 1 - restore a trustworthy build

1. Fix the current failing tests in:
   - `GutterAndLineNumbersSpec`
   - `CommandRunnerBehaviorSpec`
   - `MultiFileTabSpec`
   - `ResizeHandlingSpec`
   - `LayoutEngineSpec`
2. Decide whether the path-format failures should be normalized in code or corrected in tests
3. Reconcile `ResizeHandlingSpec` and `LayoutEngineSpec` with the current layout calculations
4. Reconcile command-runner execution expectations with the current command model
5. Re-run `sbt clean scalafix scalafmt test`
6. Do not mark any work complete again until the build is green

### Priority 2 - resolve state ownership

Choose one authoritative owner for:

- cursor position
- viewport
- animations

Recommended direction:

- buffer-local text state lives on `Buffer`
- pane-local presentation/layout state lives on `EditorPane`
- transient loop coordination stays outside `AppState`
- remove either `AppState.screenAnimations` or `buffer.animations`; do not keep both

### Priority 3 - tighten architecture after behavior is stable

After the build is green and state ownership is fixed:

- simplify `StateManager`
- reduce placeholder APIs
- clean dead code
- revisit layout abstraction boundaries
- move more colors to semantic theme-driven values

### Priority 4 - finish real command behavior

Implement actual command actions for:

- save
- save-as
- open
- find
- replace
- goto-line
- editor mode / UI commands beyond line numbers and gutter

### Priority 5 - complete file and modal workflows

Implement:

- file browser / quick open
- save-as dialog flow
- replace flow
- command categories if still desired
- search behaviors that keep command runner open where intended

## Notes for future updates

When updating this file:

- do not write "all tests pass" unless the full sbt command was run successfully
- do not call something "fully implemented" if core branches still log or no-op
- distinguish between "wired", "partially implemented", and "behavior complete"
- prefer short factual status over long retrospective narrative
