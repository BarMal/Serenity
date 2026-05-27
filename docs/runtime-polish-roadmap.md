# Serenity Runtime Polish Roadmap

Last updated: 2026-05-27

This document captures the currently known runtime issues, UX gaps, and planned
extensions gathered from recent interactive testing. It is intentionally
forward-looking. For repository status as it exists today, see
`next-steps.md`.

## Purpose

We need a working implementation order for a set of runtime and UX issues that
are too broad to tackle in one pass. The goal is to:

1. fix correctness and shutdown/layout problems first
2. settle startup/session behavior before polishing the surrounding UX
3. improve file workflows and command surfaces after the structural pieces are
   stable
4. finish with richer theming, animation, and visual effects

## Ordering Principles

Work should be pulled in this order unless a dependency changes:

1. correctness and platform behavior
2. startup/session architecture
3. file safety and workflow UX
4. layout/pane control behavior
5. font/runtime presentation support
6. command surface and overlay polish
7. theme transitions
8. pane animation effects

## Backlog

### 1. Stability and Platform Fixes

These items make the app feel broken and should be handled before broader UX
work.

#### 1.1 Graceful shutdown from window close

- Problem:
  - closing the window with native decorations does not currently follow the
    normal application quit path
- Expected result:
  - window close should trigger the same graceful shutdown path as an explicit
    quit event
  - terminal resources should be closed cleanly
  - session persistence hooks should be able to participate later
- Notes:
  - likely touches terminal/window integration and app quit signaling

#### 1.2 Top-left black cell

- Problem:
  - one cell in the top-left corner remains black across themes
- Expected result:
  - the entire screen should respect the active theme background
- Notes:
  - likely a render-order or border/corner draw issue

#### 1.3 Resize and relayout correctness

- Problem:
  - some pane visibility transitions react correctly to resize while others do
    not
  - narrow-start or narrow-creation scenarios need explicit verification
- Expected result:
  - visible pane count and pane layouts should respond consistently to resize
  - no stale layout state after creating buffers or panes while constrained

### 2. Startup and Workspace Opening

These items define what the app feels like when it first opens.

#### 2.1 Support startup path arguments

- Problem:
  - passing a file or directory path to the app at startup is not yet part of
    the startup flow
- Expected result:
  - if a file path is passed, open that file
  - if a directory path is passed, initialize the workspace around that
    directory

#### 2.2 Start without an editor buffer

- Problem:
  - the app currently boots straight into one empty buffer and one pane
- Expected result:
  - default startup should show the command runner first
  - no editor buffer should exist until the user creates or opens one

#### 2.3 Initial command runner should be required

- Problem:
  - if startup becomes command-runner-first, closing the runner immediately
    would leave the app in a degenerate empty state
- Expected result:
  - the startup command runner should not be dismissible until a buffer exists

#### 2.4 Startup precedence rules

- Problem:
  - startup behavior will need explicit rules once both session restore and path
    arguments exist
- Expected result:
  - documented precedence between:
    - startup path arguments
    - restored session
    - default empty startup runner

### 3. Session Persistence

These features are structural and should be settled before startup behavior is
considered finished.

#### 3.1 Serialize session state

- Include:
  - open buffers
  - file paths
  - pane assignments
  - active/focused pane or buffer
  - relevant UI/workspace state that should survive restart

#### 3.2 Restore last session

- Expected result:
  - reopening the app can restore the last session when configured or desired

#### 3.3 Session persistence policy

- Define:
  - when the session is written
  - what counts as recoverable state
  - whether scratch/unsaved buffers are persisted, restored as scratch, or
    excluded

### 4. File Workflow UX and Safety

The file workflows function today, but they still feel awkward in use.

#### 4.1 Refine open/save/save-as interaction flow

- Improve:
  - field flow
  - suggestion behavior
  - confirmation behavior
  - status and failure messaging
  - default focus and keyboard ergonomics

#### 4.2 Refine file operation panel layout

- Improve:
  - density
  - spacing
  - hierarchy of path, filename, suggestions, and status information

#### 4.3 External change detection

- Problem:
  - saving can overwrite file changes made outside the app
- Expected result:
  - hash file contents on open
  - refresh/compare against on-disk contents before save
  - warn when the external version no longer matches the last known opened or
    saved content
- Decision needed:
  - exact overwrite flow for conflict resolution

### 5. Layout and Pane Controls

This work clarifies editor structure rather than startup or file interaction.

#### 5.1 Forced single-pane mode

- Expected result:
  - configuration or command path that keeps the editor in one-pane mode even
    when width allows more

#### 5.2 Clarify buffer vs pane behavior

- Problem:
  - creating a new buffer is not the same as creating a new pane, but the UX
    should make this distinction clearer
- Expected result:
  - explicit and predictable semantics for:
    - new buffer
    - new pane
    - pane rebalancing
    - pane visibility under constrained width

### 6. Font Support

These are currently blocked less by design than by incomplete terminal
integration.

#### 6.1 Apply configured custom fonts

- Expected result:
  - configured fonts should actually be used by the GUI terminal

#### 6.2 Dynamic font resizing

- Expected result:
  - change font size at runtime
  - relayout should react cleanly
  - font size should have a persistence story

### 7. Command Runner and Overlay Polish

This is the first strongly visual polish pass after the structural issues.

#### 7.1 Refine overlay borders

- Problem:
  - command runner and file panels still look segmented
- Expected result:
  - borders read as one continuous surface

#### 7.2 Sliding command-runner highlight

- Expected result:
  - highlight/selection movement should animate or visually track between
    elements rather than snapping abruptly

#### 7.3 General overlay polish

- Improve:
  - row emphasis
  - selected-state continuity
  - status visibility
  - visual cohesion across command runner and workflow panels

### 8. Theme Transition Work

This is separate from static theme token support, which is already present.

#### 8.1 Smooth color interpolation on theme change

- Expected result:
  - theme changes transition smoothly rather than snapping instantly

#### 8.2 Theme transition policy

- Decide:
  - which semantic theme tokens interpolate
  - which should snap immediately
  - which background color should act as the interpolation base

### 9. Pane and Content Effects

These are rich visual enhancements that should come after theme transitions.

#### 9.1 Top-N / bottom-N fade interpolation on panes

- Expected result:
  - pane edges can fade rather than ending abruptly

#### 9.2 Page-turn animation

- Expected result:
  - explicit page-turn effect when navigating content in a way that suits the
    editor model

## Recommended Implementation Order

Use this order for the first passes:

1. graceful shutdown from window close
2. top-left black cell
3. resize and relayout correctness
4. startup path arguments
5. startup with no buffer and required initial command runner
6. session serialization and restore
7. file workflow UX refinement
8. external change hashing and overwrite warning
9. forced single-pane mode and pane semantics cleanup
10. actual font application
11. dynamic font resizing
12. overlay border and command-runner polish
13. theme-change interpolation
14. pane fade and page-turn effects

## Pull Strategy

Each item above should be implemented as a narrow slice:

1. write or update tests first
2. land one behavioral change at a time
3. re-read affected files before each new slice
4. keep this roadmap updated when priorities or dependencies shift

## First Slice

The first implementation slice should be:

- graceful shutdown from window-decoration close

Reason:

- it is a correctness issue
- it affects application lifecycle
- it is a prerequisite for reliable session persistence later
