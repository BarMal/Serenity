# Features and functionality to either design and implement, debug and fix, or finish implementing

## Most tests are failing currently

We need to fix this as a matter of priority! Compilation and tests are slow, so do not skip/use timeouts/assume a test has passed because they initialised, you must be patient with the tests; if a test message warns of infix notation, that is not the actual issue, there's a type/compilation further up, which causes these to appear - ignore infix warnings and address the other failures/errors instead

## Cursor flashing while command runner is open bug fix

When the command panel is open, the cursor should only flash inside the command runner - currently it appears to flash at the last position it was on in the buffer, but it flashes over the top of the command runner

## Per-buffer state tracking (including pending animations, cursor position, cursor visibility, etc)

We are in the middle of a refactor to include this, my expectation is that the state manager keeps track of the current buffer that has focus; we should move animation/cursor position + visibility alongside the buffer into a containing class potentially (if that's not already the case); my expectations are:

- When dealing with multiple buffers, only the currently focused buffer's cursor flashes, the others do not show a cursor
- When dealing with multiple buffers with active selections, selections should be preserved and still shown for every buffer
- **NEW FUNCTIONALITY**: "page turn animation" when there is one pane but multiple buffers, navigating between buffers should trigger a new animation, every visible character should fade in as a vertical wave, moving from left to right, or right to left, depending on which direction the user navigated to the buffer with
- **NEW FUNCTIONALITY**: the top N and bottom N visible lines of all buffers should have the interpolated values applied, to achieve a "fading out of view" effect - this should be static, and should be taken into account with other animations (e.g. the "page turn animation")
- **EXTENSION OF EXISTING FUNCTIONALITY**: Java TextColor exposes a fromRgb method, which will allow us better precision of interpolation - let's migrate all interpolation/colouring to use this, and ensure that config also uses RGB values as well in support of this

## Mechanical restructure of tests

Currently we have a lot of tests where individual lines are effects wrapped in IO, and are therefore immediately executed with .unsafeRunSync() calls - we should instead compose these into an IO as a for-comprehension, and in the yield we should have our assertions - I have done this style of refactor for the BufferCoordinateAnimationSpec.scala - look at this pattern and apply it to the rest of the tests

We need to run all tests and make sure they all pass before continuing with any new features

## Command panel extensions

Currently the command panel is a flat list with limited visibility, no tabbing, and nothing appears to be wired up, here's my expectations

- Config defines how many elements to show in the list (this should default to 5 if no config exists)
- Beneath the search bar, there should be a horizontal tab-able categories header, All | File | Buffer | Editor | Code
  - All should contain all commands
  - File should include saving the current buffer to file, opening another file, other file-handling options, etc
  - Buffer should include Go to line, copy, paste, cut, search, replace, and similar commands
  - Editor should contain editor-related commands, switching between open buffers (this should be a drop-down list), clearing a buffer, switching to next/previous buffer
  - UI should contain commands to toggle visible UI elements, e.g. the gutter (see below for more details), the side-panels, etc, and it should be able to toggle between single-pane mode or multi-pane mode, i.e. whether the app should automatically create a new pane for a new buffer when there's enough space, or if the app should always use a single pane, we should also explore being able to make the editor font larger or smaller
  - Code should remain unimplemented for now - this will require significant new features - we could put the syntax highlighting toggle under here
  - If there's any additional commands I have missed either from implementation, or as additional suggestions, raise them with me please
- The command panel needs a full suite of tests to make sure that invoking it, navigating through the different elements, and selecting them actually trigger the correct commands/events to be fired
- The actual command panel events/commands need to be implemented
- There needs to be a file browsing modal that a user can use to navigate to a file to open
- Searching should have a find next/select all/replace all sub-menu modal
- Searching needs to move the viewport to the next found element, and should not close the command runner
- We should experiment with spell-checking - this will need full research/designing

## UI adjustments

Currently there are two blank panels either side of the pane area, the pane area can contain multiple panes, each with a title

I'd like to add some functionality that is togglable, both by command runner and by hotkey

- I'd like for there to be a gutter element that shows information, e.g. current col/row position, a breadcrumb-style trail of the current buffer's path - this should be stored/retrieved per buffer as well - while the buffer is unsaved to file, just use the text "Not saved to file yet"
- I'd like a line number indicator to run vertically along the buffer
- I'd like to explore a navigable dir/file explorer tree view

There are more code-specific elements I'd like in the future, but they will require more forethought and planning, and design upfront