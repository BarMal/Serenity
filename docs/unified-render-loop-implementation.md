# Unified Variable-Rate Render Loop — Implementation Spec

## Purpose

This document is a self-contained implementation brief. Phase 1 investigation and Phase 2
design have already been completed in a prior session. Read this document fully before
writing any code, then implement the numbered changes in order.

---

## Project context

**Serenity** is a Scala 3 terminal text editor built with Cats Effect and FS2.

| Concern | Library / version |
|---|---|
| Language | Scala 3.8.3 |
| Effect system | Cats Effect 3.7 (`IO`, `Ref`, `Deferred`, `Resource`) |
| Streaming | FS2 3.13.0-M2 (`Stream[IO, A]`, `SignallingRef`, `Pipe`) |
| Terminal | Lanterna 3.2.0 (`Screen`, `TerminalScreen`) |
| Build | sbt |
| Tests | ScalaTest 3.2.19 (`AnyFlatSpec`, `AnyFunSpec`) |

**Coding conventions** (enforce throughout):
- Scala 3 syntax: indentation blocks, `given`/`using`, `enum`, extension methods
- No `var`, no `null`, no `throw` — use `Option`, `Either`, `IO.raiseError`
- All mutations go through `Ref` or `IO`; pure functions for logic
- TDD: write or update the relevant test **before** writing implementation code
- No `TODO`, no stubs, no `???` in delivered code
- Never delete or skip a failing test — fix the code

**Key source files** (read each before modifying it):

```
src/main/scala/Main.scala
src/main/scala/com/serenity/state/manager/StateManager.scala
src/main/scala/com/serenity/input/TerminalInputHandler.scala
src/main/scala/com/serenity/ui/renderer/Renderer.scala
src/main/scala/com/serenity/animation/AnimationState.scala
src/main/scala/com/serenity/state/components/ResizeComponent.scala
src/main/scala/com/serenity/ui/layout/LayoutEngine.scala
```

---

## Problem being solved

At idle the JVM process consumes ~15% CPU. Three concurrent streams are responsible:

1. **Resize busy-wait** (`TerminalInputHandler.scala`):  
   `Stream.repeatEval(checkForResize).unNone` polls `screen.doResizeIfNecessary()` — a
   near-instant non-blocking call — with zero sleep between iterations. Every call wraps
   in `IO.blocking`, generating continuous fiber park/unpark churn on the blocking thread
   pool at thousands of iterations per second.

2. **Unconditional 30 FPS render loop** (`Main.scala`):  
   `Stream.fixedRate[IO]((1000.0/30).millis)` fires a full screen rebuild 30 times per
   second regardless of whether anything changed. Each frame allocates a `TextGraphics`
   object, fills the entire back buffer with spaces, runs a character-by-character
   tokeniser (`ThemeManager.highlightLine`) on every visible line, performs per-character
   `setForegroundColor`/`setBackgroundColor` calls, and calls `screen.refresh()`.

3. **Unconditional 60 FPS animation tick** (`Main.scala`):  
   `Stream.fixedRate[IO](16.millis)` fires `advanceAnimationsOnTick()` 60 times per
   second. Even when `screenAnimations` is `Map.empty`, every tick reads the `Ref`,
   allocates intermediate `Map` copies, and writes the `Ref` back via CAS.

Additionally, `Renderer.render` calls `updateViewportDimensions` on every frame and
produces a new `AppState` copy that is immediately discarded. `ResizeComponent` already
persists correct viewport dimensions to state when a `ResizeEvent` is applied — the
renderer's copy is entirely redundant.

---

## Chosen design

Replace the three streams with a single **unified variable-rate render loop** whose tick
rate is determined by whether user input has recently arrived.

### Coordination primitives (local to `Main`, never part of `AppState`)

```scala
fastMode:      SignallingRef[IO, Boolean]   // false = idle, true = fast
cursorVisible: Ref[IO, Boolean]             // current blink state
```

Neither of these is application state. They are ephemeral operational refs that would be
discarded if the render loop restarted. Do **not** add them to `AppState`.

### The loop structure

```
[idlePhase]  500 ms ticks, cursor-only render
     |
     | (fastMode becomes true — set by inputFunnel when any key is pressed)
     ↓
[fastPhase]  16 ms ticks, advance animations + full render
     |
     | (advanceAnimationsOnTick returns false — no remaining animations)
     ↓
[back to idlePhase]
```

Expressed as a recursive FS2 stream:

```scala
def renderLoop: Stream[IO, Unit] = Stream.defer {
  idlePhase ++ fastPhase ++ renderLoop
}
```

`idlePhase` terminates when `fastMode.discrete` emits `true` (via `interruptWhen`).  
`fastPhase` terminates when animations drain (via `takeWhile`), then resets `fastMode` 
to `false` via `onFinalize`.

### Input funnel (the `through`)

The input event stream is threaded through a `Pipe[IO, Event, Nothing]` that both applies
the event to state **and** sets `fastMode = true`. This is the sole writer of the forward
switch — nothing else sets `fastMode` to `true`.

### Resize detection

Folded into both phases via a shared `checkResize: IO[Unit]` helper that calls
`screen.doResizeIfNecessary()` once per tick. At 16 ms (fast phase) this is imperceptible;
at 500 ms (idle phase) it is also acceptable. The busy-wait loop is eliminated entirely.

---

## Implementation plan

Implement these changes in order. Re-read each file immediately before editing it.

---

### Change 1 — `StateManager`: change `advanceAnimationsOnTick` to return `IO[Boolean]`

**File:** `src/main/scala/com/serenity/state/manager/StateManager.scala`

**Why:** `fastPhase` uses `takeWhile(identity)` on the stream produced by `evalMap`. The
`evalMap` block must therefore yield a `Boolean` — `true` while animations remain active,
`false` when they are exhausted. The current return type `IO[Unit]` makes this impossible.

**Trait change** — update the method signature in the `StateManager` trait:

```scala
// Before
def advanceAnimationsOnTick(): IO[Unit]

// After
def advanceAnimationsOnTick(): IO[Boolean]  // true = animations still active after this tick
```

**Implementation change** — in `StateManagerImpl`:

```scala
def advanceAnimationsOnTick(): IO[Boolean] =
  stateRef.get.flatMap { state =>
    if !state.screenAnimations.hasActiveAnimations then IO.pure(false)
    else
      val newAnimations = state.screenAnimations.advanceAllAnimations()
      stateRef.set(state.copy(screenAnimations = newAnimations)).as(
        newAnimations.hasActiveAnimations
      )
  }
```

Key points:
- Guard on `hasActiveAnimations` — if the map is empty, skip all work and return `false`
  immediately. This eliminates the 60 Hz `Ref` churn + `Map` allocations at idle.
- After advancing, check the NEW state's `hasActiveAnimations` (not the old one). An
  animation may have just completed on this tick; the method should reflect that.
- Return `true` only if animations remain in the updated state.

**Test first** — write a test in a suitable spec (e.g. `AnimationIntegrationSpec` or a
new `StateManagerAnimationSpec`) before implementing:

```scala
"advanceAnimationsOnTick" should "return false immediately when no animations are active" in {
  // create StateManager with empty animation state
  // call advanceAnimationsOnTick()
  // assert result is false
}

it should "return true while animations are still in flight" in {
  // add character animations to state
  // call advanceAnimationsOnTick() once
  // assert result is true (animations not yet exhausted)
}

it should "return false when the final animation step completes" in {
  // add animation with steps = 1
  // call advanceAnimationsOnTick() once
  // assert result is false
}
```

---

### Change 2 — `ScreenInputHandler`: remove the resize polling stream

**File:** `src/main/scala/com/serenity/input/TerminalInputHandler.scala`

**Why:** Resize detection moves into the per-tick `checkResize` helper in the render loop
(Change 5). The busy-wait `resizeStreamEvents` is the single largest CPU contributor at
idle and must be removed entirely.

**Changes:**

1. Delete the `resizeStreamEvents` and `checkForResize` private methods.
2. Change `eventStream` to return only key input:

```scala
def eventStream: Stream[F, Event] =
  keyStreamEvents   // was: keyStreamEvents.mergeHaltR(resizeStreamEvents)
```

The `mergeHaltR` call, the `ResizeEvent` import (if it becomes unused), and the
`TerminalSize` import (if unused) should also be removed. Run `scalafix` after.

**No new test needed** — resize handling is tested end-to-end in `ResizeHandlingSpec`,
which tests `stateManager.applyEvent(ResizeEvent(...))` directly and does not rely on
`ScreenInputHandler` internals. Those tests remain valid and must still pass.

---

### Change 3 — `Renderer`: add `renderCursorOnly`; remove `updateViewportDimensions`; parameterise cursor visibility

**File:** `src/main/scala/com/serenity/ui/renderer/Renderer.scala`

#### 3a — Remove `updateViewportDimensions` from `render`

`Renderer.render` currently calls `updateViewportDimensions` and produces a new
`AppState` copy that is only used for that single render call and never persisted.
`ResizeComponent` already keeps viewport dimensions correct in the real state whenever a
`ResizeEvent` is applied. Remove the dead computation:

```scala
// Delete this line from render():
val updatedState = updateViewportDimensions(state, layout)

// And replace all uses of updatedState below it with state
```

The private `updateViewportDimensions` method can be deleted entirely.

#### 3b — Parameterise cursor visibility

The cursor blink decision currently lives inside `renderCursors`:

```scala
val currentTime      = System.currentTimeMillis()
val shouldShowCursor = (currentTime / 500) % 2 == 0
```

The render loop now owns cursor visibility state (via `cursorVisible: Ref[IO, Boolean]`).
Remove the `System.currentTimeMillis()` call and accept `cursorVisible` as a parameter:

```scala
// Before
def render(state: AppState, screen: Screen): Unit

// After
def render(state: AppState, cursorVisible: Boolean, screen: Screen): Unit
```

Thread `cursorVisible` down through `renderEditorPane` → `renderCursors`, replacing the
`shouldShowCursor` local variable with the parameter.

#### 3c — Add `renderCursorOnly`

A new method that updates only the cursor cell in Lanterna's back buffer and calls
`screen.refresh()`. Because Lanterna's `Screen` maintains a persistent back buffer between
calls to `newTextGraphics()`, and we do not clear it (no `fillRectangle`), `screen.refresh()`
will diff only the changed cell and write a tiny number of bytes to the terminal.

```scala
def renderCursorOnly(state: AppState, cursorVisible: Boolean, screen: Screen): Unit =
  val graphics     = screen.newTextGraphics()
  val terminalSize = TerminalSize(screen.getTerminalSize.getColumns, screen.getTerminalSize.getRows)
  val layout       = LayoutEngine.calculateLayout(state, terminalSize)
  val rect         = layout.editorPanelRect

  // Find the active pane and its cursor
  for
    paneId <- state.layout.activeEditorPaneId
    pane   <- state.layout.editorPanes.get(paneId)
    cursor <- pane.cursors.headOption
    buffer <- pane.bufferId.flatMap(state.buffers.get)
  do
    calculateCursorVisualPosition(cursor, buffer.content, rect.width, pane.viewport) match
      case Some((visualLine, visualColumn)) =>
        val screenY = rect.y + (visualLine - pane.viewport.topLine)
        val screenX = rect.x + visualColumn

        if screenY >= 0 && screenY < terminalSize.height &&
           screenX >= 0 && screenX < terminalSize.width
        then
          if cursorVisible then
            graphics.setBackgroundColor(TextColor.ANSI.WHITE)
            graphics.setForegroundColor(TextColor.ANSI.BLACK)
            CharacterRenderer.renderChar(graphics, screenX, screenY, ' ')
          else
            // Restore the character that sits beneath the cursor
            val charBeneath =
              buffer.content
                .getLine(cursor.line)
                .map(line => if cursor.column < line.length then line(cursor.column) else ' ')
                .getOrElse(' ')
            graphics.setBackgroundColor(state.theme.backgroundColor)
            graphics.setForegroundColor(state.theme.foregroundColor)
            CharacterRenderer.renderChar(graphics, screenX, screenY, charBeneath)

      case None => ()  // cursor not in viewport, nothing to toggle

  screen.refresh()
```

Note: `calculateCursorVisualPosition` is already a private method in `Renderer`. It
traverses the rope to compute the cursor's visual (screen) position. Use it as-is — do
not duplicate it.

**Test first** — before implementing `renderCursorOnly`, write a test that verifies the
method does not throw when called with an empty buffer and with a buffer containing text.
(Full integration testing of the pixel output is not required — focus on the contract.)

---

### Change 4 — `Main`: replace three streams with the unified render loop

**File:** `src/main/scala/Main.scala`

This is the primary change. Read the full existing `Main.scala` before editing.

#### 4a — New imports

```scala
import fs2.concurrent.SignallingRef
import cats.effect.Ref
import scala.concurrent.duration.*
```

Remove the imports for anything that no longer exists after this change
(`animationTickStream`, `renderingStream`, etc.). Run `scalafix` after.

#### 4b — Coordination refs

Add these two lines to the `for` comprehension in `run`, after `stateManager` is created
and before the streams are defined:

```scala
fastMode      <- SignallingRef.of[IO, false]
cursorVisible <- Ref.of[IO, true]
```

These are the only two pieces of coordination state. They do not belong in `AppState`.

#### 4c — Resize helper

```scala
val checkResize: IO[Unit] =
  IO.blocking(Option(screen.doResizeIfNecessary())).flatMap {
    case None => IO.unit
    case Some(lanternaSize) =>
      stateManager.applyEvent(
        com.serenity.keystroke.events.ResizeEvent(
          com.serenity.ui.layout.TerminalSize(
            lanternaSize.getColumns,
            lanternaSize.getRows
          )
        )
      )
  }
```

#### 4d — Input funnel

```scala
val inputFunnel: fs2.Pipe[IO, com.serenity.keystroke.events.Event, Nothing] =
  _.evalMap { event =>
    stateManager.applyEvent(event) >> fastMode.set(true)
  }.drain
```

This is the sole place that sets `fastMode` to `true`. It replaces the inline
`stateManager.applyEvent(event)` call that previously lived in the `evalMap` of the input
stream.

#### 4e — Idle phase

```scala
def idlePhase: Stream[IO, Unit] =
  Stream
    .fixedRate[IO](500.millis)
    .interruptWhen(fastMode.discrete)
    .evalMap { _ =>
      for
        _       <- checkResize
        visible <- cursorVisible.updateAndGet(!_)
        state   <- stateManager.getCurrentState
        _       <- IO.blocking(Renderer.renderCursorOnly(state, visible, screen))
      yield ()
    }
```

Key points:
- `interruptWhen(fastMode.discrete)` — `discrete` emits the current value of `fastMode`
  whenever it changes. `interruptWhen` halts `idlePhase` the moment `true` is emitted.
  Because `Signal.discrete` emits the current value on every new subscription, a `true`
  that arrived in the gap between the previous `fastPhase` completing and the new
  `idlePhase` subscribing is not lost.
- `cursorVisible.updateAndGet(!_)` — toggles and returns the new value atomically.
- Resize is checked on every 500 ms tick. This replaces the busy-wait.

#### 4f — Fast phase

```scala
def fastPhase: Stream[IO, Unit] = Stream.defer {
  Stream
    .fixedRate[IO](16.millis)
    .evalMap { _ =>
      for
        _       <- checkResize
        active  <- stateManager.advanceAnimationsOnTick()
        state   <- stateManager.getCurrentState
        _       <- IO.blocking(Renderer.render(state, cursorVisible = true, screen))
      yield active
    }
    .takeWhile(identity)  // terminates when advanceAnimationsOnTick() returns false
    .void
    .onFinalize {
      // Only reset fastMode if no new animations were added while this phase was running.
      // Without this guard, a keystroke arriving in the sub-millisecond window between
      // takeWhile deciding to stop and onFinalize executing would be silently dropped:
      // fastMode.set(false) would overwrite the true that inputFunnel just wrote, and the
      // new character would sit unrendered until the next keypress.
      stateManager.getCurrentState.flatMap { state =>
        if state.screenAnimations.hasActiveAnimations then IO.unit
        else fastMode.set(false)
      }
    }
}
```

Key points:
- `evalMap` must use a `for` comprehension so the `Boolean` from
  `advanceAnimationsOnTick()` is preserved for `takeWhile`. The naive
  `advanceAnimationsOnTick() >> render()` would discard the `Boolean` and the stream
  would never terminate.
- Cursor is always shown as visible during the fast phase. There is no blinking while
  typing — this is intentional and consistent with common editor behaviour.
- `takeWhile(identity)` — stops before emitting the element that caused the `false`. The
  `render()` call in `evalMap` happens before `takeWhile` sees the result, so the final
  frame (with all animations complete) is always painted.
- `onFinalize` — the conditional reset is the fix for the `onFinalize` race described
  below in the Wrinkles section.
- `Stream.defer` — prevents eager evaluation during definition. Required for safety even
  though `fastPhase` is not directly recursive.

#### 4g — Render loop

```scala
def renderLoop: Stream[IO, Unit] = Stream.defer {
  idlePhase ++ fastPhase ++ renderLoop
}
```

Read this as: run idle until interrupted → run fast until animations drain → repeat.
`Stream.defer` is required to prevent infinite recursion at definition time. FS2
trampolines this internally; it is stack-safe regardless of iteration count.

#### 4h — Wire up `parMapN`

Replace the existing `parMapN` block:

```scala
// Remove:
_ <- (
  inputHandler.eventStream
    .evalMap { event => ... }
    .compile.drain,
  animationTickStream.compile.drain,
  renderingStream.compile.drain,
  stateManager.awaitQuit
).parMapN((_, _, _, _) => ())

// Replace with:
_ <- (
  inputHandler.eventStream
    .through(inputFunnel)
    .compile.drain,
  renderLoop.compile.drain,
  stateManager.awaitQuit
).parMapN((_, _, _) => ())
```

The `animationTickStream` and `renderingStream` `val`s should be deleted entirely.
The `logSelectiveEvents` call that was inside the old input `evalMap` must be preserved —
move it into `inputFunnel` or add it as an `evalTap` before `inputFunnel`:

```scala
inputHandler.eventStream
  .evalTap { event =>
    stateManager.getCurrentState.flatMap(s => logSelectiveEvents(event, s.focus, logger))
  }
  .through(inputFunnel)
  .compile.drain
```

---

## Known wrinkles — read before implementing

### W1 — `evalMap` type in `fastPhase` (must not regress)

The pattern `stateManager.advanceAnimationsOnTick() >> fullRender()` discards the
`Boolean` returned by `advanceAnimationsOnTick()`. The result of `evalMap` would be
`IO[Unit]`, making `takeWhile(identity)` a no-op (the stream would run forever). Always
use the `for` comprehension form shown in Change 4f. This is the most common mistake to
make when writing this code.

### W2 — `takeWhile` and final-frame rendering

`takeWhile(p)` in FS2 halts the stream before emitting the element that fails `p`. In
`fastPhase`, `evalMap` runs the full `advanceAnimations >> render >> yield active`
sequence before `takeWhile` sees the result. When `active` is `false` (last frame),
`render` has already been called with the final animation state. The last frame is always
painted correctly. Do not add an extra render after `takeWhile` — it is unnecessary.

### W3 — `onFinalize` race with concurrent input

`fastPhase` terminates when `takeWhile` receives `false`. The `onFinalize` runs
immediately after. There is a sub-millisecond window between `takeWhile` deciding to stop
and `onFinalize` executing. A keystroke arriving in this window calls
`fastMode.set(true)` (from `inputFunnel`). Since `fastMode` is currently `true`,
`set(true)` is idempotent and has no visible effect — but then `onFinalize` calls
`fastMode.set(false)`, which overwrites the signal. The new keystroke's animations sit in
`AppState` unrendered; the render loop enters `idlePhase` and only does cursor-only
renders.

**Fix:** The conditional check in `onFinalize` (shown in Change 4f) resolves this. Before
resetting `fastMode`, re-read `AppState`. If new animations are present (added by the
concurrent keystroke), abort the reset. Only reset if the state confirms everything is
truly idle.

### W4 — `Signal.discrete` emits current value on subscription

`idlePhase` subscribes to `fastMode.discrete` via `interruptWhen`. In FS2 3.x,
`Signal.discrete` delivers the current value immediately on each new subscription, before
emitting subsequent changes. This matters for the transition out of `fastPhase`:

- `fastPhase.onFinalize` sets `fastMode = false`
- `renderLoop` recurses and `idlePhase` subscribes to `fastMode.discrete`
- If input arrived between `onFinalize` and this subscription, `fastMode` is already
  `true` when the subscription is made
- `idlePhase` receives `true` immediately on subscription → `interruptWhen` fires
  immediately → `idlePhase` terminates → `fastPhase` starts

No explicit coordination is needed for this gap. The signal's subscription semantics
handle it.

### W5 — `Stream.defer` is required in two places

Both `fastPhase` and `renderLoop` must be wrapped in `Stream.defer { ... }`. Without it:
- `renderLoop` is an infinite recursive call that evaluates eagerly at definition time and
  crashes the JVM before `run` executes.
- `fastPhase` without `defer` is safe today but fragile — wrap it anyway as a guard
  against future refactoring.

### W6 — `ScreenInputHandler.eventStream` still compiles after removing resize stream

After removing `resizeStreamEvents`, the `InputHandler` trait's `eventStream` method
signature is unchanged (`Stream[F, Event]`). Only the implementation changes. All existing
callers and tests that use `eventStream` continue to compile without modification.

---

## Testing requirements

Follow TDD. Write each test before the implementation code it covers.

### Tests to write before Change 1

New tests in an appropriate spec (new `StateManagerAnimationSpec` is recommended):

1. `advanceAnimationsOnTick` returns `false` when `screenAnimations` is empty.
2. `advanceAnimationsOnTick` returns `true` when animations are in flight.
3. `advanceAnimationsOnTick` returns `false` on the tick that drains the final animation.
4. `advanceAnimationsOnTick` does not modify state when called with no active animations
   (confirm with a version check or state equality check).

### Tests to write before Change 3c

New tests covering `renderCursorOnly` contract (without asserting terminal pixel output):

1. `renderCursorOnly` completes without error given an `AppState` with no active pane.
2. `renderCursorOnly` completes without error given a pane with an empty buffer.
3. `renderCursorOnly` completes without error given a pane with text and a cursor in the
   middle of the content.

These tests will require a mock or in-memory `Screen`. Lanterna provides
`VirtualScreen` wrapping a `DefaultTerminalFactory`-created terminal; use the same pattern
you find in existing rendering specs (`RendererBoundarySpec`, `StartupRenderingSpec`).

### Existing tests that must continue to pass

- `ResizeHandlingSpec` — all tests. Resize event handling is through `StateManager` and is
  unaffected by removing the polling stream from `ScreenInputHandler`.
- `CursorBlinkingSpec` — all tests. These test the blink model in isolation; they do not
  depend on `Renderer` internals.
- `AnimationIntegrationSpec` — all tests. The animation data model is unchanged.
- All other specs in `src/test/scala/com/serenity/` — they must compile and pass.

### Tests that need updating

- Any test that calls `stateManager.advanceAnimationsOnTick()` and ignores the return
  value with `.unsafeRunSync()` (currently returns `Unit`; after Change 1 it returns
  `Boolean`). Search for `advanceAnimationsOnTick` across the test tree and update call
  sites. At the time of writing, no test file directly references this method, but verify
  with a grep before proceeding.

---

## What NOT to change

- `AppState` — do not add `fastMode`, `cursorVisible`, or any render-loop field to this
  model. These are operational coordination primitives, not application state.
- `AnimationState`, `AnimatedCharacter`, `RgbInterpolator` — the animation data model is
  correct and is not part of this change.
- `ThemeManager`, `ThemeRenderer`, `CharacterRenderer` — no changes to the rendering
  primitives.
- `ResizeComponent` — correctly handles `ResizeEvent` and persists viewport dimensions.
  No changes needed.
- `StateManager` interface — the only method signature changing is
  `advanceAnimationsOnTick`. All other methods stay identical.
- `InputHandler` trait — the `eventStream` signature is unchanged. Only
  `ScreenInputHandler`'s implementation changes.

---

## Acceptance criteria

The implementation is complete when:

1. All existing tests pass.
2. New tests for `advanceAnimationsOnTick` and `renderCursorOnly` pass.
3. The application starts, renders the welcome screen, and accepts keystrokes.
4. Typing characters triggers animations (fast phase) and the editor renders at ~60 FPS
   during and immediately after typing.
5. When idle (no typing, no animations in flight), the render loop is dormant except for
   the 500 ms cursor blink tick.
6. Terminal resize is reflected on screen within one render tick of the loop's current
   rate (≤ 500 ms at idle, ≤ 16 ms during fast phase).
7. No `advanceAnimationsOnTick`, `animationTickStream`, `renderingStream`, or
   `resizeStreamEvents` symbols remain in `Main.scala` or `TerminalInputHandler.scala`.
