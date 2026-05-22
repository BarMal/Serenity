# animation-overhaul — what's done and what's next

## What this branch fixes

Two root causes in the render loop:

**Animation drift (bug):** Character animations were keyed by screen position computed
at insert time. After any scroll or resize the stored coordinates no longer matched the
renderer's lookup coordinates, so animations appeared on the wrong character or not at
all. Fix: key animations by buffer position `CharacterKey(column, line)` throughout —
stored in `EditorPaneComponent`, looked up in `CharacterRenderer` using the
`bufferLine`/`bufferStartColumn` the renderer already knows.

**Resize stays idle (bug):** `checkResize` in `Main.scala` applied the `ResizeEvent`
to state but never set `fastMode = true`, so the loop stayed in the 500ms cursor-only
idle phase until the next keypress. Fix: extracted `RenderController.handleResize`
(testable, takes an `onResized: IO[Unit]` callback), wired `fastMode.set(true)` as
that callback in `Main`.

## Files changed

- `animation/AnimationState.scala` — `ScreenPosition` (non-existent) → `CharacterKey`
- `state/components/EditorPaneComponent.scala` — store at buffer coords; drop `calculateAnimationScreenPosition`
- `ui/renderer/CharacterRenderer.scala` — add `bufferLine`/`bufferStartColumn` params to animation render path
- `ui/renderer/Renderer.scala` — pass `visualLine.bufferLine` and `visualLine.startColumn` through
- `ui/renderer/RenderController.scala` — new; extracted `handleResize`
- `Main.scala` — use `RenderController.handleResize` with `fastMode.set(true)` callback
- `ResizeRenderTriggerSpec.scala` — new TDD tests for resize → fast-mode trigger
- `BufferCoordinateAnimationSpec.scala` — new TDD tests for buffer-coord animation keying

## Next: test infrastructure improvements

Three additions discussed, in priority order:

### 1. Fill pending tests in `EditorEndToEndSpec`

~14 tests are `pending` stubs. Most of the underlying operations (cursor movement,
backspace, delete-forward, line joining) already work. This is pure TDD enablement with
no new infrastructure — just writing the assertions.

### 2. Extract `RenderLoop` from `Main.scala`

The `idlePhase`/`fastPhase` stream logic is currently untestable (glued to a real
`Screen`). Extract to a `RenderLoop` class parameterised on `IO[Unit]` render effects:

```scala
class RenderLoop(
  renderFull:       IO[Unit],
  renderCursorOnly: IO[Unit],
  advanceTick:      IO[Boolean],
  fastMode:         SignallingRef[IO, Boolean]
):
  def idlePhase: Stream[IO, Unit] = ...
  def fastPhase:  Stream[IO, Unit] = ...
```

`Main` composes the real effects; tests inject fakes and control time with
`cats-effect-testing`'s `TestControl`.

### 3. `TestControl`-based animation timing tests

Add `cats-effect-testing` to `build.sbt` and write deterministic time tests:

```scala
"animation loop" should "exit fast phase when all frames complete" in {
  TestControl.execute {
    for
      sm       <- makeStateManager()
      fastMode <- SignallingRef.of[IO, Boolean](false)
      loop      = RenderLoop(IO.unit, IO.unit, sm.advanceAnimationsOnTick(), fastMode)
      _        <- sm.applyEvent(InsertChar('a')) >> fastMode.set(true)
      _        <- loop.fastPhase.compile.drain
      flag     <- fastMode.get
    yield flag shouldBe false
  }
}
```

This enables TDD for anything timing-based: animation duration, render cadence,
resize-to-render latency.

## Next: finish and fix animations

### Bugs to fix first

**`AnimatedCharacter.apply` ignores its `color` parameter** (`AnimatedCharacter.scala:65`).
It unconditionally creates `AnimatedCharacter(char, List.empty)`, silently discarding
`color`. Any caller relying on the single-color overload gets a transparent character.
Fix: `AnimatedCharacter(char, List(color))`.

**`AppConfig.default` has a stale test comment** (`AppConfig.scala:26`). The default is
`AnimationConfig.quick` with `// TEMPORARILY ENABLED FOR TESTING`. Decide on the real
production default (probably `smooth`) and remove the comment.

**`RgbInterpolator.interpolateAnsiColors` only handles `BLACK → WHITE`** (`RgbInterpolator.scala:87`).
Every other ANSI colour pair falls through to coarse RGB approximation (e.g. `BLUE →
RGB(0,0,128)`), producing ugly stepped transitions for themed colour pairs. Extend the
match to cover the common dark-background → foreground pairs, or switch to converting
both ends to RGB unconditionally and dropping the ANSI special-case entirely.

**`MoveUp`/`MoveDown` use a hardcoded `TerminalSize(80, 24)`** (`EditorPaneComponent.scala:184,203`).
The `TODO` comment is already there. Fix: read `state.terminalSize.getOrElse(TerminalSize(80, 24))`
so visual-line cursor movement uses the real panel width.

### Features to add

**Animate deletion.** `DeleteBackward` and `DeleteForward` remove characters from the
rope but add no animation. The deleted character could fade out (foreground → background)
on its visual position before being removed. This requires a brief "ghost" animation at
the buffer position of the deleted char, coordinated with the rope update so the ghost
isn't overwritten by the shifted content on the same frame.

**Configurable animation style.** The direction is always background → foreground (fade
in). Add support for at least: fade-out (foreground → background, useful for deletion),
and flash (foreground → bright → foreground). `AnimationConfig` would gain an
`animationType: AnimationType` field; `EditorPaneComponent` selects based on event type.

## Next: word-wrapping

The renderer currently hard-wraps at exactly `panelWidth` characters
(`Renderer.wrapLineToSegments`). Word-wrapping breaks at the last whitespace before the
limit, falling back to hard-wrap only when a single word exceeds the panel width.

This touches three separate layers that must be kept consistent:

**Renderer** (`Renderer.wrapLineToSegments`): replace the dumb `substring(0, panelWidth)`
loop with a word-aware break. The `VisualLine.startColumn` produced here feeds directly
into the animation buffer-coordinate lookup — it must remain the exact character offset
within the buffer line where the visual segment starts.

**Cursor movement** (`EditorPaneComponent.moveUpVisualLine` / `moveDownVisualLine`):
both currently compute visual-line position by integer division (`cursor.column / panelWidth`).
That only works for hard-wrap. Word-wrap requires the same segment-boundary list the
renderer computes — extract `wrapLineToSegments` (or an equivalent pure function) to a
shared location (e.g. `ui.layout.LineWrapper`) so cursor movement and rendering always
agree on where visual-line breaks fall.

**Viewport adjustment** (`EditorPaneComponent.adjustViewportForCursor`): `visibleLines`
counts visual rows, not buffer lines. Word-wrap increases the visual-row count for long
lines; the viewport must account for this when scrolling to keep the cursor visible.

Suggested implementation order:
1. Extract `LineWrapper.segments(line: String, width: Int): List[(String, Int, Int)]` as
   a pure function with its own spec — this is the shared source of truth.
2. Update `Renderer` to call it (no behaviour change for short lines).
3. Update `moveUpVisualLine` / `moveDownVisualLine` to use `LineWrapper`.
4. Update `adjustViewportForCursor` to count visual rows correctly.
5. Switch `LineWrapper` from hard-wrap to word-wrap and watch the tests catch anything
   that drifts.
