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
