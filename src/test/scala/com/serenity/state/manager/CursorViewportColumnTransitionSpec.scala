package com.serenity.state.manager

import com.serenity.TestWorkspaceTrees
import com.serenity.animation.TransitionDirection
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, MotionAccessibility}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Column-based document layout (issue #1338, Phase 1 animation): `CursorViewport.ensureVisibleCursors` seeds
  * `Runtime.columnTransitions` whenever `adjustForCursorColumnMode` actually moves which column is showing -- whatever
  * moved the cursor there, not only `ColumnLeft`/`ColumnRight` -- gated by the `ColumnTransitions` motion family
  * (including accessibility).
  */
class CursorViewportColumnTransitionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(buffer: Buffer, config: AppConfig => AppConfig = _.withColumnMode(true)): AppState =
    val base = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = base.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId),
        config = config(base.persisted.config)
      ),
      runtime = base.runtime.copy(isTuiMode = true)
    )

  // 30 single-row lines, 8 rows per column -> columns are [0,8) [8,16) [16,24) [24,30). `topLine` is the column
  // already showing `cursorLine` -- these fixtures stand in for an already-settled "before" state, not a placement
  // still to be computed.
  private def bufferAt(cursorLine: Int): Buffer =
    val content = (0 until 30).map(i => s"line $i").mkString("\n")
    Buffer
      .fromString(bufferId, content)
      .copy(
        viewport = Viewport(topLine = (cursorLine / 8) * 8, leftColumn = 0, visibleColumns = 40, visibleLines = 8),
        editing = Buffer.fromString(bufferId, content).editing.copy(cursors = List(CursorPosition(cursorLine, 0)))
      )

  // Mirrors what a reducer actually does (`EditorNavigationEventReducer`'s doc comment on `pageNavigate`): it moves
  // the cursor and leaves `viewport` untouched, trusting `ensureVisibleCursors` to recompute it afterwards. So the
  // "after" buffer here starts from `before`'s own buffer -- same (stale) viewport -- with only the cursor moved.
  private def movedTo(buffer: Buffer, cursorLine: Int): Buffer =
    buffer.copy(editing = buffer.editing.copy(cursors = List(CursorPosition(cursorLine, 0))))

  "CursorViewport.ensureVisibleCursors" should
    "seed a column transition when the active column changes" in {
      val before = stateWith(bufferAt(0))
      val after = before.copy(persisted =
        before.persisted.copy(buffers = Map(bufferId -> movedTo(before.persisted.buffers(bufferId), 20)))
      )

      val result = CursorViewport.ensureVisibleCursors(before, after)

      val transition = result.runtime.columnTransitions
        .getOrElse(bufferId, fail("expected a seeded column transition"))
      transition.progress shouldBe 0.0
      transition.previousTopLine shouldBe 0
      transition.previousTopVisualLine shouldBe 0
      transition.direction shouldBe TransitionDirection.RightToLeft
      result.persisted.buffers(bufferId).viewport.topLine shouldBe 16
    }

  it should "read backward movement (a smaller column index) as the opposite sweep direction" in {
    val before = stateWith(bufferAt(20))
    val after = before.copy(persisted =
      before.persisted.copy(buffers = Map(bufferId -> movedTo(before.persisted.buffers(bufferId), 0)))
    )

    val result = CursorViewport.ensureVisibleCursors(before, after)

    val transition = result.runtime.columnTransitions
      .getOrElse(bufferId, fail("expected a seeded column transition"))
    transition.direction shouldBe TransitionDirection.LeftToRight
  }

  it should "not seed a transition when the cursor stays within the same column" in {
    val before = stateWith(bufferAt(0))
    val after = before.copy(persisted =
      before.persisted.copy(buffers = Map(bufferId -> movedTo(before.persisted.buffers(bufferId), 3)))
    )

    val result = CursorViewport.ensureVisibleCursors(before, after)

    result.runtime.columnTransitions shouldBe empty
  }

  it should "not seed a transition when the ColumnTransitions family is disabled by motion accessibility" in {
    val before =
      stateWith(bufferAt(0), config = _.withColumnMode(true).withMotionAccessibility(MotionAccessibility.Off))
    val after = before.copy(persisted =
      before.persisted.copy(buffers = Map(bufferId -> movedTo(before.persisted.buffers(bufferId), 20)))
    )

    val result = CursorViewport.ensureVisibleCursors(before, after)

    result.runtime.columnTransitions shouldBe empty
    // The viewport itself must still move to the new column even with no animation to show for it.
    result.persisted.buffers(bufferId).viewport.topLine shouldBe 16
  }

  // Issue #1083's `Tween.retarget`, exercised through `CursorViewport.seedColumnTransition`: the known gap this
  // refactor fixes as a side effect (see `ColumnTransitionState`'s doc comment) is that crossing another column
  // boundary before the current sweep finishes used to snap the sweep back to progress 0, a visible jump-cut.
  it should "retarget an in-flight transition rather than reseeding it at progress zero when the column changes again" in {
    val before = stateWith(bufferAt(0))
    val afterFirstMove = before.copy(persisted =
      before.persisted.copy(buffers = Map(bufferId -> movedTo(before.persisted.buffers(bufferId), 20)))
    )
    val firstResult = CursorViewport.ensureVisibleCursors(before, afterFirstMove)
    val firstTransition =
      firstResult.runtime.columnTransitions.getOrElse(bufferId, fail("expected a seeded column transition"))

    // Advance the sweep partway, mirroring `StateManagerEditorCapability.advanceAnimationsOnTick`.
    val midFlight = firstTransition.advance.advance
    midFlight.progress should be > 0.0
    midFlight.isComplete shouldBe false
    val midFlightState = firstResult.copy(runtime =
      firstResult.runtime.copy(columnTransitions = firstResult.runtime.columnTransitions.updated(bufferId, midFlight))
    )

    // The cursor crosses into a further column before the first sweep finishes.
    val afterSecondMove = midFlightState.copy(persisted =
      midFlightState.persisted.copy(buffers = Map(bufferId -> movedTo(midFlightState.persisted.buffers(bufferId), 28)))
    )
    val secondResult = CursorViewport.ensureVisibleCursors(midFlightState, afterSecondMove)

    val retargeted =
      secondResult.runtime.columnTransitions.getOrElse(bufferId, fail("expected a retargeted column transition"))
    // No jump-cut: progress continues from where the in-flight sweep already was, not from zero.
    retargeted.progress shouldBe midFlight.progress
    retargeted.direction shouldBe firstTransition.direction
    retargeted.previousTopLine shouldBe firstTransition.previousTopLine
    retargeted.previousTopVisualLine shouldBe firstTransition.previousTopVisualLine
    retargeted.isComplete shouldBe false
  }

  it should "not seed a transition when column mode is off" in {
    val before = stateWith(bufferAt(0), config = _.withColumnMode(false))
    val after = before.copy(persisted =
      before.persisted.copy(buffers = Map(bufferId -> movedTo(before.persisted.buffers(bufferId), 20)))
    )

    val result = CursorViewport.ensureVisibleCursors(before, after)

    result.runtime.columnTransitions shouldBe empty
  }
