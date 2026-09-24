package com.serenity.state.reducers

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ViewportStateReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private def withBuffer(lines: Int, cursor: CursorPosition, viewport: Viewport): AppState =
    val buffer = AppState.initial.persisted.buffers(bufferId)
    val updated = buffer.copy(
      document = buffer.document.copy(content = Rope((1 to lines).map(i => s"Line $i").mkString("\n"))),
      editing = EditingState(List(cursor)),
      viewport = viewport
    )
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = AppState.initial.persisted.buffers.updated(bufferId, updated))
    )

  private def bufferIn(state: AppState): Buffer = state.persisted.buffers(bufferId)

  private def valid(result: ReducerResult): Boolean = AppStateValidation.validated(result.state).isRight

  "ViewportStateReducer.ensureCursorVisible" should "scroll the pane's viewport to bring its cursor into view" in {
    val state = withBuffer(200, CursorPosition(150, 0), Viewport(topLine = 0, visibleLines = 25, visibleColumns = 80))

    val result = ViewportStateReducer.ensureCursorVisible(paneId, state)

    val viewport = bufferIn(result.state).viewport
    viewport.topLine should be > 0
    viewport.topLine should be <= 150
    viewport.topLine + viewport.visibleLines should be > 150
    result.effects shouldBe Nil
    valid(result) shouldBe true
  }

  it should "leave the state unchanged for a pane that doesn't exist" in {
    val state = withBuffer(200, CursorPosition(150, 0), Viewport(topLine = 0, visibleLines = 25, visibleColumns = 80))

    ViewportStateReducer.ensureCursorVisible(PaneId(99), state) shouldBe ReducerResult.noEffects(state)
  }

  "ViewportStateReducer.clickMinimap" should "centre the viewport on the clicked line and move the cursor there" in {
    val state = withBuffer(1000, CursorPosition(0, 0), Viewport(topLine = 0, visibleLines = 25, visibleColumns = 80))

    val result = ViewportStateReducer.clickMinimap(paneId, 500, state)

    bufferIn(result.state).editing.cursorPositions shouldBe List(CursorPosition(500, 0))
    bufferIn(result.state).viewport.topLine shouldBe 488
    valid(result) shouldBe true
  }

  it should "clamp a target line past the end of a shrunken document" in {
    val state = withBuffer(10, CursorPosition(0, 0), Viewport(topLine = 0, visibleLines = 4, visibleColumns = 80))

    val result = ViewportStateReducer.clickMinimap(paneId, 500, state)

    bufferIn(result.state).editing.cursorPositions shouldBe List(CursorPosition(9, 0))
    bufferIn(result.state).viewport.topLine shouldBe 7
    valid(result) shouldBe true
  }

  it should "leave the state unchanged for a pane that doesn't exist" in {
    val state = withBuffer(10, CursorPosition(0, 0), Viewport(topLine = 0, visibleLines = 4, visibleColumns = 80))

    ViewportStateReducer.clickMinimap(PaneId(99), 5, state) shouldBe ReducerResult.noEffects(state)
  }

  "ViewportStateReducer.resize" should "record the new viewport size" in {
    val result = ViewportStateReducer.resize(ViewportSize(100, 30), AppState.initial)

    result.state.runtime.viewportSize shouldBe Some(ViewportSize(100, 30))
    result.effects shouldBe Nil
    valid(result) shouldBe true
  }
