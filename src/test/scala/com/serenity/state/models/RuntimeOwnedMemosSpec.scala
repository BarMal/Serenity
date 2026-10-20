package com.serenity.state.models

import com.serenity.rope.Balance
import com.serenity.testkit.EditingStateFixtures
import com.serenity.ui.layout.{LayoutEngine, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The memos that used to live in top-level objects belong to the state they remember answers for: two states that do
  * not descend from one another share none of them, and copies of one state share its.
  */
class RuntimeOwnedMemosSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(100, 30)

  private def withSelection(text: String): AppState =
    val buffer = Buffer
      .fromString(BufferId(1), text)
      .copy(editing =
        EditingStateFixtures(selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, text.length))))
      )
    val base = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = base.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

  "The chrome layout memo" should "belong to a state's runtime, not to the JVM" in {
    val (a, b) = (AppState.initial, AppState.initial)

    LayoutEngine.calculateLayoutWithUI(a, viewport)
    LayoutEngine.calculateLayoutWithUI(a, viewport)

    a.runtime.chromeLayouts.computationCount shouldBe 1
    b.runtime.chromeLayouts.computationCount shouldBe 0
  }

  it should "be shared by the copies of one state" in {
    val a      = AppState.initial
    val edited = a.copy(runtime = a.runtime.copy(safeMode = true))

    LayoutEngine.calculateLayoutWithUI(a, viewport)

    (edited.runtime.chromeLayouts eq a.runtime.chromeLayouts) shouldBe true
  }

  "The caret-anchoring wrap cache" should "belong to a state's runtime, not to the JVM" in {
    val (a, b) = (AppState.initial, AppState.initial)

    LayoutEngine
      .laidOutPanes(a, LayoutEngine.calculateLayout(a, viewport))
      .screenPosition(PaneId(0), CursorPosition(0, 0))

    a.runtime.anchorWraps.lines.wrapStats.coldWraps should be > 0L
    b.runtime.anchorWraps.lines.wrapStats.coldWraps shouldBe 0L
  }

  "The selection statistics memo" should "belong to a state's runtime, remembering its own answers" in {
    val (a, b) = (withSelection("alpha beta gamma"), withSelection("alpha beta gamma"))

    val first = a.activeSelectionTextStatistics

    (a.activeSelectionTextStatistics.get eq first.get) shouldBe true
    a.runtime.rangeStatistics.computations shouldBe 1L
    b.runtime.rangeStatistics.computations shouldBe 0L
  }

  "Two runtimes" should "compare equal whatever their memos hold" in {
    val a = Runtime()
    val b = Runtime()

    a shouldBe b
    (a.chromeLayouts ne b.chromeLayouts) shouldBe true
  }
