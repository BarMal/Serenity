package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{PageDown, ScrollDown}
import com.serenity.state.models.{AppState, PaneId}
import com.serenity.ui.layout.LayoutEngine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MultiPaneSessionSpec extends AnyFlatSpec with Matchers:

  import MultiBufferFixture.Size

  private val Small = Size(400)

  private def state(session: MultiPaneSession): AppState = session.manager.getCurrentState.unsafeRunSync()

  "MultiPaneSession.open" should "show a different buffer in each of the 1, 2 or 4 panes" in
    Seq(1, 2, 4).foreach { panes =>
      withClue(s"$panes panes: ") {
        val session = MultiPaneSession.open(panes, Small)
        val shown   = state(session).persisted.layout.editorPanes.values.flatMap(_.bufferId).toSet

        session.panes should have size panes
        shown should have size panes
      }
    }

  it should "lay four panes out as two columns of two, and two panes side by side" in {
    def rects(session: MultiPaneSession) =
      val current = state(session)
      val layout  = LayoutEngine.calculateLayout(current, LaptopFrameBenchmarks.viewport)
      LayoutEngine.calculatePaneLayouts(current, layout).values.toList

    val four = rects(MultiPaneSession.open(4, Small))
    four.map(_.x).distinct should have size 2
    four.map(_.y).distinct should have size 2
    four.foreach(rect => (rect.width, rect.height) should not be (0, 0))

    val two = rects(MultiPaneSession.open(2, Small))
    two.map(_.x).distinct should have size 2
    two.map(_.y).distinct should have size 1
  }

  it should "put the cursor of every pane on screen, so editing and scrolling skip no work" in {
    val current = state(MultiPaneSession.open(4, Small))

    current.persisted.layout.editorPanes.values.flatMap(_.bufferId).foreach { id =>
      val buffer = current.persisted.buffers(id)
      val cursor = buffer.editing.cursorPositions.head
      cursor.line should (be >= buffer.viewport.topLine and be < buffer.viewport.topLine + buffer.viewport.visibleLines)
    }
  }

  it should "make the first pane the one that takes the keys" in {
    val session = MultiPaneSession.open(4, Small)

    state(session).persisted.layout.activeEditorPaneId shouldBe Some(session.panes.head)
  }

  "MultiPaneSession.typing" should "change only the active pane's buffer" in {
    val session         = MultiPaneSession.open(4, Small)
    val (before, after) = session.typing()
    val active          = before.persisted.layout.editorPanes(session.panes.head).bufferId.get

    after.persisted
      .buffers(active)
      .document
      .content
      .weight should be > before.persisted.buffers(active).document.content.weight
    (before.persisted.buffers - active).foreach((id, buffer) =>
      after.persisted.buffers(id).document.content shouldBe buffer.document.content
    )
  }

  "MultiPaneSession.transition" should "scroll the active pane's viewport by a line" in {
    val session         = MultiPaneSession.open(2, Small)
    val (before, after) = session.transition(ScrollDown(1))
    val active          = before.persisted.layout.editorPanes(session.panes.head).bufferId.get

    after.persisted.buffers(active).viewport.topLine should be > before.persisted.buffers(active).viewport.topLine
  }

  it should "move the cursor of the active pane down a page" in {
    val session         = MultiPaneSession.open(2, Small)
    val (before, after) = session.transition(PageDown)
    val active          = before.persisted.layout.editorPanes(session.panes.head).bufferId.get

    after.persisted.buffers(active).editing.cursorPositions.head.line should be >
      before.persisted.buffers(active).editing.cursorPositions.head.line
  }
