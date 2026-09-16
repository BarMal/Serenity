package com.serenity

import com.serenity.config.{LineNumberLayout, LineNumberSide}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Geometry for configurable line-number placement, margin, and padding (all in cells). */
class LineNumberPlacementLayoutSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val viewport = ViewportSize(80, 24)

  private def stateWith(layout: LineNumberLayout, lineCount: Int = 3): AppState =
    val text   = (1 to lineCount).map(i => s"Line $i").mkString("\n")
    val buffer = Buffer.fromString(BufferId(1), text)
    val base   = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = com.serenity.ui.layout.Layout(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0))))
        ),
        focus = Focus.EditorPane(PaneId(0)),
        theme = Theme.light,
        config = base.persisted.config
          .withLineNumbers(true)
          .withoutStatusLine
          .withLineNumberLayout(layout)
      )
    )

  private def layoutFor(state: AppState): CalculatedLayout =
    LayoutEngine.calculateLayoutWithUI(state, viewport)

  // 3-line buffer -> max(3, digits+1) = 3 cells.
  private val counterWidth = 3

  "Default left placement" should "put the counter at the left edge with content immediately after it" in {
    val layout = layoutFor(stateWith(LineNumberLayout()))
    val left   = layout.lineNumberRect.getOrElse(fail("expected a left counter"))
    layout.rightLineNumberRect shouldBe None
    left.x shouldBe 0
    left.width shouldBe counterWidth
    layout.editorPanelRect.x shouldBe left.right
  }

  "Left placement with margin and padding" should "offset the counter and the content by those cells" in {
    val layout = layoutFor(stateWith(LineNumberLayout(side = LineNumberSide.Left, marginLeft = 2, padding = 3)))
    val left   = layout.lineNumberRect.getOrElse(fail("expected a left counter"))
    left.x shouldBe 2
    left.width shouldBe counterWidth
    layout.editorPanelRect.x shouldBe (2 + counterWidth + 3)
  }

  "Right placement" should "put the counter on the content's right, no left counter" in {
    val layout = layoutFor(stateWith(LineNumberLayout(side = LineNumberSide.Right, marginRight = 1, padding = 2)))
    layout.lineNumberRect shouldBe None
    val right = layout.rightLineNumberRect.getOrElse(fail("expected a right counter"))
    right.width shouldBe counterWidth
    right.x shouldBe (layout.editorPanelRect.right + 2)
    // marginRight cells sit between the counter and the workspace's right edge.
    (viewport.width - right.right) shouldBe 1
  }

  "Both placement" should "put a counter on each side with content between them" in {
    val layout = layoutFor(stateWith(LineNumberLayout(side = LineNumberSide.Both)))
    val left   = layout.lineNumberRect.getOrElse(fail("expected a left counter"))
    val right  = layout.rightLineNumberRect.getOrElse(fail("expected a right counter"))
    left.x shouldBe 0
    left.width shouldBe counterWidth
    right.width shouldBe counterWidth
    layout.editorPanelRect.x shouldBe left.right
    right.x shouldBe layout.editorPanelRect.right
    right.right shouldBe viewport.width
  }

  "Both placement" should "leave the content narrower by both counters" in {
    val plain = layoutFor(stateWith(LineNumberLayout(side = LineNumberSide.Left)))
    val both  = layoutFor(stateWith(LineNumberLayout(side = LineNumberSide.Both)))
    both.editorPanelRect.width shouldBe (plain.editorPanelRect.width - counterWidth)
  }

  "The right counter" should "share the left counter's vertical extent" in {
    val layout = layoutFor(stateWith(LineNumberLayout(side = LineNumberSide.Both)))
    val left   = layout.lineNumberRect.getOrElse(fail("expected a left counter"))
    val right  = layout.rightLineNumberRect.getOrElse(fail("expected a right counter"))
    right.y shouldBe left.y
    right.height shouldBe left.height
  }
