package com.serenity

import com.serenity.config.{LineNumberLayout, LineNumberSide}
import com.serenity.testkit.ActivePane
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

  // An unset margin/padding now resolves to a cell of breathing room on the GUI surface this spec runs under
  // (`AppState.effectiveLineNumberMarginLeft`/`effectiveLineNumberPadding`) -- not the flush-to-edge zero the TUI
  // keeps -- so a bare `LineNumberLayout()` no longer sits the counter flush against the panel edge.
  private val defaultGuiSpacing = 1

  "Default left placement" should "offset the counter and the content by the GUI's default cell of breathing room" in {
    val state  = stateWith(LineNumberLayout())
    val layout = layoutFor(state)
    val left   = ActivePane.of(state, layout).lineNumberRect.getOrElse(fail("expected a left counter"))
    ActivePane.of(state, layout).rightLineNumberRect shouldBe None
    left.x shouldBe defaultGuiSpacing
    left.width shouldBe counterWidth
    ActivePane.of(state, layout).contentRect.x shouldBe (left.right + defaultGuiSpacing)
  }

  "Left placement with margin and padding" should "offset the counter and the content by those cells" in {
    val state  = stateWith(LineNumberLayout(side = LineNumberSide.Left, marginLeft = Some(2), padding = Some(3)))
    val layout = layoutFor(state)
    val left   = ActivePane.of(state, layout).lineNumberRect.getOrElse(fail("expected a left counter"))
    left.x shouldBe 2
    left.width shouldBe counterWidth
    ActivePane.of(state, layout).contentRect.x shouldBe (2 + counterWidth + 3)
  }

  "Right placement" should "put the counter on the content's right, no left counter" in {
    val state  = stateWith(LineNumberLayout(side = LineNumberSide.Right, marginRight = 1, padding = Some(2)))
    val layout = layoutFor(state)
    ActivePane.of(state, layout).lineNumberRect shouldBe None
    val right = ActivePane.of(state, layout).rightLineNumberRect.getOrElse(fail("expected a right counter"))
    right.width shouldBe counterWidth
    right.x shouldBe (ActivePane.of(state, layout).contentRect.right + 2)
    // marginRight cells sit between the counter and the workspace's right edge.
    (viewport.width - right.right) shouldBe 1
  }

  "Both placement" should "put a counter on each side with content between them" in {
    val state  = stateWith(LineNumberLayout(side = LineNumberSide.Both))
    val layout = layoutFor(state)
    val left   = ActivePane.of(state, layout).lineNumberRect.getOrElse(fail("expected a left counter"))
    val right  = ActivePane.of(state, layout).rightLineNumberRect.getOrElse(fail("expected a right counter"))
    left.x shouldBe defaultGuiSpacing
    left.width shouldBe counterWidth
    right.width shouldBe counterWidth
    ActivePane.of(state, layout).contentRect.x shouldBe (left.right + defaultGuiSpacing)
    right.x shouldBe (ActivePane.of(state, layout).contentRect.right + defaultGuiSpacing)
    right.right shouldBe viewport.width
  }

  "Both placement" should "leave the content narrower by both counters, plus the GUI default's own padding" in {
    val plainState = stateWith(LineNumberLayout(side = LineNumberSide.Left))
    val bothState  = stateWith(LineNumberLayout(side = LineNumberSide.Both))
    val plain      = ActivePane.of(plainState, layoutFor(plainState))
    val both       = ActivePane.of(bothState, layoutFor(bothState))
    both.contentRect.width shouldBe (plain.contentRect.width - counterWidth - defaultGuiSpacing)
  }

  "The right counter" should "share the left counter's vertical extent" in {
    val state  = stateWith(LineNumberLayout(side = LineNumberSide.Both))
    val layout = layoutFor(state)
    val left   = ActivePane.of(state, layout).lineNumberRect.getOrElse(fail("expected a left counter"))
    val right  = ActivePane.of(state, layout).rightLineNumberRect.getOrElse(fail("expected a right counter"))
    right.y shouldBe left.y
    right.height shouldBe left.height
  }
