package com.serenity

import com.serenity.config.{LineNumberLayout, LineNumberSide}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Rendering for configurable line-number placement: the gutter/body divider must sit on the content-facing edge of
  * each counter (the last column of a left counter, the first column of a right counter).
  */
class LineNumberPlacementRenderSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def stateWith(side: LineNumberSide): AppState =
    val lines = (1 to 20).map(i => s"line $i").mkString("\n")
    val buffer = Buffer
      .fromString(BufferId(1), lines)
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 10, visibleColumns = 20))
    val base = AppState.initial
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
        ),
        theme = Theme.light,
        config = base.persisted.config.withLineNumberLayout(LineNumberLayout(side = side))
      )
    )

  private val viewport = ViewportSize(80, 24)

  "A left counter" should "paint its divider on its last column" in {
    val state   = stateWith(LineNumberSide.Left)
    val surface = new MockRenderSurface(80, 24)
    val layout  = LayoutEngine.calculateLayout(state, viewport)
    val rect    = layout.lineNumberRect.getOrElse(fail("expected a left counter"))

    RendererEntryPoints.render(state, cursorVisible = false, surface, viewport)

    val dividerColumn = rect.x + rect.width - 1
    (rect.y until rect.bottom).foreach(row =>
      surface.getBg(dividerColumn, row) shouldBe state.persisted.theme.panelBorder
    )
    surface.getRow(rect.y).slice(rect.x, rect.right).trim shouldBe "1"
  }

  "A right counter" should "paint its divider on its first (content-facing) column and right-align its digits" in {
    val state   = stateWith(LineNumberSide.Right)
    val surface = new MockRenderSurface(80, 24)
    val layout  = LayoutEngine.calculateLayout(state, viewport)
    val rect    = layout.rightLineNumberRect.getOrElse(fail("expected a right counter"))

    RendererEntryPoints.render(state, cursorVisible = false, surface, viewport)

    val dividerColumn = rect.x
    (rect.y until rect.bottom).foreach(row =>
      surface.getBg(dividerColumn, row) shouldBe state.persisted.theme.panelBorder
    )
    surface.getRow(rect.y).slice(rect.x, rect.right).trim shouldBe "1"
  }

  "Both placement" should "paint a counter on each side with mirrored dividers" in {
    val state   = stateWith(LineNumberSide.Both)
    val surface = new MockRenderSurface(80, 24)
    val layout  = LayoutEngine.calculateLayout(state, viewport)
    val left    = layout.lineNumberRect.getOrElse(fail("expected a left counter"))
    val right   = layout.rightLineNumberRect.getOrElse(fail("expected a right counter"))

    RendererEntryPoints.render(state, cursorVisible = false, surface, viewport)

    surface.getBg(left.x + left.width - 1, left.y) shouldBe state.persisted.theme.panelBorder
    surface.getBg(right.x, right.y) shouldBe state.persisted.theme.panelBorder
    surface.getRow(left.y).slice(left.x, left.right).trim shouldBe "1"
    surface.getRow(right.y).slice(right.x, right.right).trim shouldBe "1"
  }
