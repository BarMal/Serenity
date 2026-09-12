package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1483: the vertical divider between the line-number gutter and the document body rendered as short per-line
  * dashes instead of one smooth continuous line. Fixed by painting the gutter's last column as a single full-height
  * background fill ([[com.serenity.ui.renderer.RendererGutter.renderLineNumbers]]) instead of leaving its colour to
  * fall out of each row's own, independent fill.
  */
class GutterDividerRenderSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  "RendererGutter.renderLineNumbers" should "paint the gutter/body divider as a single full-height fill, not one call per row" in {
    val lines  = (1 to 20).map(i => s"line $i").mkString("\n")
    val buffer = Buffer
      .fromString(BufferId(1), lines)
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 10, visibleColumns = 20))
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
        ),
        theme = Theme.light
      )
    )
    val surface  = new MockRenderSurface(80, 24)
    val viewport = ViewportSize(80, 24)
    val layout   = LayoutEngine.calculateLayout(state, viewport)
    val gutter   = layout.lineNumberRect.getOrElse(fail("Expected line number rect"))

    RendererEntryPoints.render(state, cursorVisible = false, surface, viewport)

    val dividerColumn = gutter.x + gutter.width - 1
    val dividerCalls = surface.fillRectCalls.filter(call =>
      call.x == dividerColumn && call.w == 1 && call.background == state.persisted.theme.panelBorder
    )

    dividerCalls should have size 1
    dividerCalls.head.h shouldBe gutter.height
    dividerCalls.head.y shouldBe gutter.y

    // Every visible body row shows that one fill's colour at the divider column -- not a per-row patchwork.
    val bodyRows = gutter.y until (gutter.y + gutter.height)
    bodyRows.foreach(row => surface.getBg(dividerColumn, row) shouldBe state.persisted.theme.panelBorder)
  }
