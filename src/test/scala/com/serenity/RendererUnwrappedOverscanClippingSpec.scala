package com.serenity

import java.awt.Font

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for the unwrapped (word-wrap-disabled) cell-layout rendering path.
  *
  * With word wrap off, `AuthoritativeUiScene.forState` deliberately sizes a pane's [[TextLayoutSnapshot]] wider than
  * the pane's actual content rect -- `visibleColumns` is padded by up to `contentRect.width + 64` columns of slack so
  * scroll/wrap math stays accurate even when the buffer's real font measures narrower than the code-font grid the pane
  * is sized in. That slack is only safe when whatever paints the resulting `TextVisualLine` clips its output back down
  * to the pane's real pixel/column width. The measured (proportional-font) drawing path does this via `clipRightXPx`;
  * the cell/monospace drawing path -- the common case for a plain code font -- had no equivalent clip and painted the
  * full oversized string, so tens of characters could render past the pane's right edge whenever the cursor sat near
  * the end of a long line with word wrap disabled.
  */
class RendererUnwrappedOverscanClippingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Renderer.render" should "never paint a character past the editor pane's right edge when word wrap is disabled" in {
    // A surface reporting no FontRenderContext forces the cell/non-measured drawing path regardless of the buffer's
    // own font (see RendererCellFallbackSpec, #1105) -- the deterministic way to exercise that path in a headless
    // test environment, where font-rendering quirks can otherwise tip an ordinary monospace font onto the measured
    // path via `shouldUseMeasuredLayout`'s fractional-advance-drift probe.
    val font        = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val cellMetrics = CellMetrics.fromFont(font)

    val paneId      = PaneId(0)
    val bufferId    = BufferId(1)
    val lineContent = "0123456789" * 20
    val bufferBase  = Buffer.fromString(bufferId, lineContent)
    // Cursor at the very end of the line forces the viewport's leftColumn to scroll so the line's tail sits right at
    // the pane's right edge -- exactly the "cursor at the edge of a long line" scenario from the bug report.
    val cursor = CursorPosition(0, lineContent.length)
    val buffer = bufferBase.copy(editing = bufferBase.editing.copy(cursors = List(cursor)))
    val pane   = EditorPane.withBuffer(paneId, bufferId)

    val config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        theme = Theme.light,
        config = config
      )
    )

    val viewportSize = ViewportSize(60, 20)
    val panelRect    = LayoutEngine.calculateLayout(state, viewportSize).editorPanelRect
    val surface      = new MockRenderSurface(viewportSize.width, viewportSize.height, fontRenderContextOverride = None)

    Renderer.render(state, cursorVisible = true, surface, viewportSize, font, font, cellMetrics, None)

    // Sanity check: the cell/non-measured path is the one under test -- if this ever starts drawing via drawRunPx
    // instead, the test would vacuously pass without exercising the bug.
    surface.drawRunPxCalls shouldBe empty
    surface.putStringCalls should not be empty

    // Every character the cell/monospace path actually painted must land strictly before the pane's right edge.
    surface.putStringCalls.foreach { call =>
      val lastPaintedColumn = call.x + call.s.length - 1
      withClue(s"putString($call) overruns pane right edge (${panelRect.right}): ") {
        lastPaintedColumn should be < panelRect.right
      }
    }
  }
