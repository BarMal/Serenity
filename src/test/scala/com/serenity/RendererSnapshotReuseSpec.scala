package com.serenity

import java.awt.Font

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Verifies that rendering a buffer with a cursor produces consistent pixel coordinates — text placement and cursor
  * placement derive from the same snapshot, so the cursor cannot drift.
  */
class RendererSnapshotReuseSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val cellMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(80, 24)

  private def buildState(content: String, cursorCol: Int): AppState =
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, content).copy(cursors = List(CursorPosition(0, cursorCol)))
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false)
    )

  "Renderer" should "use consistent pixel coordinates for text and cursor on a monospaced buffer" in {
    val state   = buildState("hello", cursorCol = 2)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    Renderer.render(state, cursorVisible = true, surface, viewportSize, monoFont, monoFont, cellMetrics, None)

    val cursorRects = surface.fillPixelRectCalls
    cursorRects should not be empty

    val layout       = LayoutEngine.calculateLayout(state, viewportSize)
    val paneLayouts  = LayoutEngine.calculatePaneLayouts(state, layout)
    val paneRect     = paneLayouts(PaneId(0))
    val panelWidthPx = paneRect.width * cellMetrics.charWidth
    val snapshot = TextLayoutSnapshot.fromBuffer(
      state.buffers(BufferId(1)),
      panelWidthPx,
      monoFont,
      surface.fontRenderContext.getOrElse(fail("missing frc"))
    )
    val expectedXPx = cellMetrics.toPixelX(paneRect.x) + math.round(
      snapshot.xPxForCursor(CursorPosition(0, 2)).getOrElse(fail("missing cursor x"))
    )
    cursorRects.last.xPx shouldBe expectedXPx
  }

  it should "draw the content text via drawRunPx when measured code-font layout is required" in {
    val state   = buildState("hello", 0)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    Renderer.render(state, cursorVisible = true, surface, viewportSize, monoFont, monoFont, cellMetrics, None)
    surface.drawRunPxCalls.exists(_.s.contains("hello")) shouldBe true
  }
