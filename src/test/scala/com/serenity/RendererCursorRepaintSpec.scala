package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.{PixelRect, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the pixel geometry a frame's cursor overlay actually paints, which #963's bounded-repaint fix unions with the
  * base frame's dirty region before it hands SwingWindow a `canvas.repaint(...)` bound. If this geometry were wrong or
  * incomplete, a bounded repaint could leave a stale or missing caret on screen.
  */
class RendererCursorRepaintSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(80, 24)
  private val font     = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val metrics  = com.serenity.ui.layout.CellMetrics.fromFont(font)

  private val lines = Vector("alpha", "beta", "gamma", "delta", "epsilon")

  private def stateWith(cursors: List[CursorPosition]): AppState =
    val buffer            = Buffer.fromString(bufferId, lines.mkString("\n"))
    val bufferWithCursors = buffer.copy(editing = buffer.editing.copy(cursors = cursors))
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferWithCursors.id -> bufferWithCursors),
        bufferOrder = List(bufferWithCursors.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferWithCursors.id)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light
      )
    )

  private def cursorRects(cursors: List[CursorPosition]): List[PixelRect] =
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    Renderer.cursorRepaintRects(stateWith(cursors), surface, viewport, font, font, font, metrics, metrics, None)

  "cursorRepaintRects" should "report one rect for a single visible cursor" in {
    val rects = cursorRects(List(CursorPosition(0, 0)))

    rects should have length 1
    rects.head.widthPx should be > 0
    rects.head.heightPx should be > 0
  }

  it should "report a rect at the caret's new row when the cursor moves" in {
    val before = cursorRects(List(CursorPosition(0, 0))).head
    val after  = cursorRects(List(CursorPosition(3, 0))).head

    after.yPx should not equal before.yPx
  }

  it should "report one rect per cursor in a multi-cursor buffer" in {
    val rects = cursorRects(List(CursorPosition(0, 0), CursorPosition(2, 1)))

    rects should have length 2
  }

  it should "report no rects when the buffer has no cursors" in {
    cursorRects(Nil) shouldBe empty
  }
