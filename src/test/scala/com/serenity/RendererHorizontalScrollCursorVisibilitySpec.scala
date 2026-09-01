package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for `Renderer.renderedLeftColumn` -- a private duplicate of
  * `LayoutEngine.clampLeftColumnForBuffer` (see [[LayoutEngineSpec]]) used to recompute the horizontal scroll clamp
  * whenever a buffer's own font measures a different `visibleColumns` than the code-font grid the pane was sized in.
  * Both copies clamped the rightward scroll to `lineLength - visibleColumns`, one column short of what's needed to
  * keep a cursor sitting exactly at end-of-line (`cursorColumn == lineLength`) inside the visible viewport.
  *
  * `renderedLeftColumn` is private with no public entry point that isolates its formula from the rest of the
  * rendering pipeline (font measurement, overscan padding, pixel clipping), so it is invoked here via reflection --
  * the only way to pin down this specific duplicate's behaviour directly, the same way [[LayoutEngineSpec]] pins down
  * `clampLeftColumnForBuffer`'s.
  */
class RendererHorizontalScrollCursorVisibilitySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def invokeRenderedLeftColumn(buffer: Buffer, viewport: Viewport, wordWrapEnabled: Boolean): Int =
    val moduleField = Class.forName("com.serenity.ui.renderer.Renderer$").getField("MODULE$")
    val module      = moduleField.get(null)
    val method = module.getClass.getDeclaredMethod(
      "renderedLeftColumn",
      classOf[Buffer],
      classOf[Viewport],
      classOf[Boolean]
    )
    method.setAccessible(true)
    method.invoke(module, buffer, viewport, Boolean.box(wordWrapEnabled)).asInstanceOf[Int]

  "Renderer.renderedLeftColumn" should "keep an end-of-line cursor visible when scrolling horizontally with word wrap off" in {
    val bufferId       = BufferId(1)
    val lineContent    = "0123456789" * 20
    val bufferBase     = Buffer.fromString(bufferId, lineContent)
    val cursor         = CursorPosition(0, lineContent.length)
    val visibleColumns = 40
    // Simulate a prior scroll-to-cursor pass that already scrolled as far right as it can (leftColumn = cursorColumn
    // - visibleColumns + 1, the same bound `maxForCursor` computes) -- reproducing the state the viewport is in right
    // after the cursor lands at end-of-line.
    val scrolledViewport =
      Viewport.default.copy(leftColumn = cursor.column - visibleColumns + 1, visibleColumns = visibleColumns)
    val buffer =
      bufferBase.copy(editing = bufferBase.editing.copy(cursors = List(cursor)), viewport = scrolledViewport)

    val leftColumn = invokeRenderedLeftColumn(buffer, scrolledViewport, wordWrapEnabled = false)

    withClue(s"leftColumn=$leftColumn, visibleColumns=$visibleColumns, cursorColumn=${cursor.column}: ") {
      (leftColumn + visibleColumns - 1) should be >= cursor.column
    }
  }
