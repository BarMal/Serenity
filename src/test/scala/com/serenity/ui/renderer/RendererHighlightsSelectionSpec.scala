package com.serenity.ui.renderer

import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[RendererHighlights.selectionColumnsForVisualLine]] is the one integration point shared by both the GUI (measured)
  * and TUI (cell) painting branches of `renderTextRangeBackground`: it resolves the `(rangeStart, rangeEnd)` column
  * pair a visual line's selection highlight covers.
  */
class RendererHighlightsSelectionSpec extends AnyFlatSpec with Matchers:

  private def visualLine(bufferLine: Int, startColumn: Int, endColumn: Int): TextVisualLine =
    TextVisualLine(
      bufferLine = bufferLine,
      startColumn = startColumn,
      endColumn = endColumn,
      text = "x" * (endColumn - startColumn),
      widthPx = 0.0f,
      caretStops = Vector.empty
    )

  "selectionColumnsForVisualLine" should "use the live selection for a selection on the visual line" in {
    val cursor = Cursor(CursorPosition(0, 5), selectionAnchor = Some(CursorPosition(0, 2)))

    RendererHighlights.selectionColumnsForVisualLine(cursor, visualLine(0, 0, 10)) shouldBe Some(2 -> 5)
  }

  it should "return None when there is no selection" in {
    val cursor = Cursor(CursorPosition(0, 5))

    RendererHighlights.selectionColumnsForVisualLine(cursor, visualLine(0, 0, 10)) shouldBe None
  }
