package com.serenity.ui.renderer

import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.state.models.*
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Selection grow/settle (issue #1085 phase 3): [[RendererHighlights.selectionColumnsForVisualLine]] is the one
  * integration point shared by both the GUI (measured) and TUI (cell) painting branches of
  * `renderTextRangeBackground` -- both already take a generic `(rangeStart, rangeEnd)` column pair, so resolving that
  * pair from an in-flight `Cursor.selectionGeometry` when one exists, or the live selection otherwise, is the entire
  * renderer change either path needs; no TUI-specific animation code exists (or is needed) beyond this.
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

  private def rect(startColumn: Int, width: Int): LayoutRect = LayoutRect(startColumn, 0, width, 1)

  "selectionColumnsForVisualLine" should "use the live selection when there is no in-flight geometry" in {
    val cursor = Cursor(CursorPosition(0, 5), selectionAnchor = Some(CursorPosition(0, 2)))

    RendererHighlights.selectionColumnsForVisualLine(cursor, visualLine(0, 0, 10)) shouldBe Some(2 -> 5)
  }

  it should "return None when there is neither a live selection nor an in-flight geometry" in {
    val cursor = Cursor(CursorPosition(0, 5))

    RendererHighlights.selectionColumnsForVisualLine(cursor, visualLine(0, 0, 10)) shouldBe None
  }

  it should "prefer the in-flight geometry's animated extent over the live selection for a matching visual line" in {
    val tween    = Tween(start = rect(0, 2), end = rect(0, 8), curve = EasingCurve.Linear, steps = 4)
    val geometry = SelectionGeometryState(List(SelectionLineGeometry(SelectionLineKey(0, 0), tween)))
    val cursor = Cursor(
      CursorPosition(0, 8),
      selectionAnchor = Some(CursorPosition(0, 0)),
      selectionGeometry = Some(geometry)
    )

    RendererHighlights.selectionColumnsForVisualLine(cursor, visualLine(0, 0, 10)) shouldBe Some(0 -> 2)
  }

  it should "paint nothing for a visual line whose animated extent is still a zero-width sliver" in {
    val tween    = Tween(start = rect(3, 0), end = rect(3, 5), curve = EasingCurve.Linear, steps = 4, currentFrame = 0)
    val geometry = SelectionGeometryState(List(SelectionLineGeometry(SelectionLineKey(0, 0), tween)))
    val cursor   = Cursor(CursorPosition(0, 3), selectionGeometry = Some(geometry))

    RendererHighlights.selectionColumnsForVisualLine(cursor, visualLine(0, 0, 10)) shouldBe None
  }

  it should "keep painting a removing line's shrinking extent even after the live selection no longer covers it" in {
    val tween    = Tween(start = rect(0, 4), end = rect(0, 0), curve = EasingCurve.Linear, steps = 4)
    val geometry = SelectionGeometryState(List(SelectionLineGeometry(SelectionLineKey(1, 0), tween, removing = true)))
    val cursor   = Cursor(CursorPosition(0, 3), selectionAnchor = Some(CursorPosition(0, 0)), selectionGeometry = Some(geometry))

    RendererHighlights.selectionColumnsForVisualLine(cursor, visualLine(1, 0, 10)) shouldBe Some(0 -> 4)
  }

  it should "fall back to the live selection for a visual line the in-flight geometry has no entry for" in {
    val tween    = Tween(start = rect(0, 2), end = rect(0, 5), curve = EasingCurve.Linear, steps = 4)
    val geometry = SelectionGeometryState(List(SelectionLineGeometry(SelectionLineKey(0, 0), tween)))
    val cursor = Cursor(
      CursorPosition(1, 4),
      selectionAnchor = Some(CursorPosition(0, 0)),
      selectionGeometry = Some(geometry)
    )

    RendererHighlights.selectionColumnsForVisualLine(cursor, visualLine(1, 0, 10)) shouldBe Some(0 -> 4)
  }
