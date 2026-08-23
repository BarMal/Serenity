package com.serenity

import com.serenity.state.models.{CursorPosition, NavigationGeometry, TextCaretStop, TextVisualLine}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NavigationGeometrySpec extends AnyFlatSpec with Matchers:

  private def stops(count: Int): Vector[TextCaretStop] =
    (0 to count).map(column => TextCaretStop(column, column.toFloat * 10.0f)).toVector

  private def line(bufferLine: Int, length: Int): TextVisualLine =
    TextVisualLine(
      bufferLine = bufferLine,
      startColumn = 0,
      endColumn = length,
      text = "x" * length,
      widthPx = length.toFloat * 10.0f,
      caretStops = stops(length)
    )

  private val geometry = NavigationGeometry(Vector(line(0, 3), line(1, 3), line(2, 3)))

  "NavigationGeometry" should "return the caret x for a cursor on a visual line" in {
    geometry.xPxForCursor(CursorPosition(0, 2)) shouldBe Some(20.0f)
  }

  it should "return None for a cursor outside every visual line" in {
    geometry.xPxForCursor(CursorPosition(9, 0)) shouldBe None
  }

  it should "move the cursor down one visual line keeping the nearest column for the preferred x" in {
    geometry.moveVertical(CursorPosition(0, 2), direction = 1, preferredXPx = 20.0f) shouldBe
      Some(CursorPosition(1, 2))
  }

  it should "move the cursor up one visual line" in {
    geometry.moveVertical(CursorPosition(2, 1), direction = -1, preferredXPx = 10.0f) shouldBe
      Some(CursorPosition(1, 1))
  }

  it should "not move above the first or below the last visual line" in {
    geometry.moveVertical(CursorPosition(0, 1), direction = -1, preferredXPx = 10.0f) shouldBe None
    geometry.moveVertical(CursorPosition(2, 1), direction = 1, preferredXPx = 10.0f) shouldBe None
  }

  it should "resolve a cursor for a visual row and x position" in {
    geometry.cursorForVisualRowAndXPx(1, 20.0f) shouldBe Some(CursorPosition(1, 2))
  }
