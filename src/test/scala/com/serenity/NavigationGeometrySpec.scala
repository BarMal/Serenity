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

  // A wrapped buffer line produces two adjacent visual lines that share a boundary column
  // (`lineA.endColumn == lineB.startColumn == 5`). A cursor sitting exactly at that column is, by convention, the
  // start of the *next* visual line (`lineB`) -- the wrap point resets x to 0, so `lineB`'s own caret stop for
  // column 5 is 0.0f, distinct from `lineA`'s trailing caret stop for the same column (50.0f).
  private val wrapLineA = TextVisualLine(
    bufferLine = 0,
    startColumn = 0,
    endColumn = 5,
    text = "aaaaa",
    widthPx = 50.0f,
    caretStops = (0 to 5).map(column => TextCaretStop(column, column.toFloat * 10.0f)).toVector
  )

  private val wrapLineB = TextVisualLine(
    bufferLine = 0,
    startColumn = 5,
    endColumn = 10,
    text = "bbbbb",
    widthPx = 50.0f,
    caretStops = (5 to 10).map(column => TextCaretStop(column, (column - 5).toFloat * 10.0f)).toVector
  )

  private val nextBufferLine = TextVisualLine(
    bufferLine = 1,
    startColumn = 0,
    endColumn = 3,
    text = "ccc",
    widthPx = 30.0f,
    caretStops = (0 to 3).map(column => TextCaretStop(column, column.toFloat * 10.0f)).toVector
  )

  private val wrapGeometry = NavigationGeometry(Vector(wrapLineA, wrapLineB, nextBufferLine))

  it should "resolve a cursor at a wrap boundary column to the later visual line's caret x" in {
    wrapGeometry.xPxForCursor(CursorPosition(0, 5)) shouldBe Some(0.0f)
  }

  it should "move down from a wrap boundary column using the later visual line's row" in {
    // If the boundary column were matched against `wrapLineA` (row 0) instead of `wrapLineB` (row 1), moving down
    // would land back on `wrapLineB` (row 1) rather than advancing to `nextBufferLine` (row 2).
    wrapGeometry.moveVertical(CursorPosition(0, 5), direction = 1, preferredXPx = 0.0f) shouldBe
      Some(CursorPosition(1, 0))
  }
