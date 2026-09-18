package com.serenity

import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Column-based document layout (issue #1338, Phase 1 animation): `TextVisualLine.visibleSlice` is the clip
  * `RendererColumnTransition`'s sweep uses to confine a receding column's text to the sliver of the pane it hasn't
  * swept out of yet -- snapping to the nearest grapheme boundary (never a fractional pixel cut, so no glyph is ever
  * drawn half-formed) whether the line's "pixels" are real measured pixels or plain grid cells.
  */
class TextVisualLineVisibleSliceSpec extends AnyFlatSpec with Matchers:

  // Ten one-character graphemes, each 10 px wide: caret stops at 0, 10, 20, ..., 100 for columns 0..10.
  private def line: TextVisualLine =
    TextVisualLine(
      bufferLine = 0,
      startColumn = 0,
      endColumn = 10,
      text = "0123456789",
      widthPx = 100.0f,
      caretStops = (0 to 10).map(column => TextCaretStop(column, column.toFloat * 10.0f)).toVector
    )

  "TextVisualLine.visibleSlice" should "return the whole line when the window covers it entirely" in {
    line.visibleSlice(0.0f, 100.0f) shouldBe Some(("0123456789", 0.0f, 100.0f))
  }

  it should "clip characters past the window's right edge" in {
    line.visibleSlice(0.0f, 45.0f) shouldBe Some(("01234", 0.0f, 50.0f))
  }

  it should "clip characters before the window's left edge, shifting the returned start position" in {
    line.visibleSlice(55.0f, 100.0f) shouldBe Some(("56789", 50.0f, 50.0f))
  }

  it should "clip both edges at once" in {
    line.visibleSlice(25.0f, 75.0f) shouldBe Some(("234567", 20.0f, 60.0f))
  }

  it should "return None when the window falls entirely outside the line" in {
    line.visibleSlice(200.0f, 300.0f) shouldBe None
    line.visibleSlice(-100.0f, -10.0f) shouldBe None
  }

  it should "return None for an empty or inverted window" in {
    line.visibleSlice(50.0f, 50.0f) shouldBe None
    line.visibleSlice(60.0f, 50.0f) shouldBe None
  }

  it should "return None for an empty line (fewer than two caret stops)" in {
    val empty = line.copy(text = "", caretStops = Vector(TextCaretStop(0, 0.0f)))
    empty.visibleSlice(0.0f, 100.0f) shouldBe None
  }
