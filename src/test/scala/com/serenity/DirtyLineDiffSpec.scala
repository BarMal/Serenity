package com.serenity

import com.serenity.ui.layout.{DirtyLineDiff, TextCaretStop, TextLayoutSnapshot, TextVisualLine}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The dirty-line diff is the safety-critical half of dirty-region rendering: a row it reports clean keeps the pixels
  * an earlier frame left behind, so every input that can change a row's pixels has to make it dirty.
  */
class DirtyLineDiffSpec extends AnyFlatSpec with Matchers:

  private def visualLine(bufferLine: Int, text: String, startColumn: Int = 0): TextVisualLine =
    TextVisualLine(
      bufferLine = bufferLine,
      startColumn = startColumn,
      endColumn = startColumn + text.length,
      text = text,
      widthPx = text.length * 8.0f,
      caretStops = (0 to text.length).toVector.map(offset => TextCaretStop(startColumn + offset, offset * 8.0f))
    )

  private def snapshot(lines: Vector[TextVisualLine], panelWidthPx: Int = 320): TextLayoutSnapshot =
    TextLayoutSnapshot(
      visualLines = lines,
      panelWidthPx = panelWidthPx,
      lineHeightPx = 16,
      ascentPx = 13
    )

  private val threeLines = snapshot(
    Vector(
      visualLine(0, "alpha"),
      visualLine(1, "beta"),
      visualLine(2, "gamma")
    )
  )

  "DirtyLineDiff" should "mark every row dirty when there is no previous frame" in {
    DirtyLineDiff.dirtyRows(None, threeLines) shouldBe Set(0, 1, 2)
  }

  it should "report no dirty rows when nothing changed" in {
    DirtyLineDiff.dirtyRows(Some(threeLines), threeLines) shouldBe Set.empty[Int]
  }

  it should "report no dirty rows for an equal but distinct snapshot" in {
    val equalCopy = snapshot(
      Vector(
        visualLine(0, "alpha"),
        visualLine(1, "beta"),
        visualLine(2, "gamma")
      )
    )
    DirtyLineDiff.dirtyRows(Some(threeLines), equalCopy) shouldBe Set.empty[Int]
  }

  it should "mark only the edited row dirty when a character is typed mid-line" in {
    val edited = snapshot(
      Vector(
        visualLine(0, "alpha"),
        visualLine(1, "beXta"),
        visualLine(2, "gamma")
      )
    )
    DirtyLineDiff.dirtyRows(Some(threeLines), edited) shouldBe Set(1)
  }

  it should "mark every row dirty when the view scrolled down one line" in {
    val scrolled = snapshot(
      Vector(
        visualLine(1, "beta"),
        visualLine(2, "gamma"),
        visualLine(3, "delta")
      )
    )
    DirtyLineDiff.dirtyRows(Some(threeLines), scrolled) shouldBe Set(0, 1, 2)
  }

  it should "mark only the rows scrolled into view dirty when the pane grew" in {
    val taller = snapshot(
      Vector(
        visualLine(0, "alpha"),
        visualLine(1, "beta"),
        visualLine(2, "gamma"),
        visualLine(3, "delta"),
        visualLine(4, "epsilon")
      )
    )
    DirtyLineDiff.dirtyRows(Some(threeLines), taller) shouldBe Set(3, 4)
  }

  it should "report no dirty rows when the pane shrank but the surviving rows are unchanged" in {
    val shorter = snapshot(Vector(visualLine(0, "alpha"), visualLine(1, "beta")))
    DirtyLineDiff.dirtyRows(Some(threeLines), shorter) shouldBe Set.empty[Int]
  }

  it should "mark every row dirty when the pane width changed" in {
    val narrower = snapshot(threeLines.visualLines, panelWidthPx = 240)
    DirtyLineDiff.dirtyRows(Some(threeLines), narrower) shouldBe Set(0, 1, 2)
  }

  it should "mark every row dirty when the row metrics changed" in {
    val taller = threeLines.copy(lineHeightPx = 20)
    DirtyLineDiff.dirtyRows(Some(threeLines), taller) shouldBe Set(0, 1, 2)
  }

  it should "mark every row dirty when the layout mode changed" in {
    val measured = threeLines.copy(usesMeasuredLayout = true)
    DirtyLineDiff.dirtyRows(Some(threeLines), measured) shouldBe Set(0, 1, 2)
  }

  it should "mark rows dirty when their horizontal offset changed but the text did not" in {
    val shifted = snapshot(
      threeLines.visualLines.updated(1, threeLines.visualLines(1).copy(xOffsetPx = 12.0f))
    )
    DirtyLineDiff.dirtyRows(Some(threeLines), shifted) shouldBe Set(1)
  }

  "DirtyLineDiff with row style keys" should "report no dirty rows when neither layout nor style changed" in {
    val keys = Vector("plain", "plain", "plain")
    DirtyLineDiff.dirtyRows(Some(threeLines), threeLines, keys, keys) shouldBe Set.empty[Int]
  }

  it should "mark only the row whose style key changed when the cursor moved" in {
    val before = Vector("no-cursor", "cursor@2", "no-cursor")
    val after  = Vector("no-cursor", "no-cursor", "cursor@4")
    DirtyLineDiff.dirtyRows(Some(threeLines), threeLines, before, after) shouldBe Set(1, 2)
  }

  it should "union layout dirt and style dirt" in {
    val edited = snapshot(
      Vector(
        visualLine(0, "alpha!"),
        visualLine(1, "beta"),
        visualLine(2, "gamma")
      )
    )
    val before = Vector("plain", "plain", "plain")
    val after  = Vector("plain", "plain", "selected")
    DirtyLineDiff.dirtyRows(Some(threeLines), edited, before, after) shouldBe Set(0, 2)
  }

  it should "mark every row dirty when the style keys do not line up with the rows" in {
    val before = Vector("plain", "plain")
    val after  = Vector("plain", "plain", "plain")
    DirtyLineDiff.dirtyRows(Some(threeLines), threeLines, before, after) shouldBe Set(0, 1, 2)
  }

  it should "mark every row dirty when there is no previous frame even with matching style keys" in {
    val keys = Vector("plain", "plain", "plain")
    DirtyLineDiff.dirtyRows(None, threeLines, Vector.empty, keys) shouldBe Set(0, 1, 2)
  }

  "DirtyLineDiff.dilate" should "grow the dirty set by one row in each direction" in {
    DirtyLineDiff.dilate(Set(2), rowCount = 5) shouldBe Set(1, 2, 3)
  }

  it should "clamp dilation to the available rows" in {
    DirtyLineDiff.dilate(Set(0, 4), rowCount = 5) shouldBe Set(0, 1, 3, 4)
  }

  it should "leave an empty dirty set empty" in {
    DirtyLineDiff.dilate(Set.empty, rowCount = 5) shouldBe Set.empty[Int]
  }
