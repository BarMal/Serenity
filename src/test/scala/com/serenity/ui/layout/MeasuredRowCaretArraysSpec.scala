package com.serenity.ui.layout

import java.awt.Font

import com.serenity.state.models.{MeasuredRow, TextCaretStop}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A measured row keeps its caret stops as primitive arrays with offsets relative to its start column, so a cached row
  * holds no per-stop objects and moving it to another column is a field change (#1908).
  */
class MeasuredRowCaretArraysSpec extends AnyFlatSpec with Matchers:

  private val font = Font(Font.SANS_SERIF, Font.PLAIN, 13)

  private def sameArray(first: Any, second: Any): Boolean = first.asInstanceOf[AnyRef] eq second.asInstanceOf[AnyRef]

  private def wrapped(text: String, baseColumn: Int): Vector[MeasuredRow] =
    TextLayoutSnapshot
      .boundedVisualLinesForText(text, 0, 120, font, baseColumn = baseColumn, wrapCache = WrappedLineCache.bounded())
      .map(_.row)

  "A measured row" should "hold one x and one start-relative offset per grapheme boundary" in {
    val row = wrapped("abcd efgh ijkl mnop qrst", baseColumn = 40).head

    row.caretXs.length shouldBe row.caretOffsets.length
    row.caretOffsets.head shouldBe 0
    row.caretCount shouldBe row.caretOffsets.length
    row.caretOffsets.indices.foreach(index => row.caretColumn(index) shouldBe row.startColumn + row.caretOffsets(index))
    row.caretStops shouldBe row.caretOffsets.indices
      .map(index => TextCaretStop(row.caretColumn(index), row.caretXs(index)))
      .toVector
  }

  it should "share the caret arrays with its x-sorted view when the stops already ascend" in {
    val row = wrapped("abcd efgh", baseColumn = 0).head

    sameArray(row.landingXs, row.caretXs) shouldBe true
    sameArray(row.landingOffsets, row.caretOffsets) shouldBe true
  }

  it should "order right-to-left stops by x without disturbing the caret order" in {
    val row = MeasuredRow.fromStops(
      startColumn = 10,
      endColumn = 13,
      text = "abc",
      widthPx = 26.5f,
      stops = Vector(
        TextCaretStop(10, 26.5f),
        TextCaretStop(11, 15.8f),
        TextCaretStop(12, 6.6f),
        TextCaretStop(13, 26.5f)
      )
    )

    row.caretStops.map(_.column) shouldBe Vector(10, 11, 12, 13)
    row.xSortedCaretStops shouldBe Vector(
      TextCaretStop(12, 6.6f),
      TextCaretStop(11, 15.8f),
      TextCaretStop(10, 26.5f),
      TextCaretStop(13, 26.5f)
    )
    row.nearestColumnForXPx(15.8f) shouldBe 11
    row.nearestColumnForXPx(6.6f) shouldBe 12
    row.xForColumn(12) shouldBe Some(6.6f)
  }

  it should "move to another start column without touching its arrays" in {
    val row     = wrapped("abcd efgh ijkl", baseColumn = 5).head
    val shifted = row.copy(startColumn = row.startColumn + 3, endColumn = row.endColumn + 3)

    sameArray(shifted.caretXs, row.caretXs) shouldBe true
    sameArray(shifted.caretOffsets, row.caretOffsets) shouldBe true
    shifted.caretStops.map(_.column) shouldBe row.caretStops.map(_.column + 3)
    shifted.caretStops.map(_.xPx) shouldBe row.caretStops.map(_.xPx)
  }

  it should "answer an empty row without stops" in {
    val row = MeasuredRow.fromStops(0, 0, "", 0.0f, Vector.empty)

    row.xForColumn(0) shouldBe None
    row.nearestColumnForXPx(12.0f) shouldBe 0
    row.caretCount shouldBe 0
  }

  it should "equal an identical measurement held in different arrays, and differ when one stop moves" in {
    val row    = wrapped("abcd efgh", baseColumn = 0).head
    val copied = row.copy(caretXs = row.caretXs.map(x => x), landingXs = row.landingXs.map(x => x))
    val moved  = row.copy(caretXs = row.caretXs.updated(1, row.caretXs(1) + 1.0f))

    copied shouldBe row
    copied.hashCode shouldBe row.hashCode
    moved should not be row
  }
