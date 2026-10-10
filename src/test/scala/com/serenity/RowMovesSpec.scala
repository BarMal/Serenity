package com.serenity

import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import com.serenity.ui.layout.{PixelRect, RowBands, RowMoves, RowRun, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A moved row is reused only when nothing about its pixels can differ from drawing it again where it now sits: the
  * same visual line, the same distance in every band, and neighbours that moved with it.
  */
class RowMovesSpec extends AnyFlatSpec with Matchers:

  private val rowHeight = 16

  private def visualLine(bufferLine: Int, text: String = "text", startColumn: Int = 0): TextVisualLine =
    TextVisualLine(
      bufferLine = bufferLine,
      startColumn = startColumn,
      endColumn = startColumn + text.length,
      text = text,
      widthPx = text.length * 8.0f,
      caretStops = (0 to text.length).toVector.map(offset => TextCaretStop(startColumn + offset, offset * 8.0f))
    )

  private def snapshot(lines: Vector[TextVisualLine], panelWidthPx: Int = 320): TextLayoutSnapshot =
    TextLayoutSnapshot(visualLines = lines, panelWidthPx = panelWidthPx, lineHeightPx = rowHeight, ascentPx = 13)

  private def uniformBands(rows: Int): Vector[PixelRect] =
    Vector.tabulate(rows)(row => PixelRect(0, row * rowHeight, 100, rowHeight))

  private def lines(range: Range): Vector[TextVisualLine] = range.toVector.map(visualLine(_))

  private def runs(
    previous: TextLayoutSnapshot,
    current: TextLayoutSnapshot,
    unreusable: Set[Int] = Set.empty,
    bands: Option[RowBands] = None
  ): List[RowRun] =
    RowMoves.reusableRuns(
      previous,
      current,
      List(
        bands.getOrElse(
          RowBands(uniformBands(previous.visualLines.length), uniformBands(current.visualLines.length))
        )
      ),
      unreusable
    )

  "RowMoves" should "reuse the interior of a scroll, leaving the exposed rows and the rows beside them to be drawn" in {
    val found = runs(snapshot(lines(0 until 10)), snapshot(lines(2 until 12)))

    found shouldBe List(RowRun(firstRow = 1, rowCount = 6, fromRow = 3))
  }

  it should "reuse nothing when no row moved" in {
    val same = snapshot(lines(0 until 10))

    runs(same, same) shouldBe Nil
  }

  it should "reuse nothing when the view jumped past every row it held" in {
    runs(snapshot(lines(0 until 10)), snapshot(lines(50 until 60))) shouldBe Nil
  }

  it should "split a run around rows that must be drawn, and draw their neighbours too" in {
    val found = runs(snapshot(lines(0 until 10)), snapshot(lines(2 until 12)), unreusable = Set(4))

    found shouldBe List(RowRun(1, 2, 3), RowRun(6, 1, 8))
  }

  it should "not reuse a row whose text differs, even on the same line" in {
    val edited = snapshot(lines(2 until 12).updated(4, visualLine(6, "edited")))

    runs(snapshot(lines(0 until 10)), edited).flatMap(_.rows) should not contain 4
  }

  it should "find the rows a wrapped row pushed down" in {
    val before = snapshot(Vector(visualLine(0), visualLine(1)) ++ lines(2 until 9))
    val after =
      snapshot(Vector(visualLine(0), visualLine(1), visualLine(1, "wrapped", startColumn = 40)) ++ lines(2 until 9))

    runs(before, after) shouldBe List(RowRun(firstRow = 4, rowCount = 6, fromRow = 3))
  }

  it should "not reuse rows whose band is not as tall as it was" in {
    val previous  = snapshot(lines(0 until 10))
    val current   = snapshot(lines(2 until 12))
    val shortened = uniformBands(10).zipWithIndex.map((band, row) => if row >= 5 then band.copy(heightPx = 9) else band)

    val found = runs(previous, current, bands = Some(RowBands(uniformBands(10), shortened)))

    found.flatMap(_.rows).filter(_ >= 5) shouldBe empty
  }

  it should "require every band to agree on the distance" in {
    val previous = snapshot(lines(0 until 10))
    val current  = snapshot(lines(2 until 12))
    val content  = RowBands(uniformBands(10), uniformBands(10))
    val numbers  = RowBands(uniformBands(10), uniformBands(10).map(band => band.copy(yPx = band.yPx + 3)))

    RowMoves.reusableRuns(previous, current, List(content, numbers), Set.empty) shouldBe Nil
  }

  it should "reuse nothing when the layout around the rows changed" in {
    runs(snapshot(lines(0 until 10)), snapshot(lines(2 until 12), panelWidthPx = 400)) shouldBe Nil
  }

  it should "give up on a frame that would need more copies than drawing is worth" in {
    val scattered = (3 until 40 by 4).toSet

    runs(snapshot(lines(0 until 40)), snapshot(lines(2 until 42)), unreusable = scattered) shouldBe Nil
  }
