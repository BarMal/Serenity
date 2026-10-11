package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.layout.TextCaretMeasurement.{ColumnFontRun, FontSpan, LineFontResolver}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

class RunLookupAndCaretRepairSpec extends AnyFlatSpec with Matchers:

  private val base  = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val fonts = Vector(10, 14, 18, 22).map(new Font(Font.MONOSPACED, Font.PLAIN, _))

  /** Every third column is a gap, so lookups hit runs, run edges and uncovered columns alike. */
  private def manyRuns(count: Int): Vector[ColumnFontRun] =
    Vector.tabulate(count)(i => ColumnFontRun(i * 3, i * 3 + 2, fonts(i % fonts.size), hidden = i % 7 == 0))

  private def linearRunAt(runs: Vector[ColumnFontRun], column: Int): Option[ColumnFontRun] =
    runs.find(run => column >= run.startColumn && column < run.endColumn)

  private def linearFontAt(runs: Vector[ColumnFontRun], column: Int): Font =
    linearRunAt(runs, column).map(_.font).getOrElse(base)

  private def linearFontRuns(runs: Vector[ColumnFontRun], start: Int, end: Int): Vector[FontSpan] =
    val cuts = (runs
      .flatMap(run => Vector(run.startColumn, run.endColumn))
      .filter(column => column > start && column < end) :+ start :+ end).distinct.sorted
    cuts.zip(cuts.drop(1)).foldLeft(Vector.empty[FontSpan]) {
      case (merged :+ previous, (_, cutEnd))
          if linearFontAt(runs, cutEnd - 1) == previous.font &&
            linearRunAt(runs, cutEnd - 1).exists(_.hidden) == previous.hidden =>
        merged :+ previous.copy(endColumn = cutEnd)
      case (merged, (cutStart, cutEnd)) =>
        merged :+ FontSpan(
          cutStart,
          cutEnd,
          linearFontAt(runs, cutStart),
          linearRunAt(runs, cutStart).exists(_.hidden)
        )
    }

  private def sameArray(expected: IArray[Float]): Matcher[IArray[Float]] =
    Matcher { actual =>
      MatchResult(
        actual.asInstanceOf[AnyRef] eq expected.asInstanceOf[AnyRef],
        "was not the same array",
        "was the same array"
      )
    }

  "LineFontResolver.fontAt" should "match a linear scan on every column of a many-run line" in {
    val runs     = manyRuns(1500)
    val resolver = LineFontResolver(base, runs)
    (-2 to 4510).foreach { column =>
      withClue(s"column $column: ")(resolver.fontAt(column) shouldBe linearFontAt(runs, column))
      withClue(s"column $column: ")(resolver.isHidden(column) shouldBe linearRunAt(runs, column).exists(_.hidden))
    }
  }

  it should "match a linear scan when runs overlap, as the first listed run wins" in {
    val runs = Vector(
      ColumnFontRun(0, 10, fonts(0)),
      ColumnFontRun(4, 6, fonts(1)),
      ColumnFontRun(8, 20, fonts(2)),
      ColumnFontRun(2, 3, fonts(3))
    )
    val resolver = LineFontResolver(base, runs)
    (-1 to 22).foreach(column => resolver.fontAt(column) shouldBe linearFontAt(runs, column))
  }

  it should "match a linear scan when runs are listed out of order" in {
    val runs     = manyRuns(40).reverse
    val resolver = LineFontResolver(base, runs)
    (-1 to 125).foreach(column => resolver.fontAt(column) shouldBe linearFontAt(runs, column))
  }

  "LineFontResolver.fontRuns" should "match the linear cut-and-merge over windows of a many-run line" in {
    val runs     = manyRuns(300)
    val resolver = LineFontResolver(base, runs)
    val windows = for start <- Vector(0, 1, 2, 3, 100, 451); end <- Vector(0, 1, 5, 6, 200, 899, 1000)
    yield (start, end)
    windows.foreach { (start, end) =>
      withClue(s"[$start, $end): ")(resolver.fontRuns(start, end) shouldBe linearFontRuns(runs, start, end))
    }
  }

  "TextCaretMeasurement.normalizeCollapsedCarets" should "hand back the very array it was given when no caret collapsed" in {
    val carets = IArray(0.0f, 7.0f, 14.0f, 21.0f, 28.0f)
    TextCaretMeasurement.normalizeCollapsedCarets(carets) should sameArray(carets)
  }

  it should "still spread a collapsed run of carets over the width it covers" in {
    val carets = IArray(0.0f, 7.0f, 21.0f, 21.0f, 21.0f, 28.0f)
    val spread = TextCaretMeasurement.normalizeCollapsedCarets(carets)
    spread.toList.zip(List(0.0f, 7.0f, 11.666667f, 16.333334f, 21.0f, 28.0f)).foreach { (actual, expected) =>
      actual shouldBe expected +- 0.001f
    }
  }

  it should "leave carets of one or two stops alone" in {
    val short = IArray(0.0f, 5.0f)
    TextCaretMeasurement.normalizeCollapsedCarets(short) should sameArray(short)
  }
