package com.serenity.ui.layout

import scala.util.Random

import com.serenity.ui.layout.WrapFixtures.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Typing into a paragraph must cost a bounded number of rows and measured characters, not the whole paragraph. */
class IncrementalWrapWorkSpec extends AnyFlatSpec with Matchers:

  private val rows = 40

  private def worked(layoutName: String)(change: String => String): (WrapStats, WrapStats, Int) =
    val layout = layouts.find(_.name == layoutName).get
    val width  = 36 * layout.charWidthPx + 2
    val before = paragraphFor(Random(31L), layout, width, rows)
    val cache  = WrappedLineCache.bounded()
    val cold   = wrap(before, cache, layout, width)
    val first  = cache.wrapStats
    val after  = change(before)
    assertSameAsCold(wrap(after, cache, layout, width), after, layout, width)
    (first, cache.wrapStats, cold.length)

  private def costOf(layoutName: String)(change: String => String): (Long, Long) =
    val (first, second, coldRows) = worked(layoutName)(change)
    coldRows should be >= rows - 5
    (second.rowsComputed - first.rowsComputed, second.charsMeasured - first.charsMeasured)

  "typing a character at the end of a 40-row paragraph" should "re-wrap a handful of rows" in
    Vector("default-prose", "sans", "serif-kerned-ligatured", "mono-cells").foreach { name =>
      val (rowsComputed, _) = costOf(name)(_ + "x")
      withClue(s"$name: ")(rowsComputed should be <= 4L)
    }

  it should "measure a window of characters, not the paragraph" in
    Vector("default-prose", "sans", "serif-kerned-ligatured").foreach { name =>
      val (_, measured) = costOf(name)(_ + "x")
      withClue(s"$name: ")(measured should be <= 120L)
    }

  "typing a character in the middle of a 40-row paragraph" should "re-wrap a handful of rows" in
    Vector("default-prose", "sans", "serif-kerned-ligatured", "mono-cells").foreach { name =>
      val (rowsComputed, measured) = costOf(name)(text => text.patch(text.length / 2, "x", 0))
      withClue(s"$name: ")(rowsComputed should be <= 6L)
      if name != "mono-cells" then withClue(s"$name: ")(measured should be <= 160L)
    }

  "deleting a character at the start of a 40-row paragraph" should "re-wrap a handful of rows" in {
    val (rowsComputed, _) = costOf("default-prose")(_.drop(1))
    rowsComputed should be <= 8L
  }
