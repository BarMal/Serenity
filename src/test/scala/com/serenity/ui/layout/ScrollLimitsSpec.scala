package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1948 phase 1: one answer to "how far can a pane scroll", in the pane's own rows. */
class ScrollLimitsSpec extends AnyFlatSpec with Matchers:

  "ScrollLimits.rowsThatFit" should "count the whole rows of the pane's font in the pane's height" in {
    ScrollLimits.rowsThatFit(paneHeightPx = 600, rowHeightPx = 17) shouldBe 35
    ScrollLimits.rowsThatFit(paneHeightPx = 600, rowHeightPx = 15) shouldBe 40
  }

  it should "never be fewer than one row, and survive a row height of nothing" in {
    ScrollLimits.rowsThatFit(paneHeightPx = 10, rowHeightPx = 17) shouldBe 1
    ScrollLimits.rowsThatFit(paneHeightPx = 600, rowHeightPx = 0) shouldBe 600
  }

  "ScrollLimits.last" should "rest the final screen of rows at the bottom of the document" in {
    ScrollLimits.last(VisualRowCounts.oneRowPerLine(100), rowsThatFit = 40) shouldBe ScrollLimit(60, 0)
  }

  it should "rest at the top of a document shorter than the pane" in {
    ScrollLimits.last(VisualRowCounts.oneRowPerLine(5), rowsThatFit = 40) shouldBe ScrollLimit(0, 0)
  }

  it should "count a wrapped last paragraph in rows, not lines" in {
    val rows = VisualRowCounts.walking(lineCount = 10, measure = line => if line == 9 then 6 else 1)

    // 6 rows in the last line and 4 more above it fill 10 rows.
    ScrollLimits.last(rows, rowsThatFit = 10) shouldBe ScrollLimit(5, 0)
    // A pane of 4 rows rests part-way into the last paragraph.
    ScrollLimits.last(rows, rowsThatFit = 4) shouldBe ScrollLimit(9, 2)
  }

  "A scroll limit" should "leave a position at or above it alone" in {
    val limit = ScrollLimit(60, 2)

    limit.clamp(59, 5) shouldBe ((59, 5))
    limit.clamp(60, 2) shouldBe ((60, 2))
    limit.clamp(0, 0) shouldBe ((0, 0))
  }

  it should "pull a position below it back to it" in {
    val limit = ScrollLimit(60, 2)

    limit.clamp(61, 0) shouldBe ((60, 2))
    limit.clamp(60, 3) shouldBe ((60, 2))
    limit.clamp(500, 9) shouldBe ((60, 2))
  }
