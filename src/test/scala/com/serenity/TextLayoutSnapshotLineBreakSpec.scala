package com.serenity

import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// #1277 step 4: wordBoundarySegmentLength now breaks on real UAX#14 line-break boundaries
// (BreakIterator.getLineInstance) rather than scanning for a preceding whitespace character. These use a uniform
// (non-display-width-aware) 1-cell-per-character grid so they isolate the line-break decision itself from
// CharWidth's East Asian Width handling (already covered by CharWidthSpec). Split out of TextLayoutSnapshotSpec to
// keep that file under the architecture ratchet's file-length target.
class TextLayoutSnapshotLineBreakSpec extends AnyFlatSpec with Matchers:

  "TextLayoutSnapshot" should "break between adjacent CJK ideographs with no whitespace, per UAX#14" in {
    val font = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    val lines = TextLayoutSnapshot.boundedVisualLinesForText(
      "上海市浦东新区", // 7 CJK ideographs, no whitespace anywhere
      bufferLine = 0,
      panelWidthPx = 3,
      font,
      cellMetricsOverride = Some(CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)),
      forceCellLayout = true
    )

    lines.map(_.text) shouldBe Vector("上海市", "浦东新", "区")
  }

  it should "keep a following Latin word whole rather than force-breaking mid-word after a CJK run" in {
    // Verified directly against ICU4J before writing this: "abc漢字defgh" has line-break boundaries at
    // {0, 3, 4, 5, 10} -- the script transition after "字" (index 5) is a legal break, so with 6 characters fitting
    // the panel, the old ad hoc implementation (which only ever looked for a preceding whitespace character, found
    // none, and force-broke at the fitting length) would have split "defgh" into "d" + "efgh". UAX#14 line breaking
    // defers to the earlier, correct boundary at the script transition instead.
    val font = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    val lines = TextLayoutSnapshot.boundedVisualLinesForText(
      "abc漢字defgh",
      bufferLine = 0,
      panelWidthPx = 6, // fits "abc漢字d" (6 chars) by raw character count alone
      font,
      cellMetricsOverride = Some(CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)),
      forceCellLayout = true
    )

    lines.map(_.text) shouldBe Vector("abc漢字", "defgh")
  }

  it should "never break at a no-break space, unlike a plain space" in {
    // Verified against ICU4J: "10 km away" has line-break boundaries only at {0, 6, 10} -- none between "10"
    // and "km" across the no-break space at index 2, only after the plain space before "away" (index 6). With a
    // 5-character-wide panel, no legal boundary falls at or before the fitting length (the only one behind it is 0),
    // so this forces the whole "10 km" run onto one line rather than splitting inside it.
    val font = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)

    val lines = TextLayoutSnapshot.boundedVisualLinesForText(
      "10 km away", // U+00A0 no-break space between "10" and "km"
      bufferLine = 0,
      panelWidthPx = 5, // fits "10 km" (5 chars) by raw character count alone
      font,
      cellMetricsOverride = Some(CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)),
      forceCellLayout = true
    )

    lines.map(_.text) shouldBe Vector("10 km", " away")
  }
