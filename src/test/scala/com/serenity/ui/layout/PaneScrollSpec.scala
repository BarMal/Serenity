package com.serenity.ui.layout

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Where a pane sits in its document, counted in the visual rows its lines wrap into. */
class PaneScrollSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def threeRowsAtTwo(line: Int): Int = if line == 2 then 3 else 1

  "A short document's extent" should "count every row, including those a wrapped line adds" in {
    val rows = VisualRowCounts.walking(10, threeRowsAtTwo)

    PaneScroll.of(rows, topLine = 0, topVisualLine = 0, visibleRows = 4).extent shouldBe
      ScrollExtent(firstRow = 0, visibleRows = 4, totalRows = 12)
  }

  it should "place the first row after the wrapped lines above the top line" in {
    val rows = VisualRowCounts.walking(10, threeRowsAtTwo)

    PaneScroll.of(rows, topLine = 4, topVisualLine = 0, visibleRows = 4).extent.firstRow shouldBe 6
  }

  it should "place the first row inside a wrapped line when the view starts part-way through it" in {
    val rows = VisualRowCounts.walking(10, threeRowsAtTwo)

    PaneScroll.of(rows, topLine = 2, topVisualLine = 1, visibleRows = 4).extent.firstRow shouldBe 3
  }

  "A pane scroll" should "turn a first row back into the line and row the viewport rests on" in {
    val scroll = PaneScroll.of(VisualRowCounts.walking(10, threeRowsAtTwo), 0, 0, visibleRows = 4)

    scroll.viewportTop(0) shouldBe ((0, 0))
    scroll.viewportTop(2) shouldBe ((2, 0))
    scroll.viewportTop(3) shouldBe ((2, 1))
    scroll.viewportTop(5) shouldBe ((3, 0))
  }

  it should "not rest beyond the last full screen" in {
    val scroll = PaneScroll.of(VisualRowCounts.walking(10, threeRowsAtTwo), 0, 0, visibleRows = 4)

    scroll.viewportTop(500) shouldBe scroll.viewportTop(scroll.extent.lastFirstRow)
    scroll.extent.lastFirstRow shouldBe 8
  }

  "A long document's extent" should "count the lines it has not measured as one row each" in {
    val content  = Rope(("word " * 12 + "\n") * 5000)
    val measured = new AtomicInteger(0)
    def tripleAtTheTop(line: Int): Int =
      val _ = measured.incrementAndGet()
      if line < 3 then 3 else 1
    val rows = new VisualLineIndexStore[Unit](1).counts((), content, tripleAtTheTop)

    val scroll = PaneScroll.of(rows, topLine = 0, topVisualLine = 0, visibleRows = 30)

    scroll.extent.totalRows shouldBe content.lineCount + 6
    measured.get() should be <= 30
  }

  it should "stay exact over the lines on screen" in {
    val content = Rope(("word " * 12 + "\n") * 5000)
    val rows    = new VisualLineIndexStore[Unit](1).counts((), content, line => if line % 10 == 0 then 2 else 1)

    val scroll = PaneScroll.of(rows, topLine = 1000, topVisualLine = 0, visibleRows = 30)

    scroll.extent.firstRow shouldBe 1000
    scroll.extent.totalRows shouldBe content.lineCount + 3
    rows.rowsBetween(1000, 1030) shouldBe 33
  }

  it should "find the line at a first row from the rows measured so far, without measuring more" in {
    val content  = Rope(("word " * 12 + "\n") * 5000)
    val measured = new AtomicInteger(0)
    def counting(line: Int): Int =
      val _ = measured.incrementAndGet()
      1
    val scroll = PaneScroll.of(new VisualLineIndexStore[Unit](1).counts((), content, counting), 0, 0, visibleRows = 30)
    measured.set(0)

    scroll.viewportTop(2500) shouldBe ((2500, 0))
    scroll.viewportTop(scroll.extent.lastFirstRow) shouldBe ((scroll.extent.lastFirstRow, 0))
    measured.get() shouldBe 0
  }
