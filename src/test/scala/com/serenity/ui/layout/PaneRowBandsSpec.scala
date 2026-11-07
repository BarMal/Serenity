package com.serenity.ui.layout

import java.awt.Font

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.PaneRowBands.Row
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

/** Placing a view reads several limits off the rows above the caret. They share one walk of those rows (#2618): the
  * rows are measured once, not once per limit, and the answer is the one a walk of its own gives.
  */
class PaneRowBandsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font = Font(Font.MONOSPACED, Font.PLAIN, 12)

  private def bandsOf(lines: Int): PaneRowBands =
    val text = (0 until lines).map(i => s"line $i ${"word " * (i % 9)}").mkString("\n")
    PaneRowBands.forBuffer(
      Buffer.fromString(BufferId(1), text),
      wrapWidthPx = 200,
      font = font,
      cellMetricsOverride = None,
      forceCellLayout = false,
      wordWrapEnabled = true,
      richText = RichTextContext.plain,
      wrapCache = WrappedLineCache.Uncached
    )

  private val carets = List(Row(0, 0), Row(3, 1), Row(60, 0), Row(199, 2))
  private val grids  = List((16, 5), (16, 12), (20, 1))

  "topPainting over a walk" should "give the top a walk of its own gives" in {
    val bands = bandsOf(200)
    for
      caret               <- carets
      (gridRowPx, target) <- grids
    do
      withClue(s"caret $caret, grid row $gridRowPx px, target row $target: ")(
        bands.topPainting(bands.tops(caret), caret, gridRowPx, target) shouldBe
          bands.topPainting(caret, gridRowPx, target)
      )
  }

  it should "give the same top when another limit has already read part of the walk" in {
    val bands = bandsOf(200)
    for
      caret               <- carets
      (gridRowPx, target) <- grids
    do
      val walk = bands.tops(caret)
      val _    = bands.topWithin(walk, caret, 40)
      withClue(s"caret $caret, grid row $gridRowPx px, target row $target: ")(
        bands.topPainting(walk, caret, gridRowPx, target) shouldBe bands.topPainting(caret, gridRowPx, target)
      )
  }

  "Bands reading a window of lines in one pass" should "measure every row as bands reading line by line do" in {
    val random = Random(2618)
    for _ <- 0 until 60 do
      val lineCount = 1 + random.nextInt(60)
      val text = (0 until lineCount)
        .map(i => if random.nextInt(8) == 0 then "" else s"line $i ${"word " * random.nextInt(30)}")
        .mkString("\n")
      val buffer     = Buffer.fromString(BufferId(1), text)
      val caret      = Row(random.nextInt(lineCount), 0)
      val readAhead  = PaneRowBands.ReadAhead(random.nextInt(lineCount + 6) - 3, random.nextInt(45))
      val windowed   = bandsOver(buffer, readAhead)
      val unwindowed = bandsOver(buffer, PaneRowBands.ReadAhead.None)
      withClue(s"$lineCount lines, caret $caret, $readAhead: ") {
        windowed.tops(caret).take(90).toList shouldBe unwindowed.tops(caret).take(90).toList
        windowed.textBoxPx(caret) shouldBe unwindowed.textBoxPx(caret)
        windowed.belowPx(caret, 240) shouldBe unwindowed.belowPx(caret, 240)
        windowed.topPainting(caret, 16, 5) shouldBe unwindowed.topPainting(caret, 16, 5)
        windowed.topWithin(windowed.tops(caret), caret, 120) shouldBe
          unwindowed.topWithin(unwindowed.tops(caret), caret, 120)
      }
  }

  it should "read nothing for a window past the document's end" in {
    val buffer = Buffer.fromString(BufferId(1), "one\ntwo")
    val bands  = bandsOver(buffer, PaneRowBands.ReadAhead(50, 10))
    bands.tops(Row(1, 0)).take(5).toList shouldBe bandsOver(buffer, PaneRowBands.ReadAhead.None)
      .tops(Row(1, 0))
      .take(5)
      .toList
  }

  private def bandsOver(buffer: Buffer, readAhead: PaneRowBands.ReadAhead): PaneRowBands =
    PaneRowBands.forBuffer(
      buffer,
      wrapWidthPx = 200,
      font = font,
      cellMetricsOverride = None,
      forceCellLayout = false,
      wordWrapEnabled = true,
      richText = RichTextContext.plain,
      wrapCache = WrappedLineCache.Uncached,
      readAhead = readAhead
    )
