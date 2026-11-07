package com.serenity.ui.layout

import java.awt.Font

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.PaneRowBands.Row
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
