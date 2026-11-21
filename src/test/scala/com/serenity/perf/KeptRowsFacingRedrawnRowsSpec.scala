package com.serenity.perf

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.ColumnPagingFixture
import com.serenity.ui.layout.PixelRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A glyph can put ink on the line just past its row, as macOS draws it, and that line is the first (or last) line of
  * the row beside it. Where that row is kept, a frame that redraws the row beside it would neither clear nor paint the
  * line, and it would show the last frame's ink there. A kept row therefore gives up the line that faces a row drawn
  * again, and the frame clears it, paints the redrawn row's ink on it, and paints the kept row's own ink on it again.
  */
class KeptRowsFacingRedrawnRowsSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private val columnConfig: AppConfig =
    baseConfig
      .withColumnMode(true)
      .withColumnCount(Some(2))
      .withWordWrap(true)
      .withHeadingsConfig(ColumnPagingFixture.spacing)

  private def overlapsInX(a: PixelRect, b: PixelRect): Boolean = a.xPx < b.rightPx && b.xPx < a.rightPx

  private def linesBeside(redrawn: PixelRect): List[PixelRect] =
    List(
      PixelRect(redrawn.xPx, redrawn.bottomPx, redrawn.widthPx, 1),
      PixelRect(redrawn.xPx, redrawn.yPx - 1, redrawn.widthPx, 1)
    )

  private def keptLinesBesideRedrawnRows(session: Session): List[(PixelRect, PixelRect)] =
    val plan  = session.planOfLastFrame.getOrElse(fail("expected a bounded frame"))
    val rects = plan.repaintRegion.getOrElse(fail("expected a bounded repaint")).sortBy(_.yPx)
    val rows  = if plan.chrome.statusRow then rects.dropRight(1) else rects
    for
      row  <- rows
      line <- linesBeside(row)
      kept <- plan.keptBands
      if overlapsInX(line, kept) && kept.yPx <= line.yPx && line.yPx < kept.bottomPx
    yield (line, kept)

  for (name, config) <- List("a single column" -> baseConfig, "two columns" -> columnConfig) do
    s"A typing frame in $name" should "keep no line beside a row it draws again" in {
      val session = Session(ColumnPagingFixture.document, 1.0, config)
      session.markModified()
      session.placeCursor(3, 30)
      (1 to 6).foreach { n =>
        session(InsertChar('w'), s"type $n")
        withClue(s"step $n: ")(keptLinesBesideRedrawnRows(session) shouldBe empty)
      }
      session.mismatches shouldBe empty
    }
