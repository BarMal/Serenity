package com.serenity.ui.renderer

import com.serenity.config.{AppMode, ProseMeasure}
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.{FramePixels, PersistedFrameFixture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A glyph's antialiasing can land a pixel past the left edge of the text column, in the spacer beside it. A frame that
  * draws the row again has to clear that pixel as it clears the rest of the row, and one that keeps the row has to keep
  * it, or the incremental frame shows a fringe a cold frame does not (#2016, macOS and Windows).
  *
  * "Cleared or kept" is observed on the image itself, as in [[com.serenity.ScrollReusePixelsSpec]]: a sentinel pixel is
  * stamped in the pooled image the next frame draws into, one pixel left of the column.
  */
class SpacerMarginClearSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val sentinel = 0xff00ff

  private val scale = 2.0

  private def proseState(cursorLine: Int): AppState =
    val base = stateOf(proseLines.mkString("\n"), CursorPosition(cursorLine, 0))
    base.copy(persisted =
      base.persisted.copy(config =
        base.persisted.config.withAppMode(AppMode.Prose).withProseMeasure(Some(ProseMeasure.Default))
      )
    )

  private def stamped(image: FramePixels, x: Int, y: Int): Boolean = (image.rgbAt(x, y) & 0xffffff) == sentinel

  /** The middle of the band the edit redraws, found by drawing the edit once, so the sentinel lands inside a row the
    * frame draws again whatever height the rows are drawn at.
    */
  private def redrawnBandMidY(before: AppState, after: AppState, columnLeftPx: Int): Int =
    val probe = Session(scale)
    val _     = probe.settle(before)
    val _     = probe.draw(after, DamageProducer.forTransition(before, after))
    val bands = probe.repaintRegion.getOrElse(fail("expected a bounded repaint")).filter { rect =>
      rect.xPx <= (columnLeftPx - 1) / scale && (columnLeftPx - 1) / scale < rect.xPx + rect.widthPx
    }
    val top    = bands.map(_.yPx).minOption.getOrElse(fail("expected a repaint rect over the column's edge"))
    val bottom = bands.map(rect => rect.yPx + rect.heightPx).max
    math.round((top + bottom) / 2.0 * scale).toInt

  "A row drawn again" should "clear the pixel left of the text column that its glyphs can reach" in {
    val before  = proseState(cursorLine = 1)
    val after   = typedAt(before, proseLines, line = 1, text = "x")
    val session = Session(scale)
    val plan    = session.settle(before)

    val layout = plan.paneLayouts.getOrElse(paneId, fail("expected the pane's layout"))
    plan.layoutContract.leftSpacerRect.width should be > 0

    val columnLeftPx = math.round(cellMetrics.toPixelX(layout.contentRect.x) * scale).toInt
    val topPx        = math.round(cellMetrics.toPixelY(layout.contentRect.y) * scale).toInt
    val editedRowY   = redrawnBandMidY(before, after, columnLeftPx)
    val keptRowY     = topPx + 700
    val image        = session.pendingImage
    image.stamp(columnLeftPx - 1, editedRowY, sentinel)
    image.stamp(columnLeftPx - 1, keptRowY, sentinel)

    val (drawn, _) = session.draw(after, DamageProducer.forTransition(before, after))

    withClue(s"column left at $columnLeftPx, edited row y=$editedRowY: ") {
      stamped(drawn, columnLeftPx - 1, editedRowY) shouldBe false
    }
    withClue(s"column left at $columnLeftPx, kept row y=$keptRowY: ") {
      stamped(drawn, columnLeftPx - 1, keptRowY) shouldBe true
    }
  }
