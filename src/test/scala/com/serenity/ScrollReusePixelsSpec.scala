package com.serenity

import java.awt.image.BufferedImage

import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.{ScrollbarLayout, TextRowMetrics}
import com.serenity.ui.renderer.EditorPaneRenderPlan
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1833: a frame whose rows moved -- a scroll, or a paragraph that gained a wrapped row above them -- copies those
  * rows' pixels instead of drawing them again, and the result is the frame drawn from scratch to the last device pixel
  * (at 2x, with wrapped rows of different heights).
  *
  * "Copied, not redrawn" is observed on the image itself: a column of sentinel pixels is stamped down the pane in the
  * pooled image the next frame draws into. A row that was drawn again is cleared first, so it loses its stamp; a row
  * that was copied carries it to where the row now sits.
  */
class ScrollReusePixelsSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val sentinel = 0xff00ff

  final private case class Column(x: Int, fromY: Int, toY: Int)

  private def stampColumn(image: BufferedImage, plan: EditorPaneRenderPlan, scale: Double): Column =
    val content  = plan.paneLayouts.getOrElse(paneId, fail("expected the pane's layout")).contentRect
    val snapshot = plan.snapshots.getOrElse(paneId, fail("expected the pane's snapshot"))
    val metrics  = TextRowMetrics.forSnapshot(content, cellMetrics, snapshot, snapshot.usesMeasuredLayout)
    // The column covers the rows: the room under the last one is kept as it is whatever the frame copies.
    val rowsBottom = metrics.bandBottomPx(snapshot.visualLines.size - 1).min(metrics.contentBottomPx)
    val column = Column(
      x = math.round(ScrollbarLayout.strip(content, cellMetrics).xPx * scale).toInt - 3,
      fromY = math.round(cellMetrics.toPixelY(content.y) * scale).toInt,
      toY = math.round(rowsBottom * scale).toInt
    )
    (column.fromY until column.toY).foreach(y => image.setRGB(column.x, y, sentinel))
    column

  /** The share of `column`, between `from` and `to` as fractions of its height, that still carries a stamp. */
  private def stamped(image: BufferedImage, column: Column, from: Double = 0.0, to: Double = 1.0): Double =
    val height = column.toY - column.fromY
    val ys     = (column.fromY + (height * from).toInt) until (column.fromY + (height * to).toInt)
    ys.count(y => (image.getRGB(column.x, y) & 0xffffff) == sentinel).toDouble / ys.size

  private def copiedShare(before: AppState, after: AppState, scale: Double = 2.0): Double =
    val session    = Session(scale)
    val plan       = session.settle(before)
    val column     = stampColumn(session.pendingImage, plan, scale)
    val (image, _) = session.draw(after, DamageProducer.forTransition(before, after))
    stamped(image, column)

  /** A scroll copies the rows it keeps and draws the rows it exposes, the rows beside them, and the top row. What it
    * may draw is therefore a number of rows, and a row is as tall as the font makes it (17 px on Linux, 19 on Windows)
    * in a pane whose height is whole cells (555 px on Linux, 544 on Windows). The share left copied is bounded by the
    * rows drawn at most, measured in those: 0.858 and 0.838 for one row, 0.735 and 0.698 for five, against the same
    * bound of five rows and ten rows on both.
    */
  private def copiedAllButRows(before: AppState, after: AppState, drawnRowsAtMost: Int, scale: Double = 2.0): Unit =
    val session    = Session(scale)
    val plan       = session.settle(before)
    val column     = stampColumn(session.pendingImage, plan, scale)
    val rowPx      = plan.snapshots.getOrElse(paneId, fail("expected the pane's snapshot")).lineHeightPx
    val paneHeight = (column.toY - column.fromY) / scale
    val (image, _) = session.draw(after, DamageProducer.forTransition(before, after))
    val share      = stamped(image, column)
    val leastShare = 1.0 - drawnRowsAtMost * rowPx / paneHeight

    withClue(s"share of the pane copied, with rows of ${rowPx}px in a pane ${paneHeight}px tall: ")(
      share should be >= leastShare
    )

  private def drawnFrom(before: AppState, after: AppState): BufferedImage =
    val session = Session()
    val _       = session.settle(before)
    session.draw(after, DamageProducer.forTransition(before, after))._1

  /** macOS draws the gutter's divider on a boundary that falls between device pixels (cold-frame dump of run
    * 39210220486, ScrollReusePixelsSpec "gains wrapped rows": f3 | bb | 95 across x=110..112 down every row), so the
    * one pixel the boundary shares with two fills is blended by coverage, and where a repainted row meets a kept one
    * (y=264) the two frames blend it with a different coverage: 0xb7 against 0xc8 where the other rows have 0xbb. Linux
    * and Windows put the boundary on a pixel edge and the frames agree exactly. Only this pixel kind is forgiven here:
    * at most [[MostEdgePixelsDiffering]] of them, each within [[EdgeChannelTolerance]] per channel of the other frame's
    * and, in both frames, between the colours of the pixel to its left and right on every channel.
    */
  private val EdgeChannelTolerance    = 0x18
  private val MostEdgePixelsDiffering = 2

  private def isEdgeCoverage(a: BufferedImage, b: BufferedImage, x: Int, y: Int): Boolean =
    def channels(image: BufferedImage, px: Int): Seq[Int] =
      val rgb = image.getRGB(px, y)
      Seq((rgb >> 16) & 0xff, (rgb >> 8) & 0xff, rgb & 0xff)
    def between(image: BufferedImage): Boolean =
      channels(image, x - 1).zip(channels(image, x)).zip(channels(image, x + 1)).forall {
        case ((left, here), right) => here >= math.min(left, right) && here <= math.max(left, right)
      }
    x > 0 && x < a.getWidth - 1 &&
    channels(a, x).zip(channels(b, x)).forall((first, second) => math.abs(first - second) <= EdgeChannelTolerance) &&
    between(a) && between(b)

  "A scroll by one row" should "copy the rows it keeps and draw the exposed row and its neighbours" in {
    val before = stateOf(codeLines.mkString("\n"), CursorPosition(30, 0), topLine = 10)
    val after  = scrolledTo(before, 11)

    copiedAllButRows(before, after, drawnRowsAtMost = 5)
  }

  it should "equal a frame drawn from scratch to the last device pixel" in {
    val before = stateOf(codeLines.mkString("\n"), CursorPosition(30, 0), topLine = 10)
    val after  = scrolledTo(before, 11)

    differingPixels(drawnFrom(before, after), cold(after)) shouldBe empty
  }

  "A scroll by five rows" should "copy all but the exposed rows and the rows beside them, to the last device pixel" in {
    val before = stateOf(codeLines.mkString("\n"), CursorPosition(30, 0), topLine = 10)
    val after  = scrolledTo(before, 15)

    copiedAllButRows(before, after, drawnRowsAtMost = 10)
    differingPixels(drawnFrom(before, after), cold(after)) shouldBe empty
  }

  "A scroll up" should "equal a frame drawn from scratch, copying rows the other way" in {
    val before = stateOf(codeLines.mkString("\n"), CursorPosition(30, 0), topLine = 12)
    val after  = scrolledTo(before, 9)

    withClue("share of the pane copied: ")(copiedShare(before, after) should be >= 0.7)
    differingPixels(drawnFrom(before, after), cold(after)) shouldBe empty
  }

  "A scroll through wrapped prose" should "copy the rows that kept their height and equal a frame drawn from scratch" in {
    val before = stateOf(proseLines.mkString("\n"), CursorPosition(12, 0), topLine = 8)
    val after  = scrolledTo(before, 11)

    withClue("share of the pane copied: ")(copiedShare(before, after) should be >= 0.3)
    differingPixels(drawnFrom(before, after), cold(after)) shouldBe empty
  }

  "A paragraph that gains wrapped rows" should "push the rows below it down by copying them" in {
    val before = stateOf(proseLines.mkString("\n"), CursorPosition(1, 0))
    val after  = typedAt(before, proseLines, line = 1, text = "inserted words " * 12)

    val session    = Session()
    val plan       = session.settle(before)
    val column     = stampColumn(session.pendingImage, plan, 2.0)
    val (image, _) = session.draw(after, DamageProducer.forTransition(before, after))

    withClue("share of the lower half of the pane copied: ")(stamped(image, column, from = 0.5) should be >= 0.3)
    val drawn    = drawnFrom(before, after)
    val expected = cold(after)
    val (edges, others) =
      differingPixels(drawn, expected).partition((x, y) => isEdgeCoverage(drawn, expected, x, y))
    withClue("pixels differing beyond a boundary's antialiasing: ")(others shouldBe empty)
    withClue("boundary pixels differing in antialiasing: ")(edges.size should be <= MostEdgePixelsDiffering)
  }

  "A scroll through monospace code" should "equal a frame drawn from scratch to the last device pixel" in {
    val before =
      stateOf(codeLines.mkString("\n"), CursorPosition(30, 0), topLine = 10, language = Some(LanguageId.Scala))
    val after = scrolledTo(before, 13)

    differingPixels(drawnFrom(before, after), cold(after)) shouldBe empty
  }

  "A scroll at a fractional device scale" should "draw every row, as there is no whole-pixel copy to make" in {
    val before = stateOf(codeLines.mkString("\n"), CursorPosition(30, 0), topLine = 10)
    val after  = scrolledTo(before, 11)

    copiedShare(before, after, scale = 1.5) should be < 0.02
  }

  private def strip(values: Int*): BufferedImage =
    val image = new BufferedImage(values.size, 1, BufferedImage.TYPE_INT_RGB)
    values.zipWithIndex.foreach((value, x) => image.setRGB(x, 0, value * 0x010101))
    image

  "The boundary tolerance" should "forgive a pixel between its neighbours that differs a little" in {
    isEdgeCoverage(strip(0xf3, 0xb7, 0x95), strip(0xf3, 0xc8, 0x95), 1, 0) shouldBe true
  }

  it should "not forgive a pixel that differs by more than the tolerance" in {
    isEdgeCoverage(strip(0xf3, 0x9a, 0x95), strip(0xf3, 0xe0, 0x95), 1, 0) shouldBe false
  }

  it should "not forgive a pixel outside the colours around it" in {
    isEdgeCoverage(strip(0xf3, 0x20, 0x95), strip(0xf3, 0x30, 0x95), 1, 0) shouldBe false
  }
