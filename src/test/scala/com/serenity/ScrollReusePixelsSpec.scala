package com.serenity

import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
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
abstract class ScrollReusePixelsBehaviour extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val sentinel = 0xff00ff

  final private case class Column(x: Int, fromY: Int, toY: Int)

  private def stampColumn(image: FramePixels, plan: EditorPaneRenderPlan, scale: Double): Column =
    val content = plan.paneLayouts.getOrElse(paneId, fail("expected the pane's layout")).contentRect
    val column = Column(
      x = math.round(cellMetrics.toPixelX(content.right) * scale).toInt - 3,
      fromY = math.round(cellMetrics.toPixelY(content.y) * scale).toInt,
      toY = math.round(cellMetrics.toPixelY(content.bottom) * scale).toInt
    )
    (column.fromY until column.toY).foreach(y => image.stamp(column.x, y, sentinel))
    column

  /** The share of `column`, between `from` and `to` as fractions of its height, that still carries a stamp. */
  private def stamped(image: FramePixels, column: Column, from: Double = 0.0, to: Double = 1.0): Double =
    val height = column.toY - column.fromY
    val ys     = (column.fromY + (height * from).toInt) until (column.fromY + (height * to).toInt)
    ys.count(y => (image.rgbAt(column.x, y) & 0xffffff) == sentinel).toDouble / ys.size

  private def copiedShare(before: AppState, after: AppState, scale: Double = 2.0): Double =
    val session    = Session(scale)
    val plan       = session.settle(before)
    val column     = stampColumn(session.pendingImage, plan, scale)
    val (image, _) = session.draw(after, DamageProducer.forTransition(before, after))
    stamped(image, column)

  private def drawnFrom(before: AppState, after: AppState): FramePixels =
    val session = Session()
    val _       = session.settle(before)
    session.draw(after, DamageProducer.forTransition(before, after))._1

  "A scroll by one row" should "copy the rows it keeps and draw the exposed row and its neighbours" in {
    val before = stateOf(codeLines.mkString("\n"), CursorPosition(30, 0), topLine = 10)
    val after  = scrolledTo(before, 11)

    withClue("share of the pane copied: ")(copiedShare(before, after) should be >= 0.85)
  }

  it should "equal a frame drawn from scratch to the last device pixel" in {
    val before = stateOf(codeLines.mkString("\n"), CursorPosition(30, 0), topLine = 10)
    val after  = scrolledTo(before, 11)

    differingPixels(drawnFrom(before, after), cold(after)) shouldBe empty
  }

  "A scroll by five rows" should "copy all but the exposed rows and the rows beside them, to the last device pixel" in {
    val before = stateOf(codeLines.mkString("\n"), CursorPosition(30, 0), topLine = 10)
    val after  = scrolledTo(before, 15)

    withClue("share of the pane copied: ")(copiedShare(before, after) should be >= 0.7)
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
    differingPixels(drawnFrom(before, after), cold(after)) shouldBe empty
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

class ScrollReusePixelsSpec extends ScrollReusePixelsBehaviour

/** The same assertions with Skia painting into off-screen raster surfaces. Whether a row was repainted or copied is
  * read from sentinel pixels and whether a frame equals a cold one is compared against Skia's own cold frame, so none
  * of it depends on how Skia and Java2D rasterise a glyph differently: the tolerance is zero because both sides of
  * every comparison come from the same rasteriser.
  */
class SkiaScrollReusePixelsSpec extends ScrollReusePixelsBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
