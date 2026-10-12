package com.serenity

import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1839: chrome -- line numbers, pane headers, spacers -- is part of the pixels a persisting surface keeps. A frame
  * that edits one text row must leave every other row's line number, the header and the spacers exactly as they were
  * (not repainted), and must still equal a frame drawn from scratch to the last device pixel, at the laptop's 2x scale.
  *
  * "Not repainted" is observed on the image itself: a sentinel colour is stamped into the pooled image the next frame
  * will draw into, and survives only if nothing repainted those pixels.
  */
abstract class ChromeRepaintPixelsBehaviour extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val scale    = 2.0
  private val sentinel = 0xff00ff

  /** A device pixel just inside the top-left corner of the cell `rowOffset` rows down `cells`, away from glyph ink. */
  private def stamp(image: FramePixels, cells: LayoutRect, rowOffset: Int = 0): (Int, Int) =
    val x = math.round(cellMetrics.toPixelX(cells.x) * scale).toInt + 1
    val y = math.round(cellMetrics.toPixelY(cells.y + rowOffset) * scale).toInt + 1
    image.stamp(x, y, sentinel)
    (x, y)

  private def survives(image: FramePixels, at: (Int, Int)): Boolean =
    (image.rgbAt(at._1, at._2) & 0xffffff) == sentinel

  private def nonEmpty(rect: LayoutRect): Boolean = rect.width > 0 && rect.height > 0

  "A keystroke frame" should "leave other rows' line numbers, the pane header and the spacers unpainted" in {
    val before  = stateOf(codeLines.mkString("\n"), CursorPosition(10, 0), language = Some(LanguageId.Scala))
    val after   = typedAt(before, codeLines, line = 10, text = "x")
    val session = Session()
    val plan    = session.settle(before)

    val gutter = plan.layoutContract.lineNumberRect.getOrElse(fail("expected a line-number rect"))
    val header = plan.layoutContract.paneHeaderRect(paneId).getOrElse(fail("expected a pane header"))
    val spacer = List(plan.layoutContract.leftSpacerRect, plan.layoutContract.rightSpacerRect)
      .find(nonEmpty)
      .getOrElse(fail("expected a left or right spacer"))
    header.height should be > 0

    val target     = session.pendingImage
    val farRow     = stamp(target, gutter, rowOffset = 25)
    val editedRow  = stamp(target, gutter, rowOffset = 10)
    val headerMark = stamp(target, header)
    val spacerMark = stamp(target, spacer)

    val (drawn, _) = session.draw(after, DamageProducer.forTransition(before, after))

    withClue("line number of a row the edit did not touch: ")(survives(drawn, farRow) shouldBe true)
    withClue("pane header: ")(survives(drawn, headerMark) shouldBe true)
    withClue("spacer: ")(survives(drawn, spacerMark) shouldBe true)
    withClue("line number of the edited row: ")(survives(drawn, editedRow) shouldBe false)
  }

  it should "repaint the pane header when the keystroke changes it" in {
    val before =
      stateOf(codeLines.mkString("\n"), CursorPosition(10, 0), dirty = false, language = Some(LanguageId.Scala))
    val after   = typedAt(before, codeLines, line = 10, text = "x")
    val session = Session()
    val plan    = session.settle(before)

    val header     = plan.layoutContract.paneHeaderRect(paneId).getOrElse(fail("expected a pane header"))
    val headerMark = stamp(session.pendingImage, header)

    val (drawn, _) = session.draw(after, DamageProducer.forTransition(before, after))

    withClue("pane header after the buffer turned dirty: ")(survives(drawn, headerMark) shouldBe false)
  }

  it should "equal a frame drawn from scratch to the last device pixel, prose at 2x" in {
    val first   = stateOf(proseLines.mkString("\n"), CursorPosition(4, 5))
    val second  = typedAt(first, proseLines, line = 4, text = "x")
    val session = Session()

    val _                = session.settle(first)
    val (afterTyping, _) = session.draw(second, DamageProducer.forTransition(first, second))
    withClue("typing: ")(differingPixels(afterTyping, cold(second)) shouldBe empty)

    val (afterUndo, _) = session.draw(first, DamageProducer.forTransition(second, first))
    withClue("undo: ")(differingPixels(afterUndo, cold(first)) shouldBe empty)
  }

  it should "equal a frame drawn from scratch when a line number gains a digit" in {
    val short   = Vector.tabulate(98)(line => s"line $line")
    val first   = stateOf(short.mkString("\n"), CursorPosition(10, 0))
    val second  = typedAt(first, short, line = 97, text = "extra\nlines\nmore")
    val session = Session()

    val _          = session.settle(first)
    val (drawn, _) = session.draw(second, DamageProducer.forTransition(first, second))

    differingPixels(drawn, cold(second)) shouldBe empty
  }

class ChromeRepaintPixelsSpec extends ChromeRepaintPixelsBehaviour

/** The same assertions with Skia painting into off-screen raster surfaces. Whether a row was repainted or copied is
  * read from sentinel pixels and whether a frame equals a cold one is compared against Skia's own cold frame, so none
  * of it depends on how Skia and Java2D rasterise a glyph differently: the tolerance is zero because both sides of
  * every comparison come from the same rasteriser.
  */
class SkiaChromeRepaintPixelsSpec extends ChromeRepaintPixelsBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
