package com.serenity

import java.awt.image.BufferedImage

import com.serenity.config.StatusLinePlacement
import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutRect, TextRowMetrics}
import com.serenity.ui.renderer.EditorPaneRenderPlan
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1839: chrome -- line numbers, pane headers, spacers -- is part of the pixels a persisting surface keeps. A frame
  * that edits one text row must leave every other row's line number, the header and the spacers exactly as they were
  * (not repainted), and must still equal a frame drawn from scratch to the last device pixel, at the laptop's 2x scale.
  *
  * "Not repainted" is observed on the image itself: a sentinel colour is stamped into the pooled image the next frame
  * will draw into, and survives only if nothing repainted those pixels.
  */
class ChromeRepaintPixelsSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val scale    = 2.0
  private val sentinel = 0xff00ff

  /** A device pixel just inside the top-left corner of the cell `rowOffset` rows down `cells`, away from glyph ink. */
  private def stamp(image: BufferedImage, cells: LayoutRect, rowOffset: Int = 0): (Int, Int) =
    val x = math.round(cellMetrics.toPixelX(cells.x) * scale).toInt + 1
    val y = math.round(cellMetrics.toPixelY(cells.y + rowOffset) * scale).toInt + 1
    image.setRGB(x, y, sentinel)
    (x, y)

  private def survives(image: BufferedImage, at: (Int, Int)): Boolean =
    (image.getRGB(at._1, at._2) & 0xffffff) == sentinel

  private def nonEmpty(rect: LayoutRect): Boolean = rect.width > 0 && rect.height > 0

  private def deviceX(logicalPx: Int): Int = math.round(logicalPx * scale).toInt
  private def deviceY(logicalPx: Int): Int = math.round(logicalPx * scale).toInt

  private def layoutOf(plan: EditorPaneRenderPlan) = plan.paneLayouts.getOrElse(paneId, fail("expected a pane layout"))

  /** The logical pixel rows between the pane's last text row and the bottom of its content rect. */
  private def roomBelowLastRow(plan: EditorPaneRenderPlan): (Int, Int) =
    val snapshot = plan.snapshots.getOrElse(paneId, fail("expected a snapshot"))
    val metrics =
      TextRowMetrics.forSnapshot(layoutOf(plan).contentRect, cellMetrics, snapshot, snapshot.usesMeasuredLayout)
    (metrics.bandBottomPx(snapshot.visualLines.size - 1), metrics.contentBottomPx)

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

  it should "leave the room below the last row unpainted" in {
    val before  = stateOf(proseLines.mkString("\n"), CursorPosition(4, 5))
    val after   = typedAt(before, proseLines, line = 4, text = "x")
    val session = Session()
    val plan    = session.settle(before)

    val (roomTop, roomBottom) = roomBelowLastRow(plan)
    roomBottom - roomTop should (be > 0 and be < cellMetrics.lineHeight)
    val target = session.pendingImage
    val mark   = (deviceX(cellMetrics.toPixelX(layoutOf(plan).contentRect.x)) + 1, deviceY(roomTop) + 1)
    target.setRGB(mark._1, mark._2, sentinel)

    val (drawn, _) = session.draw(after, DamageProducer.forTransition(before, after))

    survives(drawn, mark) shouldBe true
  }

  it should "equal a frame drawn from scratch when the last row appears and the room below it goes" in {
    val fewer   = proseLines.take(31)
    val first   = stateOf(fewer.mkString("\n"), CursorPosition(4, 5))
    val second  = typedAt(first, fewer, line = 30, text = "x\nmore")
    val session = Session()

    val _          = session.settle(first)
    val (drawn, _) = session.draw(second, DamageProducer.forTransition(first, second))

    differingPixels(drawn, cold(second)) shouldBe empty
  }

  it should "equal a frame drawn from scratch at the fractional device scales of a Windows display" in
    List(1.25, 1.5, 1.75).foreach { windowScale =>
      val first   = stateOf(proseLines.mkString("\n"), CursorPosition(4, 5))
      val second  = typedAt(first, proseLines, line = 4, text = "x")
      val session = Session(windowScale)

      val _           = session.settle(first)
      val (drawn, _)  = session.draw(second, DamageProducer.forTransition(first, second))
      val (undone, _) = session.draw(first, DamageProducer.forTransition(second, first))

      withClue(s"typing at $windowScale: ")(differingPixels(drawn, cold(second, windowScale)) shouldBe empty)
      withClue(s"undo at $windowScale: ")(differingPixels(undone, cold(first, windowScale)) shouldBe empty)
    }

  it should "repaint a changed status row on both pooled images, equal to a frame drawn from scratch" in {
    def pinned(state: AppState): AppState =
      state.copy(persisted =
        state.persisted.copy(config = state.persisted.config.withStatusLinePlacement(StatusLinePlacement.Pinned))
      )
    val first   = pinned(stateOf(proseLines.mkString("\n"), CursorPosition(4, 5), dirty = false))
    val second  = pinned(typedAt(first, proseLines, line = 4, text = "x"))
    val session = Session()

    val _              = session.settle(first)
    val (firstDraw, _) = session.draw(second, DamageProducer.forTransition(first, second))
    val (otherDraw, _) = session.draw(second, Damage.Nothing)

    withClue("image drawn on the changing frame: ")(differingPixels(firstDraw, cold(second)) shouldBe empty)
    withClue("the other pooled image, one frame later: ")(differingPixels(otherDraw, cold(second)) shouldBe empty)
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
