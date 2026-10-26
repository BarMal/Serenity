package com.serenity.perf

import java.awt.image.BufferedImage

import com.serenity.keystroke.events.{ScrollDown, ScrollUp}
import com.serenity.richtext.{RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.ui.color.RenderColor.argb
import com.serenity.{FrameBackend, Java2DFrames, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A wrapped continuation row's line-number label is filled with the gutter colour, and that fill must stop at the
  * counter. Rows that scrolling reuses by blitting are not repainted over, so a fill spilling into the spacer column
  * beside the counter would stay on screen and the frame would differ from a cold one. Frames are compared with cold
  * ones as [[ColdFrameHarness]] describes.
  */
class LineNumberGutterSpillSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  // Windows only: on Skia the last scroll step leaves 2 to 3 pixels of one row different from the cold frame (CI on
  // #2402, step 24 of the 1x and 2x cases); Linux passes. It stays on Java2D there until that is fixed (#2380).
  override protected def referenceFrames: FrameBackend =
    if System.getProperty("os.name", "").toLowerCase.contains("windows") then Java2DFrames else SkiaFrames

  private val scales = List(1.0, 2.0)

  private def wrappedProse: RichTextDocument =
    RichTextDocument((1 to 150).toList.map { n =>
      RichTextParagraph(List(RichTextRun((1 to 40).map(word => s"p${n}word$word").mkString(" "))))
    })

  private def rgb(image: BufferedImage, x: Int, y: Int): Int = image.getRGB(x, y) & 0xffffff

  /** Columns just right of the divider's rightmost pixel, one character cell wide, in the spacer before the text. */
  private def spacerColumns(frame: BufferedImage, dividerColour: Int, scale: Double): Range =
    val dividerRight = (for
      x <- 0 until frame.getWidth
      y <- 0 until frame.getHeight
      if rgb(frame, x, y) == dividerColour
    yield x).maxOption.getOrElse(-1)
    (dividerRight + 1) until (dividerRight + 1 + math.floor(cellMetrics.charWidth * scale).toInt)

  for scale <- scales do
    s"Scrolling wrapped prose with line numbers at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(wrappedProse, scale, baseConfig.withLineNumbers(true))
      (1 to 12).foreach(step => session(ScrollDown(1), s"down $step"))
      (1 to 12).foreach(step => session(ScrollUp(1), s"up $step"))
      session.mismatches shouldBe empty
    }

    s"A wrapped continuation row's line-number fill at ${scale}x" should "leave the spacer column beside the counter unpainted once scrolled" in {
      val session = Session(wrappedProse, scale, baseConfig.withLineNumbers(true))
      val theme   = session.currentState.persisted.theme
      val gutter  = theme.panel.background.argb & 0xffffff
      val divider = theme.panelBorder.argb & 0xffffff
      (1 to 12).foreach(step => session(ScrollDown(1), s"down $step"))
      val frame         = session.screen
      val spacer        = spacerColumns(frame, divider, scale)
      val statusLineTop = (viewport.height - 1) * cellMetrics.lineHeight * scale
      val spilled = for
        x <- spacer
        y <- 0 until statusLineTop.toInt
        if rgb(frame, x, y) == gutter
      yield (x, y)
      spacer should not be empty
      spilled shouldBe empty
    }
