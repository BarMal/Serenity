package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.theme.TextStyle
import com.serenity.{FrameBackend, FramePixels, Java2DFrames, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a run paints on a [[RenderSurface]]: its background only where the pixels are not already that colour, its
  * glyphs inside the clip in force, and an underline as a rule rather than a font attribute. The scenes and assertions
  * are shared by every backend; each asserts pixel colours and ink presence only, never the shape of a glyph, so the
  * tolerance between rasterisers is none needed.
  */
abstract class RunPaintBehaviour extends AnyFlatSpec with Matchers:

  protected def frames: FrameBackend

  private val metrics    = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val font       = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val width      = 210
  private val height     = 130
  private val navy       = new Color(10, 20, 60)
  private val marker     = 0xff00ff00
  private val runLeftPx  = 14
  private val runTopPx   = 26
  private val runWidthPx = 56

  private def freshImage(): FramePixels = frames.newFrame(width, height)

  private def surfaceOver(image: FramePixels): RenderSurface =
    frames.surfaceOver(image, metrics, font, width, height, 1.0, persists = false, RenderCaches.create())

  private def drawBlankRun(surface: RenderSurface, background: Color): Unit =
    surface.setBackgroundColor(RenderColor.fromAwt(background))
    surface.text.drawRunPx(runLeftPx.toFloat, runTopPx, runWidthPx.toFloat, metrics.lineHeight, metrics.ascent, "")

  private val insideRun = (runLeftPx + 20, runTopPx + 5)

  private def markInsideRun(image: FramePixels): Unit = image.stamp(insideRun._1, insideRun._2, marker)

  private def pixelInsideRun(image: FramePixels): Int = image.rgbAt(insideRun._1, insideRun._2)

  "A run whose background is the colour the viewport was just cleared to" should "not paint that background again" in {
    val image   = freshImage()
    val surface = surfaceOver(image)
    surface.clearViewport(RenderColor.fromAwt(navy))
    markInsideRun(image)

    drawBlankRun(surface, navy)

    pixelInsideRun(image) shouldBe marker
  }

  it should "not paint again after the viewport was cleared around preserved rectangles that miss the run" in {
    val image   = freshImage()
    val surface = surfaceOver(image)
    surface.clearViewportExcept(RenderColor.fromAwt(navy), List(PixelRect(150, 0, 40, 40)))
    markInsideRun(image)

    drawBlankRun(surface, navy)

    pixelInsideRun(image) shouldBe marker
  }

  it should "not paint again inside a pixel translation" in {
    val image   = freshImage()
    val surface = surfaceOver(image)
    surface.clearViewport(RenderColor.fromAwt(navy))
    markInsideRun(image)

    surface.pixels.withPixelTranslation(0.0, 0.0)(drawBlankRun(surface, navy))

    pixelInsideRun(image) shouldBe marker
  }

  "A run whose background differs from the cleared colour" should "still paint its background" in {
    val image   = freshImage()
    val surface = surfaceOver(image)
    surface.clearViewport(RenderColor.fromAwt(navy))
    markInsideRun(image)

    drawBlankRun(surface, Color.RED)

    pixelInsideRun(image) shouldBe Color.RED.getRGB
  }

  "A run on a surface that was never cleared" should "paint its background" in {
    val image   = freshImage()
    val surface = surfaceOver(image)
    markInsideRun(image)

    drawBlankRun(surface, navy)

    pixelInsideRun(image) shouldBe navy.getRGB
  }

  "A run over pixels another draw touched since the clear" should "paint its background" in {
    val image   = freshImage()
    val surface = surfaceOver(image)
    surface.clearViewport(RenderColor.fromAwt(navy))
    surface.pixels.fillPixelRect(runLeftPx, runTopPx, 6, metrics.lineHeight, RenderColor.fromAwt(Color.RED))
    markInsideRun(image)

    drawBlankRun(surface, navy)

    pixelInsideRun(image) shouldBe navy.getRGB
    image.rgbAt(runLeftPx + 2, runTopPx + 2) shouldBe navy.getRGB
  }

  "A run in a rectangle the clear preserved" should "paint its background" in {
    val image   = freshImage()
    val surface = surfaceOver(image)
    surface.clearViewportExcept(
      RenderColor.fromAwt(navy),
      List(PixelRect(runLeftPx - 4, runTopPx - 4, runWidthPx + 8, metrics.lineHeight + 8))
    )
    markInsideRun(image)

    drawBlankRun(surface, navy)

    pixelInsideRun(image) shouldBe navy.getRGB
  }

  "A run on a translucent clear colour" should "paint its background, which blends rather than replaces" in {
    val translucent = new Color(10, 20, 60, 120)
    val image       = freshImage()
    val surface     = surfaceOver(image)
    surface.clearViewport(RenderColor.fromAwt(translucent))
    markInsideRun(image)

    drawBlankRun(surface, translucent)

    pixelInsideRun(image) should not be marker
  }

  "A run painted while a translucency scale is active" should "paint its background" in {
    val image   = freshImage()
    val surface = surfaceOver(image)
    surface.clearViewport(RenderColor.fromAwt(navy))
    markInsideRun(image)

    surface.effects.getOrElse(fail("expected alpha compositing")).setAlpha(0.5f)
    drawBlankRun(surface, navy)

    pixelInsideRun(image) should not be marker
  }

  "A row of runs" should "paint the same pixels as when every background is repainted" in {
    val runs = List(
      (14.0f, 28.0f, "Hello", navy, Color.WHITE),
      (42.0f, 35.0f, "World", Color.DARK_GRAY, Color.YELLOW),
      (77.0f, 28.0f, "again", navy, Color.WHITE),
      (105.5f, 28.5f, "last.", navy, Color.CYAN)
    )
    def paint(forceEveryFill: Boolean): FramePixels =
      val image   = freshImage()
      val surface = surfaceOver(image)
      surface.clearViewport(RenderColor.fromAwt(navy))
      if forceEveryFill then surface.pixels.fillPixelRect(0, 0, width, height, RenderColor.fromAwt(navy))
      List(26, 52).foreach { top =>
        runs.foreach { (x, runWidth, text, background, foreground) =>
          surface.setBackgroundColor(RenderColor.fromAwt(background))
          surface.setForegroundColor(RenderColor.fromAwt(foreground))
          surface.text.drawRunPx(x, top, runWidth, metrics.lineHeight, metrics.ascent, text, clipGlyphToRun = true)
        }
      }
      surface.flush()
      image

    val skipped   = paint(forceEveryFill = false)
    val repainted = paint(forceEveryFill = true)

    val differing = skipped.rgbPixels.zip(repainted.rgbPixels).zipWithIndex.collect {
      case ((left, right), index) if left != right => (index % width, index / width)
    }
    differing shouldBe empty
  }

  "A glyph run" should "stay inside the clip in force and leave that clip in force afterwards" in {
    val image   = freshImage()
    val surface = surfaceOver(image)
    surface.clearViewport(RenderColor.fromAwt(Color.BLACK))
    surface.setForegroundColor(RenderColor.fromAwt(Color.WHITE))
    surface.setBackgroundColor(RenderColor.fromAwt(Color.BLACK))

    surface.panelOutlines.getOrElse(fail("expected rect clipping")).withRectClip(1, 2, 2, 1) {
      surface.text.drawRunPx(0f, 26, 140f, metrics.lineHeight, metrics.ascent, "MMMMMMMMMMMMMMMMMMMM")
      surface.text.drawRunPx(0f, 26, 140f, metrics.lineHeight, metrics.ascent, "MMMMMMMMMMMMMMMMMMMM")
    }
    val insideClip  = inkIn(image, 7, 21, 26, 39)
    val outsideClip = inkIn(image, 0, 7, 26, 39) + inkIn(image, 21, 140, 26, 39)
    insideClip should be > 0
    outsideClip shouldBe 0

    surface.text.drawRunPx(0f, 52, 140f, metrics.lineHeight, metrics.ascent, "MMMMMMMMMMMMMMMMMMMM")
    inkIn(image, 0, 7, 52, 65) should be > 0
    inkIn(image, 100, 140, 52, 65) should be > 0
  }

  "An underlined run" should "draw a rule under its text across the run" in {
    val image   = freshImage()
    val surface = surfaceOver(image)
    surface.clearViewport(RenderColor.fromAwt(Color.BLACK))
    surface.setForegroundColor(RenderColor.fromAwt(Color.WHITE))
    surface.setBackgroundColor(RenderColor.fromAwt(Color.BLACK))

    surface.enableStyle(TextStyle.underlined)
    surface.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "MMMMMMMM")
    surface.disableStyle(TextStyle.underlined)
    surface.text.drawRunPx(14f, 52, 56f, metrics.lineHeight, metrics.ascent, "MMMMMMMM")

    hasFullInkRow(image, 26) shouldBe true
    hasFullInkRow(image, 52) shouldBe false
  }

  private def inkIn(image: FramePixels, left: Int, right: Int, top: Int, bottom: Int): Int =
    (for
      y <- top until bottom
      x <- left until right
      if image.rgbAt(x, y) != Color.BLACK.getRGB
    yield 1).size

  private def hasFullInkRow(image: FramePixels, top: Int): Boolean =
    ((top + metrics.ascent) until (top + metrics.lineHeight + 2)).exists { y =>
      (runLeftPx + 2 until runLeftPx + runWidthPx - 2).forall(x => image.rgbAt(x, y) != Color.BLACK.getRGB)
    }

class Java2DRunPaintSpec extends RunPaintBehaviour:
  override protected def frames: FrameBackend = Java2DFrames

class SkiaRunPaintSpec extends RunPaintBehaviour:
  override protected def frames: FrameBackend = SkiaFrames
