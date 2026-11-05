package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Color, Font}

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LabelSpritesSpec extends AnyFlatSpec with Matchers:

  private val font    = Font(Font.SERIF, Font.PLAIN, 14)
  private val metrics = CellMetrics.fromFont(Font(Font.MONOSPACED, Font.PLAIN, 12))
  private val white   = RenderColor.fromAwt(Color.WHITE)
  private val black   = RenderColor.fromAwt(Color.BLACK)

  private def drawn(sprites: LabelSprites, scale: Double = 2.0)(draw: Java2DRenderSurface => Unit): BufferedImage =
    val image = new BufferedImage((200 * scale).toInt, (60 * scale).toInt, BufferedImage.TYPE_INT_RGB)
    val surface = new Java2DRenderSurface(
      image,
      metrics,
      font,
      _ => (),
      logicalWidthPx = 200,
      logicalHeightPx = 60,
      deviceScaleX = scale,
      deviceScaleY = scale,
      textCaches = SurfaceTextCaches.fresh().copy(labelSprites = sprites)
    )
    surface.setBackgroundColor(white)
    surface.setForegroundColor(black)
    surface.fillRect(0, 0, surface.viewportWidth, surface.viewportHeight, ' ')
    draw(surface)
    surface.flush()
    image

  private def asRun(label: String, xPx: Float)(surface: Java2DRenderSurface): Unit =
    surface.drawRunPx(xPx, 8, 60f, 20, 15, label)

  private def asLabel(label: String, xPx: Float, yPx: Int = 8)(surface: Java2DRenderSurface): Unit =
    surface.drawLabelPx(xPx, yPx, 60f, 20, 15, label)

  private def maxChannelDifference(a: BufferedImage, b: BufferedImage): Int =
    (for
      x     <- 0 until a.getWidth
      y     <- 0 until a.getHeight
      shift <- List(0, 8, 16)
    yield math.abs(((a.getRGB(x, y) >> shift) & 0xff) - ((b.getRGB(x, y) >> shift) & 0xff))).max

  private def inkBounds(image: BufferedImage): (Int, Int, Int, Int) =
    val ink = for
      x <- 0 until image.getWidth
      y <- 0 until image.getHeight
      if (image.getRGB(x, y) & 0xffffff) != 0xffffff
    yield (x, y)
    (ink.map(_._1).min, ink.map(_._2).min, ink.map(_._1).max, ink.map(_._2).max)

  "LabelSprites" should "draw a label that starts on a sixteenth of a pixel as the run would, within blend rounding" in {
    val sprites = LabelSprites(16)
    val run     = drawn(sprites)(asRun("1234", 10.375f))
    val label   = drawn(sprites)(asLabel("1234", 10.375f))

    maxChannelDifference(run, label) should be <= 4
  }

  it should "place a label that starts between sixteenths on the same ink bounds as the run" in {
    val sprites = LabelSprites(16)
    val run     = drawn(sprites)(asRun("1234", 10.37f))
    val label   = drawn(sprites)(asLabel("1234", 10.37f))

    inkBounds(label) shouldBe inkBounds(run)
    maxChannelDifference(run, label) should be <= 48
  }

  it should "serve a repeated label from its cache, wherever on the surface it lands" in {
    val sprites = LabelSprites(16)

    val _ = drawn(sprites) { surface =>
      asLabel("4096", 10.5f, yPx = 4)(surface)
      asLabel("4096", 10.5f, yPx = 4)(surface)
      asLabel("4096", 10.5f, yPx = 30)(surface)
    }

    sprites.rasterised shouldBe 1L
    sprites.size shouldBe 1
  }

  it should "never hold more sprites than its capacity" in {
    val sprites = LabelSprites(8)

    val _ = drawn(sprites)(surface => (1 to 50).foreach(number => asLabel(number.toString, 10.5f)(surface)))

    sprites.size should be <= 8
    sprites.rasterised shouldBe 50L
  }

  it should "draw as the run does, without sprites, at a fractional device scale" in {
    val sprites = LabelSprites(16)
    val run     = drawn(sprites, scale = 1.5)(asRun("1234", 10.5f))
    val label   = drawn(sprites, scale = 1.5)(asLabel("1234", 10.5f))

    maxChannelDifference(run, label) shouldBe 0
    sprites.rasterised shouldBe 0L
  }

  private val keptRect = PixelRect(14, 12, 10, 6)

  private def inKeptRect(x: Int, y: Int): Boolean =
    x >= keptRect.xPx * 2 && x < (keptRect.xPx + keptRect.widthPx) * 2 && y >= keptRect.yPx * 2 &&
      y < (keptRect.yPx + keptRect.heightPx) * 2

  private def labelDrawnOffKept(kept: List[PixelRect]): BufferedImage =
    drawn(LabelSprites(16))(surface => surface.withoutPixels(kept)(asLabel("1234", 10.5f)(surface)))

  it should "leave the pixels of a kept rect as they were, as a clip on the surface does (#2481)" in {
    val image = labelDrawnOffKept(List(keptRect))

    val touched = for
      x <- 0 until image.getWidth
      y <- 0 until image.getHeight
      if inKeptRect(x, y) && (image.getRGB(x, y) & 0xffffff) != 0xffffff
    yield (x, y)
    touched shouldBe empty
  }

  it should "paint everywhere else as a label drawn with nothing kept, once however the kept rect splits it (#2481)" in {
    val withKept  = labelDrawnOffKept(List(keptRect))
    val unclipped = labelDrawnOffKept(Nil)

    val differing = for
      x <- 0 until withKept.getWidth
      y <- 0 until withKept.getHeight
      if !inKeptRect(x, y) && withKept.getRGB(x, y) != unclipped.getRGB(x, y)
    yield (x, y)
    differing shouldBe empty
  }
