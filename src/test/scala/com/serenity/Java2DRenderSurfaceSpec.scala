package com.serenity

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.Java2DRenderSurface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Java2DRenderSurfaceSpec extends AnyFlatSpec with Matchers:

  "Java2DRenderSurface.strokeRoundRect" should "respect the active alpha composite when drawing borders" in {
    val lowAlphaImage  = new BufferedImage(80, 60, BufferedImage.TYPE_INT_ARGB)
    val fullAlphaImage = new BufferedImage(80, 60, BufferedImage.TYPE_INT_ARGB)
    val metrics        = CellMetrics(charWidth = 10, lineHeight = 10, ascent = 8)
    val font           = new Font(Font.MONOSPACED, Font.PLAIN, 12)

    val lowAlphaSurface =
      new Java2DRenderSurface(lowAlphaImage, metrics, font, _ => ())
    lowAlphaSurface.setAlpha(0.25f)
    lowAlphaSurface.strokeRoundRect(1, 1, 4, 3, arcPx = 0, color = java.awt.Color.WHITE, strokeWidth = 2.0f)
    lowAlphaSurface.flush()

    val fullAlphaSurface =
      new Java2DRenderSurface(fullAlphaImage, metrics, font, _ => ())
    fullAlphaSurface.setAlpha(1.0f)
    fullAlphaSurface.strokeRoundRect(1, 1, 4, 3, arcPx = 0, color = java.awt.Color.WHITE, strokeWidth = 2.0f)
    fullAlphaSurface.flush()

    maxAlpha(lowAlphaImage) should be < maxAlpha(fullAlphaImage)
  }

  private def maxAlpha(image: BufferedImage): Int =
    (for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
    yield (image.getRGB(x, y) >>> 24) & 0xff).max
