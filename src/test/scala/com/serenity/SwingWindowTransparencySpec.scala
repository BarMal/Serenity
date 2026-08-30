package com.serenity

import java.awt.Color
import java.awt.image.BufferedImage

import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** GUI-mode handling of the alpha-0 "transparent background" sentinel (#1240): a theme with an alpha-0 background (the
  * built-in "Transparent" theme, or any user theme opting into the same sentinel) should only make the canvas paint
  * through to the desktop when the window can genuinely composite that -- custom chrome with per-pixel translucency
  * support -- and must otherwise fall back to an ordinary opaque paint rather than compositing garbage.
  */
class SwingWindowTransparencySpec extends AnyFlatSpec with Matchers:

  "SwingWindow.shouldPaintTransparentContent" should "paint through only with custom chrome and translucency support" in {
    SwingWindow.shouldPaintTransparentContent(
      usesCustomChrome = true,
      perPixelTranslucencySupported = true,
      backgroundAlpha = 0
    ) shouldBe true

    SwingWindow.shouldPaintTransparentContent(
      usesCustomChrome = false,
      perPixelTranslucencySupported = true,
      backgroundAlpha = 0
    ) shouldBe false

    SwingWindow.shouldPaintTransparentContent(
      usesCustomChrome = true,
      perPixelTranslucencySupported = false,
      backgroundAlpha = 0
    ) shouldBe false

    SwingWindow.shouldPaintTransparentContent(
      usesCustomChrome = true,
      perPixelTranslucencySupported = true,
      backgroundAlpha = 255
    ) shouldBe false
  }

  "SwingWindow.paintCanvasBackground" should "write fully transparent pixels over stale opaque content when transparent" in {
    val image = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    try
      g.setColor(Color.RED)
      g.fillRect(0, 0, 10, 10)
      SwingWindow.paintCanvasBackground(g, 10, 10, transparent = true)
    finally g.dispose()

    new Color(image.getRGB(5, 5), true).getAlpha shouldBe 0
  }

  it should "paint opaque black as the non-translucent fallback" in {
    val image = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    try SwingWindow.paintCanvasBackground(g, 10, 10, transparent = false)
    finally g.dispose()

    new Color(image.getRGB(5, 5), true) shouldBe new Color(0, 0, 0, 255)
  }
end SwingWindowTransparencySpec
