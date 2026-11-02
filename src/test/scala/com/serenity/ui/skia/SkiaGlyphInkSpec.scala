package com.serenity.ui.skia

import java.awt.{Color, Font}

import com.serenity.SkiaPixels
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, StyledFontCache}
import org.jetbrains.skia.{Paint, Surface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Text is drawn in a colour shader because Skia blits anti-aliased text in a shader's paint several times faster than
  * in a plain-colour one. The ink must come out as it did: the same pixels, give or take the rounding of an 8-bit
  * blend.
  */
class SkiaGlyphInkSpec extends AnyFlatSpec with Matchers:

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val awtFont = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val caches  = SkiaTextCaches.fresh()
  private val text    = "Skia draws every glyph: fi fl AV, 0123456789"
  private val width   = 330
  private val height  = 40

  private def drawnBySurface(ink: Color, background: Color): Array[Int] =
    val surface = Surface.Companion.makeRasterN32Premul(width, height)
    val render =
      SkiaRenderSurface.retaining(
        surface,
        metrics,
        FontSpec.fromAwt(awtFont),
        width,
        height,
        1.0,
        caches,
        StyledFontCache()
      )
    render.setBackgroundColor(RenderColor.fromAwt(background))
    render.setForegroundColor(RenderColor.fromAwt(ink))
    render.clearViewport(RenderColor.fromAwt(background))
    render.text.drawRunPx(5f, 10, 320f, metrics.lineHeight, metrics.ascent, text)
    render.flush()
    SkiaPixels.read(surface)

  private def drawnWithPlainPaint(ink: Color, background: Color): Array[Int] =
    val surface = Surface.Companion.makeRasterN32Premul(width, height)
    surface.getCanvas.clear(background.getRGB)
    val paint = Paint()
    paint.setAntiAlias(true)
    paint.setColor(ink.getRGB)
    val _ = surface.getCanvas.drawString(text, 5f, 20f, caches.fonts.faceFor(awtFont).font, paint)
    SkiaPixels.read(surface)

  private def channels(argb: Int): List[Int] = List(argb >>> 16 & 0xff, argb >>> 8 & 0xff, argb & 0xff)

  for (label, ink, background) <- List(
        ("light on dark", new Color(221, 221, 221), new Color(20, 24, 40)),
        ("dark on light", new Color(30, 30, 30), new Color(250, 248, 240)),
        ("coloured", new Color(200, 80, 40), new Color(10, 60, 90)),
        ("translucent", new Color(255, 255, 255, 128), new Color(40, 40, 120))
      )
  do
    s"Text drawn in $label" should "match a plain-colour draw to within the rounding of the blend" in {
      val shader = drawnBySurface(ink, background)
      val plain  = drawnWithPlainPaint(ink, background)
      val worst  = shader.zip(plain).map((a, b) => channels(a).zip(channels(b)).map((x, y) => math.abs(x - y)).max).max
      worst should be <= 3
    }
