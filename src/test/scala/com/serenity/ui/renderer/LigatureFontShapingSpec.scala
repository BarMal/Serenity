package com.serenity.ui.renderer

import java.awt.image.BufferedImage

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** A line of the bundled ligature font is shaped once for both halves of a frame: laying it out puts its glyphs in the
  * shaped-text cache, and painting the laid-out row takes them from there instead of shaping the line again.
  */
class LigatureFontShapingSpec extends AnyFlatSpec with Matchers:

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val font        = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
  private val metrics     = CellMetrics.fromFont(font)
  private val widthPx     = 900
  private val heightPx    = 40
  private val deviceScale = 2.0

  private def surfaceOver(image: BufferedImage, caches: RenderCaches): Java2DRenderSurface =
    new Java2DRenderSurface(
      image,
      metrics,
      font,
      _ => (),
      widthPx,
      heightPx,
      deviceScale,
      deviceScale,
      textCaches = caches.surfaceText
    )

  "Laying out a line of the ligature code font" should "leave it shaped for the painter, under the key it paints with" in {
    val line    = "val answer = items.map(_ + 1).filter(_ >= 2) // a => b != c, distinct to this spec"
    val caches  = RenderCaches.create()
    val surface = surfaceOver(new BufferedImage(widthPx * 2, heightPx * 2, BufferedImage.TYPE_INT_ARGB), caches)
    val frc     = surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    caches.shapedText.holds(line, font, frc) shouldBe false

    val rows =
      TextLayoutSnapshot.boundedVisualLinesForText(line, 0, widthPx, font, frc, wrapCache = caches.wrappedLines)

    rows.map(_.text) shouldBe Vector(line)
    caches.shapedText.holds(line, font, frc) shouldBe true
  }
