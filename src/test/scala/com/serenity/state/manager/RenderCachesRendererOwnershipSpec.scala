package com.serenity.state.manager

import java.awt.image.BufferedImage

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import com.serenity.ui.renderer.Java2DRenderSurface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** The renderer's text caches belong to the [[RenderCaches]] that renders, not to the JVM: laying a line out through
  * one owner leaves it shaped for that owner's painter and for nobody else's.
  */
class RenderCachesRendererOwnershipSpec extends AnyFlatSpec with Matchers:

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val font     = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
  private val metrics  = CellMetrics.fromFont(font)
  private val widthPx  = 900
  private val heightPx = 40
  private val line     = "val owned = items.map(_ + 1).filter(_ >= 2) // a => b != c, distinct to the ownership spec"

  private def surfaceOver(caches: RenderCaches): Java2DRenderSurface =
    new Java2DRenderSurface(
      new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_ARGB),
      metrics,
      font,
      _ => (),
      widthPx,
      heightPx,
      1.0,
      1.0,
      textCaches = caches.surfaceText
    )

  "Two RenderCaches" should "own distinct renderer text caches" in {
    val (a, b) = (RenderCaches.create(), RenderCaches.create())

    (a.shapedText `ne` b.shapedText) shouldBe true
    (a.styledFonts `ne` b.styledFonts) shouldBe true
    (a.labelSprites `ne` b.labelSprites) shouldBe true
    (a.measuredAdvances `ne` b.measuredAdvances) shouldBe true
  }

  "Laying a ligature line out through a RenderCaches" should "shape it in that owner's cache and no other's" in {
    val (owner, other) = (RenderCaches.create(), RenderCaches.create())
    val frc            = surfaceOver(owner).fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())

    val rows = TextLayoutSnapshot.boundedVisualLinesForText(line, 0, widthPx, font, frc, wrapCache = owner.wrappedLines)

    rows.map(_.text) shouldBe Vector(line)
    owner.shapedText.holds(line, font, frc) shouldBe true
    other.shapedText.holds(line, font, frc) shouldBe false
  }

  it should "leave nothing to shape when its painter, built over the same owner, draws the row" in {
    val owner   = RenderCaches.create()
    val surface = surfaceOver(owner)
    val frc     = surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    val _ = TextLayoutSnapshot.boundedVisualLinesForText(line, 0, widthPx, font, frc, wrapCache = owner.wrappedLines)
    val shaped = owner.shapedText.shapings

    surface.drawShapedRunPx(0.0f, 0, widthPx.toFloat, metrics.lineHeight, metrics.ascent, line, 0, line.length)

    owner.shapedText.shapings shouldBe shaped
  }

  "A RenderCaches' measured advances" should "shape through the owner's cache, not a shared one" in {
    val (owner, other) = (RenderCaches.create(), RenderCaches.create())
    val frc            = TextLayoutSnapshot.defaultFontRenderContext()

    val advance = owner.measuredAdvances.advanceOf(line, font, frc)

    advance should be > 0.0f
    owner.measuredAdvances.measurements shouldBe 1L
    other.measuredAdvances.measurements shouldBe 0L
    owner.shapedText.holds(line, font, frc) shouldBe true
    other.shapedText.holds(line, font, frc) shouldBe false
  }

  "A layer surface of an owned surface" should "paint through the owner's cache, not one of its own" in {
    val owner = RenderCaches.create()
    val layer = surfaceOver(owner).newLayerSurface(_ => (), None)
    val frc   = layer.text.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())

    layer.text.drawShapedRunPx(0.0f, 0, widthPx.toFloat, metrics.lineHeight, metrics.ascent, line, 0, line.length)

    owner.shapedText.holds(line, font, frc) shouldBe true
  }

  "A RenderCaches' surface text caches" should "be the owner's own instances" in {
    val owner = RenderCaches.create()

    (owner.surfaceText.shapedText eq owner.shapedText) shouldBe true
    (owner.surfaceText.styledFonts eq owner.styledFonts) shouldBe true
    (owner.surfaceText.labelSprites eq owner.labelSprites) shouldBe true
  }
