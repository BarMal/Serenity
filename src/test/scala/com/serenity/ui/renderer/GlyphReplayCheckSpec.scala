package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.layout.TextLayoutSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A glyph vector rebuilt from glyph codes is only as good as the runtime's reading of those codes: on JetBrains
  * Runtime 25 the codes of a composite font come back as other glyphs. So a font is replayed only once its replay has
  * been seen to draw what `drawString` draws.
  */
class GlyphReplayCheckSpec extends AnyFlatSpec with Matchers:

  private val frc   = TextLayoutSnapshot.defaultFontRenderContext()
  private val serif = new Font(Font.SERIF, Font.PLAIN, 14)
  private val mono  = new Font(Font.MONOSPACED, Font.PLAIN, 14)

  private def countingCheck(verdict: (Font, FontRenderContext) => Boolean): (GlyphReplayCheck, AtomicInteger) =
    val asked = new AtomicInteger(0)
    val check = GlyphReplayCheck { (font, context) =>
      val _ = asked.incrementAndGet()
      verdict(font, context)
    }
    (check, asked)

  "A font whose replay matched drawString" should "be trusted" in
    GlyphReplayCheck((_, _) => true).faithful(serif, frc) shouldBe true

  "A font whose replay differed from drawString" should "not be trusted" in
    GlyphReplayCheck((_, _) => false).faithful(serif, frc) shouldBe false

  "The check" should "judge a font and render context once, however often it is asked" in {
    val (check, asked) = countingCheck((_, _) => false)

    val verdicts = List.fill(5)(check.faithful(serif, frc))

    verdicts shouldBe List.fill(5)(false)
    asked.get() shouldBe 1
  }

  it should "judge another font, or the same font at another scale, on its own" in {
    val (check, asked) = countingCheck((font, _) => font == mono)

    val scaled = new FontRenderContext(
      AffineTransform.getScaleInstance(2.0, 2.0),
      frc.getAntiAliasingHint,
      frc.getFractionalMetricsHint
    )

    check.faithful(serif, frc) shouldBe false
    check.faithful(mono, frc) shouldBe true
    check.faithful(mono, scaled) shouldBe true
    check.faithful(mono, scaled) shouldBe true

    asked.get() shouldBe 3
  }

  it should "give a stable verdict when it renders the comparison itself" in {
    val check = GlyphReplayCheck()

    List(serif, mono).foreach(font => check.faithful(font, frc) shouldBe check.faithful(font, frc))
  }

  "GlyphReplayCheck.sameRaster" should "accept images that hold the same pixels" in {
    val a = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
    val b = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
    a.setRGB(1, 1, 0xff112233)
    b.setRGB(1, 1, 0xff112233)

    GlyphReplayCheck.sameRaster(a, b) shouldBe true
  }

  it should "reject images that differ in one pixel" in {
    val a = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
    val b = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
    b.setRGB(3, 2, 0xff000001)

    GlyphReplayCheck.sameRaster(a, b) shouldBe false
  }

  it should "reject images of different sizes" in
    GlyphReplayCheck.sameRaster(
      new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB),
      new BufferedImage(3, 4, BufferedImage.TYPE_INT_ARGB)
    ) shouldBe false
