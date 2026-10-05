package com.serenity.ui.color

import java.awt.Color
import java.awt.image.BufferedImage

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.Java2DRenderSurface
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Pins the neutral colour type (#1812 step 3): a packed ARGB value that round-trips through the `java.awt.Color` the
  * AWT boundaries still use without losing any channel -- alpha included, since a zero-alpha background is the "show
  * the backdrop through" sentinel (#1240) -- whose arithmetic matches the java.awt code the theme used before it moved
  * onto this type, and that the Java2D surface paints exactly the colour it is handed.
  */
class RenderColorSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  private val anyArgb = Gen.choose(Int.MinValue, Int.MaxValue)

  "RenderColor" should "round-trip every ARGB value through java.awt.Color unchanged" in
    forAll(anyArgb) { argb =>
      val awt = RenderColor.fromArgb(argb).toAwt
      awt.getRGB shouldBe argb
      RenderColor.fromAwt(awt).argb shouldBe argb
    }

  it should "keep every channel of an AWT colour, alpha included" in
    forAll(Gen.choose(0, 255), Gen.choose(0, 255), Gen.choose(0, 255), Gen.choose(0, 255)) { (r, g, b, a) =>
      val color = RenderColor.fromAwt(new Color(r, g, b, a))
      (color.red, color.green, color.blue, color.alpha) shouldBe (r, g, b, a)
      color.toAwt shouldBe new Color(r, g, b, a)
    }

  it should "unpack 0xAARRGGBB into its channels" in {
    val color = RenderColor.fromArgb(0x80102030)
    (color.alpha, color.red, color.green, color.blue) shouldBe (0x80, 0x10, 0x20, 0x30)
  }

  it should "treat two colours with the same ARGB value as equal" in {
    RenderColor.fromAwt(Color.RED) shouldBe RenderColor.fromArgb(0xffff0000)
    RenderColor.fromAwt(new Color(0, 0, 0, 0)) should not be RenderColor.fromAwt(Color.BLACK)
  }

  it should "build from channels exactly as the java.awt.Color constructor the theme parsers used" in
    forAll(Gen.choose(0, 255), Gen.choose(0, 255), Gen.choose(0, 255), Gen.choose(0, 255)) { (r, g, b, a) =>
      RenderColor.fromRgba(r, g, b, a).argb shouldBe new Color(r, g, b, a).getRGB
      RenderColor.fromRgba(r, g, b).argb shouldBe new Color(r, g, b).getRGB
    }

  it should "name opaque black and white as java.awt does" in {
    RenderColor.Black.argb shouldBe Color.BLACK.getRGB
    RenderColor.White.argb shouldBe Color.WHITE.getRGB
  }

  it should "replace only the alpha channel, as the java.awt withAlpha overlays used" in
    forAll(anyArgb, Gen.choose(0, 255)) { (argb, alpha) =>
      val awt = new Color(argb, true)
      RenderColor.fromArgb(argb).withAlpha(alpha).argb shouldBe
        new Color(awt.getRed, awt.getGreen, awt.getBlue, alpha).getRGB
    }

  it should "blend toward another colour exactly as the java.awt theme blend did, keeping its own alpha" in
    forAll(anyArgb, anyArgb, Gen.choose(-0.5, 1.5)) { (from, to, factor) =>
      RenderColor.fromArgb(from).blendToward(RenderColor.fromArgb(to), factor).argb shouldBe
        awtThemeBlend(new Color(from, true), new Color(to, true), factor).getRGB
    }

  it should "mix opaquely with a background exactly as the java.awt highlight blend did" in
    forAll(anyArgb, anyArgb, Gen.choose(-0.5, 1.5)) { (foreground, background, weight) =>
      RenderColor.fromArgb(foreground).mixOver(RenderColor.fromArgb(background), weight).argb shouldBe
        awtHighlightBlend(new Color(foreground, true), new Color(background, true), weight).getRGB
    }

  it should "only compare equal-typed colours, so a java.awt.Color or bare Int slip is a compile error" in {
    RenderColor.fromArgb(0xff336699) == RenderColor.fromArgb(0xff336699) shouldBe true
    assertTypeError("RenderColor.fromArgb(0) == java.awt.Color.BLACK")
    assertTypeError("RenderColor.fromArgb(0) == 0")
  }

  /** The blend `Theme.blend` computed on `java.awt.Color` before the theme moved onto [[RenderColor]], kept verbatim as
    * the oracle hover/pressed/disabled derivation must still match.
    */
  private def awtThemeBlend(from: Color, to: Color, factor: Double): Color =
    val t = factor.max(0.0).min(1.0)
    def component(start: Int, end: Int): Int =
      math.round(start + (end - start) * t).toInt.max(0).min(255)
    new Color(
      component(from.getRed, to.getRed),
      component(from.getGreen, to.getGreen),
      component(from.getBlue, to.getBlue),
      from.getAlpha
    )

  /** `RendererHighlights.blend` on `java.awt.Color`, verbatim: a weighted average that always comes out opaque. */
  private def awtHighlightBlend(foreground: Color, background: Color, warningWeight: Double): Color =
    val clampedWeight    = math.max(0.0, math.min(1.0, warningWeight))
    val backgroundWeight = 1.0 - clampedWeight
    def blendChannel(channel: Color => Int): Int =
      math.round(channel(foreground) * clampedWeight + channel(background) * backgroundWeight).toInt
    Color(blendChannel(_.getRed), blendChannel(_.getGreen), blendChannel(_.getBlue))

  "Java2DRenderSurface" should "paint exactly the RenderColor it is handed and report it back unchanged" in {
    val image   = new BufferedImage(20, 20, BufferedImage.TYPE_INT_ARGB)
    val font    = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
    val surface = new Java2DRenderSurface(image, CellMetrics.fromFont(font), font, _ => ())
    val teal    = RenderColor.fromArgb(0xff336699)

    surface.setBackgroundColor(teal)
    surface.fillPixelRect(0, 0, 4, 4, teal)

    surface.getBackgroundColor shouldBe teal
    image.getRGB(1, 1) shouldBe 0xff336699
  }
