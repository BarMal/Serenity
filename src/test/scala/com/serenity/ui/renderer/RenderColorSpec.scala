package com.serenity.ui.renderer

import java.awt.Color
import java.awt.image.BufferedImage

import com.serenity.ui.layout.CellMetrics
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Pins the render seam's own colour type (#1812 step 3): a packed ARGB value that round-trips through the
  * `java.awt.Color` the theme still holds without losing any channel -- alpha included, since a zero-alpha background
  * is the "show the backdrop through" sentinel (#1240) -- and that the Java2D surface paints exactly the colour it is
  * handed.
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
