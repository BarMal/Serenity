package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.TextAttribute
import java.awt.image.BufferedImage

import scala.jdk.CollectionConverters.*

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pins the render seam's font type (#1812 step 3): wrapping a loaded Java2D font keeps the very same instance --
  * derived attributes such as ligatures included -- reports its family/size/style without unwrapping, and is exactly
  * the font the Java2D surface then draws with.
  */
class FontSpecSpec extends AnyFlatSpec with Matchers:

  private val base = new Font(Font.MONOSPACED, Font.PLAIN, 12).deriveFont(13.5f)

  "FontSpec" should "wrap and unwrap the very same java.awt.Font, attributes included" in {
    val ligatures = base.deriveFont(Map(TextAttribute.LIGATURES -> TextAttribute.LIGATURES_ON).asJava)

    FontSpec.fromAwt(ligatures).toAwt should be theSameInstanceAs ligatures
  }

  it should "report the font's family, fractional point size and style" in {
    val boldItalic = FontSpec.fromAwt(base.deriveFont(Font.BOLD | Font.ITALIC))

    (boldItalic.family, boldItalic.sizePt, boldItalic.isBold, boldItalic.isItalic) shouldBe
      (base.getFamily, 13.5f, true, true)
    (FontSpec.fromAwt(base).isBold, FontSpec.fromAwt(base).isItalic) shouldBe (false, false)
  }

  "Java2DRenderSurface.setFont" should "draw with exactly the font behind the spec" in {
    val serif = new Font(Font.SERIF, Font.BOLD, 20)

    val viaSpec   = glyphPixels(base)(surface => surface.setFont(FontSpec.fromAwt(serif)))
    val direct    = glyphPixels(serif)(_ => ())
    val unchanged = glyphPixels(base)(_ => ())

    viaSpec shouldBe direct
    viaSpec should not be unchanged
  }

  private def glyphPixels(font: Font)(prepare: Java2DRenderSurface => Unit): Vector[Int] =
    val image = new BufferedImage(60, 40, BufferedImage.TYPE_INT_ARGB)
    val surface =
      new Java2DRenderSurface(image, CellMetrics(charWidth = 30, lineHeight = 40, ascent = 30), font, _ => ())
    prepare(surface)
    surface.setForegroundColor(RenderColor.fromArgb(0xffffffff))
    surface.setBackgroundColor(RenderColor.fromArgb(0xff000000))
    surface.putString(0, 0, "Wg")
    image.getRGB(0, 0, 60, 40, null, 0, 60).toVector
