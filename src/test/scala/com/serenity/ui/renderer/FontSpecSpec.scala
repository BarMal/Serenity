package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.TextAttribute

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class FontSpecSpec extends AnyFlatSpec with Matchers:

  private def attributed(font: Font, attribute: TextAttribute, value: AnyRef): Font =
    font.deriveFont(java.util.Map.of(attribute, value))

  private def bundled(resource: String): Font =
    val stream = getClass.getResourceAsStream(resource)
    try Font.createFont(Font.TRUETYPE_FONT, stream).deriveFont(14f)
    finally stream.close()

  "FontSpec.fromAwt" should "read the family, size and style of a plain font" in {
    val spec = FontSpec.fromAwt(new Font(Font.SERIF, Font.PLAIN, 14))
    spec.family shouldBe new Font(Font.SERIF, Font.PLAIN, 14).getFamily
    spec.sizePt shouldBe 14.0f
    spec.isBold shouldBe false
    spec.isItalic shouldBe false
    spec.ligatures shouldBe false
    spec.strikethrough shouldBe false
  }

  it should "read bold and italic from the style" in {
    val spec = FontSpec.fromAwt(new Font(Font.SERIF, Font.BOLD | Font.ITALIC, 12))
    spec.isBold shouldBe true
    spec.isItalic shouldBe true
  }

  it should "read the ligature and strikethrough attributes" in {
    val base      = new Font(Font.SERIF, Font.PLAIN, 14)
    val ligatured = attributed(base, TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
    val struck    = attributed(base, TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON)
    FontSpec.fromAwt(ligatured).ligatures shouldBe true
    FontSpec.fromAwt(ligatured).strikethrough shouldBe false
    FontSpec.fromAwt(struck).strikethrough shouldBe true
    FontSpec.fromAwt(struck).ligatures shouldBe false
  }

  it should "keep a fractional size" in {
    FontSpec.fromAwt(new Font(Font.SERIF, Font.PLAIN, 14).deriveFont(13.5f)).sizePt shouldBe 13.5f
  }

  "A FontSpec" should "be equal to another with the same data whichever AWT font it was read from" in {
    val first  = FontSpec.fromAwt(new Font(Font.SERIF, Font.PLAIN, 14))
    val second = FontSpec.fromAwt(new Font(Font.SERIF, Font.PLAIN, 14).deriveFont(14.0f))
    first shouldBe second
    first.hashCode shouldBe second.hashCode
    val fromData = FontSpec(first.family, first.faceName, first.sizePt, false, false, false, false)()
    fromData shouldBe first
  }

  it should "differ when the face, size, style or an attribute differs" in {
    val base = FontSpec.fromAwt(new Font(Font.SERIF, Font.PLAIN, 14))
    base should not be FontSpec.fromAwt(new Font(Font.SERIF, Font.PLAIN, 15))
    base should not be FontSpec.fromAwt(new Font(Font.SERIF, Font.BOLD, 14))
    base should not be FontSpec.fromAwt(new Font(Font.SERIF, Font.ITALIC, 14))
    base should not be FontSpec.fromAwt(new Font(Font.MONOSPACED, Font.PLAIN, 14))
  }

  it should "tell two faces of one family apart, though AWT calls both of a bundled pair plain" in {
    val regular = FontSpec.fromAwt(bundled("/fonts/CourierPrime-Regular.ttf"))
    val bold    = FontSpec.fromAwt(bundled("/fonts/CourierPrime-Bold.ttf"))
    regular.family shouldBe bold.family
    regular should not be bold
  }

  "FontSpec.toAwt" should "hand back the AWT font the spec was read from" in {
    val font = new Font(Font.SERIF, Font.BOLD, 14)
    FontSpec.fromAwt(font).toAwt should be theSameInstanceAs font
  }

  it should "build an equivalent AWT font for a spec made from data" in {
    val source = attributed(
      attributed(
        new Font(Font.SERIF, Font.BOLD | Font.ITALIC, 14).deriveFont(13.5f),
        TextAttribute.LIGATURES,
        TextAttribute.LIGATURES_ON
      ),
      TextAttribute.STRIKETHROUGH,
      TextAttribute.STRIKETHROUGH_ON
    )
    val read = FontSpec.fromAwt(source)
    val built = FontSpec(
      read.family,
      read.faceName,
      read.sizePt,
      read.isBold,
      read.isItalic,
      read.ligatures,
      read.strikethrough
    )()
    val awt = built.toAwt
    awt.getFamily shouldBe source.getFamily
    awt.getSize2D shouldBe 13.5f
    awt.isBold shouldBe true
    awt.isItalic shouldBe true
    awt.getAttributes.get(TextAttribute.LIGATURES) shouldBe TextAttribute.LIGATURES_ON
    awt.getAttributes.get(TextAttribute.STRIKETHROUGH) shouldBe TextAttribute.STRIKETHROUGH_ON
    FontSpec.fromAwt(awt) shouldBe read
  }
