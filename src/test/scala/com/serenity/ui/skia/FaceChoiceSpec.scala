package com.serenity.ui.skia

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FaceChoiceSpec extends AnyFlatSpec with Matchers:

  private def face(name: String, weight: Int, italic: Boolean = false) =
    FaceChoice.Candidate(name, weight, italic)

  private val inter = Vector(
    face("Black", 900),
    face("Bold", 700),
    face("ExtraLight", 200),
    face("Italic", 400, italic = true),
    face("BoldItalic", 700, italic = true),
    face("Light", 300),
    face("Medium", 500),
    face("Regular", 400),
    face("SemiBold", 600),
    face("Thin", 100)
  )

  "FaceChoice.choose" should "pick the regular weight for plain text, whatever order the family lists its faces in" in {
    FaceChoice.choose(inter, bold = false, italic = false) shouldBe Some("Regular")
    FaceChoice.choose(inter.reverse, bold = false, italic = false) shouldBe Some("Regular")
  }

  it should "pick the bold weight for bold text" in {
    FaceChoice.choose(inter, bold = true, italic = false) shouldBe Some("Bold")
  }

  it should "keep to the requested slant before looking at weights" in {
    FaceChoice.choose(inter, bold = false, italic = true) shouldBe Some("Italic")
    FaceChoice.choose(inter, bold = true, italic = true) shouldBe Some("BoldItalic")
  }

  it should "prefer a heavier face to a lighter one when the weight asked for is missing, above regular" in {
    val noBold = Vector(face("Light", 300), face("Medium", 500), face("Black", 900), face("SemiBold", 600))
    FaceChoice.choose(noBold, bold = true, italic = false) shouldBe Some("Black")
  }

  it should "look between regular and medium first for regular text, then lighter, then heavier" in {
    FaceChoice.choose(Vector(face("Thin", 100), face("Medium", 500), face("Black", 900)), false, false) shouldBe Some(
      "Medium"
    )
    FaceChoice.choose(Vector(face("Thin", 100), face("Light", 300), face("Black", 900)), false, false) shouldBe Some(
      "Light"
    )
    FaceChoice.choose(Vector(face("SemiBold", 600), face("Black", 900)), false, false) shouldBe Some("SemiBold")
  }

  it should "take the first of two faces of one weight" in {
    FaceChoice.choose(Vector(face("first", 400), face("second", 400)), false, false) shouldBe Some("first")
  }

  it should "fall back to the other slant when the family has none of the one asked for" in {
    FaceChoice.choose(Vector(face("Regular", 400), face("Bold", 700)), bold = true, italic = true) shouldBe Some("Bold")
  }

  it should "choose nothing from no candidates" in {
    FaceChoice.choose(Vector.empty[FaceChoice.Candidate[String]], bold = false, italic = false) shouldBe None
  }
