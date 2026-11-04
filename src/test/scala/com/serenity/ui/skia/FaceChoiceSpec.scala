package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FaceChoiceSpec extends AnyFlatSpec with Matchers:

  private def face(name: String, bold: Boolean, italic: Boolean, advance: Double) =
    FaceChoice.Candidate(name, bold, italic, () => advance)

  private val inter = Vector(
    face("Black", bold = true, italic = false, 218.11),
    face("Bold", bold = true, italic = false, 212.39),
    face("ExtraLight", bold = false, italic = false, 198.52),
    face("Italic", bold = false, italic = true, 206.89),
    face("Light", bold = false, italic = false, 202.11),
    face("Medium", bold = false, italic = false, 207.96),
    face("Regular", bold = false, italic = false, 205.71),
    face("Thin", bold = false, italic = false, 194.92)
  )

  "FaceChoice.choose" should "pick the weight whose advance is the one layout measured with" in {
    FaceChoice.choose(inter, bold = false, italic = false, Some(205.71)) shouldBe Some("Regular")
  }

  it should "pick the bold face nearest the reference among bold candidates only" in {
    FaceChoice.choose(inter, bold = true, italic = false, Some(212.0)) shouldBe Some("Bold")
  }

  it should "keep to the requested style before looking at advances" in {
    FaceChoice.choose(inter, bold = false, italic = true, Some(194.92)) shouldBe Some("Italic")
  }

  it should "take the first face of the best style when there is no reference" in {
    FaceChoice.choose(inter, bold = false, italic = false, None) shouldBe Some("ExtraLight")
  }

  it should "fall back to the closest style when none matches exactly" in {
    val onlyRegular = Vector(face("Regular", bold = false, italic = false, 205.71))
    FaceChoice.choose(onlyRegular, bold = true, italic = true, Some(300.0)) shouldBe Some("Regular")
  }

  it should "choose nothing from no candidates" in {
    FaceChoice.choose(Vector.empty[FaceChoice.Candidate[String]], bold = false, italic = false, Some(1.0)) shouldBe None
  }

  it should "not measure faces of a worse style" in {
    val measured = new AtomicReference(List.empty[String])
    def measuring(name: String, bold: Boolean) =
      FaceChoice.Candidate(
        name,
        bold = bold,
        italic = false,
        () =>
          measured.updateAndGet(name :: _)
          1.0
      )
    val _ = FaceChoice.choose(Vector(measuring("plain", false), measuring("bold", true)), false, false, Some(1.0))
    measured.get shouldBe List("plain")
  }
