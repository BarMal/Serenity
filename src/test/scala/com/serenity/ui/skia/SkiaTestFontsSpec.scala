package com.serenity.ui.skia

import java.awt.Font
import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaTestFontsSpec extends AnyFlatSpec with Matchers:

  private val resource = "/fonts/SerenityFallbackTest.ttf"

  private val familyInTheFile: String =
    Font.createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(resource).toURI).toFile).getFamily

  private def typefaceOf(family: String): String =
    SkiaTypefaces.typefaceFor(SkiaTypefaces.TypefaceKey(family, bold = false, italic = false, 12.0f, 7.0)).getFamilyName

  "A font file the system does not hold" should "be what Skia answers for its family once it is registered" in {
    SkiaTestFonts.register(resource) shouldBe familyInTheFile
    typefaceOf(familyInTheFile) shouldBe familyInTheFile
  }

  it should "stay registered when it is registered a second time" in {
    SkiaTestFonts.register(resource) shouldBe familyInTheFile
    typefaceOf(familyInTheFile) shouldBe familyInTheFile
  }

  it should "not be what Skia answers for a family that was never registered" in {
    val _ = SkiaTestFonts.register(resource)
    typefaceOf("A Family Nobody Registered") should not equal familyInTheFile
  }
