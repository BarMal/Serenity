package com.serenity.ui.skia

import com.serenity.ui.fonts.FontLoader
import org.jetbrains.skia.Font
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class FallbackAgreementSpec extends AnyFlatSpec with Matchers:

  private def fonts: Vector[(String, Font)] =
    val config = FontLoader.FontConfig()
    Vector(
      "text" -> SkiaTypefaces.fontFor(FontLoader.previewTextFont(config)),
      "code" -> SkiaTypefaces.fontFor(FontLoader.previewCodeFont(config))
    )

  "The fallback agreement check" should "find one string per allowlisted code point and font, all identical to TextLine" in {
    val result = FallbackAgreement.check(fonts)

    result.checked shouldBe fonts.size * FallbackRuns.codePoints.size
    result.identical shouldBe result.checked
    result.differing shouldBe empty
  }

  it should "name the strings that differ when the routed result is not TextLine's" in {
    val wrong = FallbackFonts.matching((base, _) => Some(base))

    val result = FallbackAgreement.check(fonts, wrong)

    result.identical should be < result.checked
    result.differing should not be empty
    result.differing.head should include("U+")
  }

  it should "count a string TextLine still shapes as identical, since it is TextLine's own result" in {
    val result = FallbackAgreement.check(fonts, FallbackFonts.unavailable)

    result.identical shouldBe result.checked
  }
