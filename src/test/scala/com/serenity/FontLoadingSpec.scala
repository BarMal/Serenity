package com.serenity

import java.awt.Font

import cats.effect.IO
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.effect.unsafe.implicits.global

class FontLoadingSpec extends AnyFlatSpec with Matchers:

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "FontLoader" should "expose only monospaced families in the code-font choices" in {
    val families = FontLoader.availableMonospaceFamilies

    families should not be empty
    families should contain(FontLoader.BundledCodeFontFamily)
    families.filterNot(_ == FontLoader.BundledCodeFontFamily).forall(FontLoader.isMonospacedFamily) shouldBe true
  }

  it should "expose only non-monospaced families in the text-font choices" in {
    val families = FontLoader.availableTextFamilies

    families should not be empty
    families.forall(family => !FontLoader.isMonospacedFamily(family)) shouldBe true
  }

  it should "load the configured code font family" in {
    val config = FontConfig(codeFontFamily = FontLoader.BundledCodeFontFamily, fontSize = 12.0f)
    val font   = FontLoader.loadCodeFont(config).unsafeRunSync()

    font.getName should not be empty
    font.getSize2D shouldBe 12.0f
  }

  it should "load the configured text font family" in {
    val config = FontConfig(textFontFamily = Font.SANS_SERIF, fontSize = 12.0f)
    val font   = FontLoader.loadTextFont(config).unsafeRunSync()

    font.getName should not be empty
    font.getSize2D shouldBe 12.0f
  }

  it should "apply ligature attributes when enabled" in {
    val font = FontLoader
      .loadTextFont(FontConfig(textFontFamily = Font.SANS_SERIF, enableLigatures = true))
      .unsafeRunSync()

    Option(font.getAttributes.get(java.awt.font.TextAttribute.LIGATURES)) shouldBe
      Some(java.awt.font.TextAttribute.LIGATURES_ON)
  }
