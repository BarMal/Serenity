package com.serenity

import java.awt.Font

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

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

  it should "expose UI font family choices" in {
    val families = FontLoader.availableUiFamilies

    families should not be empty
    families should contain(Font.SANS_SERIF)
  }

  it should "load the configured code font family" in {
    val config = FontConfig(codeFontFamily = FontLoader.BundledCodeFontFamily, fontSize = 12.0f)
    val font   = FontLoader.loadCodeFont(config).unsafeRunSync()

    font.getName should not be empty
    font.getSize2D shouldBe 12.0f
  }

  it should "preview the bundled code font with the same family used at runtime" in {
    val config      = FontConfig(codeFontFamily = FontLoader.BundledCodeFontFamily, fontSize = 12.0f)
    val runtimeFont = FontLoader.loadCodeFont(config).unsafeRunSync()
    val previewFont = FontLoader.previewCodeFont(config)

    previewFont.getFontName shouldBe runtimeFont.getFontName
  }

  it should "load the configured text font family" in {
    val config = FontConfig(textFontFamily = Font.SANS_SERIF, fontSize = 12.0f)
    val font   = FontLoader.loadTextFont(config).unsafeRunSync()

    font.getName should not be empty
    font.getSize2D shouldBe 12.0f
  }

  it should "load the UI font from its own family rather than code or text font families" in {
    val config = FontConfig(
      codeFontFamily = Font.MONOSPACED,
      textFontFamily = Font.SERIF,
      uiFontFamily = Font.SANS_SERIF,
      fontSize = 17.0f,
      uiFontSize = 13.0f
    )

    val font = FontLoader.loadUiFont(config).unsafeRunSync()

    font.getFamily shouldBe Font.SANS_SERIF
    font.getSize2D shouldBe 13.0f
  }

  it should "keep the UI font stable when only the code font family changes" in {
    val before = FontLoader.previewUiFont(
      FontConfig(codeFontFamily = Font.MONOSPACED, textFontFamily = Font.SERIF, uiFontFamily = Font.SANS_SERIF)
    )
    val after = FontLoader.previewUiFont(
      FontConfig(codeFontFamily = Font.DIALOG_INPUT, textFontFamily = Font.SERIF, uiFontFamily = Font.SANS_SERIF)
    )

    after.getFamily shouldBe before.getFamily
    after.getSize2D shouldBe before.getSize2D
  }

  it should "apply text ligature attributes when enabled" in {
    val font = FontLoader
      .loadTextFont(FontConfig(textFontFamily = Font.SANS_SERIF, textLigatures = true))
      .unsafeRunSync()

    Option(font.getAttributes.get(java.awt.font.TextAttribute.LIGATURES)) shouldBe
      Some(java.awt.font.TextAttribute.LIGATURES_ON)
  }

  it should "apply UI ligature attributes independently when enabled" in {
    val font = FontLoader
      .loadUiFont(FontConfig(uiFontFamily = Font.SANS_SERIF, uiLigatures = true, textLigatures = false))
      .unsafeRunSync()

    Option(font.getAttributes.get(java.awt.font.TextAttribute.LIGATURES)) shouldBe
      Some(java.awt.font.TextAttribute.LIGATURES_ON)
  }

  it should "only include monospaced families that can render basic ASCII" in {
    val testString = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    FontLoader.availableMonospaceFamilies.foreach { family =>
      if family != FontLoader.BundledCodeFontFamily then
        val font = Font(family, Font.PLAIN, 12)
        withClue(s"Font family '$family' cannot render basic ASCII: ") {
          font.canDisplayUpTo(testString) shouldBe -1
        }
    }
  }

  it should "only include monospaced families that produce valid CellMetrics" in
    FontLoader.availableMonospaceFamilies.foreach { family =>
      if family != FontLoader.BundledCodeFontFamily then
        val font    = Font(family, Font.PLAIN, 12).deriveFont(12.0f)
        val metrics = CellMetrics.fromFont(font)
        withClue(s"Font family '$family' produces invalid metrics: ") {
          metrics.isValid shouldBe true
        }
    }

  it should "only include text families that can render basic ASCII" in {
    val testString = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    FontLoader.availableTextFamilies.foreach { family =>
      val font = Font(family, Font.PLAIN, 12)
      withClue(s"Font family '$family' cannot render basic ASCII: ") {
        font.canDisplayUpTo(testString) shouldBe -1
      }
    }
  }

  it should "only include text families that produce valid CellMetrics" in
    FontLoader.availableTextFamilies.foreach { family =>
      val font    = Font(family, Font.PLAIN, 12).deriveFont(12.0f)
      val metrics = CellMetrics.fromFont(font)
      withClue(s"Font family '$family' produces invalid metrics: ") {
        metrics.isValid shouldBe true
      }
    }
