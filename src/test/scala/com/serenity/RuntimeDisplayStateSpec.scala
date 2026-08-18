package com.serenity

import java.awt.Font

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.RuntimeDisplayState
import com.serenity.rope.Balance
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class RuntimeDisplayStateSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given org.typelevel.log4cats.Logger[IO] =
    LoggerFactory[IO].getLogger(using LoggerName("RuntimeDisplayStateSpec"))

  "RuntimeDisplayState" should "derive cell metrics from the current runtime font" in {
    val runtime = RuntimeDisplayState.create(FontConfig(fontSize = 12.0f, uiFontSize = 14.0f)).unsafeRunSync()

    runtime.codeMetrics shouldBe CellMetrics.fromFont(runtime.codeFont)
    runtime.textMetrics shouldBe CellMetrics.fromFont(runtime.textFont)
    runtime.uiMetrics shouldBe CellMetrics.fromFont(runtime.uiFont)
  }

  it should "refresh both runtime fonts and metrics when the font config changes" in {
    val runtime          = RuntimeDisplayState.create(FontConfig(fontSize = 12.0f, uiFontSize = 14.0f)).unsafeRunSync()
    val originalCodeFont = runtime.codeFont
    val originalCodeMetric = runtime.codeMetrics

    runtime
      .update(
        FontConfig(
          codeFontFamily = "Monospaced",
          textFontFamily = "SansSerif",
          fontSize = 18.0f,
          textFontSize = 18.0f,
          uiFontSize = 16.0f
        )
      )
      .unsafeRunSync()

    runtime.codeFont.getSize2D shouldBe 18.0f
    runtime.textFont.getSize2D shouldBe 18.0f
    runtime.uiFont.getSize2D shouldBe 16.0f
    runtime.codeMetrics shouldBe CellMetrics.fromFont(runtime.codeFont)
    runtime.textMetrics shouldBe CellMetrics.fromFont(runtime.textFont)
    runtime.uiMetrics shouldBe CellMetrics.fromFont(runtime.uiFont)
    runtime.codeFont.getSize2D should not be originalCodeFont.getSize2D
    runtime.codeMetrics should not be originalCodeMetric
  }

  it should "derive runtime fonts and metrics from the effective scaled font configuration" in {
    val runtime = RuntimeDisplayState
      .create(
        FontConfig(
          codeFontFamily = Font.MONOSPACED,
          textFontFamily = Font.SERIF,
          uiFontFamily = Font.SANS_SERIF,
          fontSize = 12.0f,
          textFontSize = 14.0f,
          uiFontSize = 16.0f,
          textScaleMultiplier = 1.5
        )
      )
      .unsafeRunSync()

    runtime.codeFont.getSize2D shouldBe 18.0f
    runtime.textFont.getSize2D shouldBe 21.0f
    runtime.uiFont.getSize2D shouldBe 24.0f
    runtime.codeMetrics shouldBe CellMetrics.fromFont(runtime.codeFont)
    runtime.textMetrics shouldBe CellMetrics.fromFont(runtime.textFont)
    runtime.uiMetrics shouldBe CellMetrics.fromFont(runtime.uiFont)
  }

  it should "use code metrics for the primary editor grid" in {
    val runtime = RuntimeDisplayState
      .create(
        FontConfig(
          codeFontFamily = "Monospaced",
          textFontFamily = "SansSerif",
          fontSize = 12.0f,
          textFontSize = 18.0f,
          uiFontSize = 24.0f
        )
      )
      .unsafeRunSync()

    runtime.textMetrics.charWidth should be > runtime.codeMetrics.charWidth
    runtime.uiMetrics.lineHeight should be > runtime.codeMetrics.lineHeight
    runtime.primaryMetrics shouldBe runtime.codeMetrics
  }

  it should "update primaryMetrics when code font config changes" in {
    val runtime = RuntimeDisplayState.create(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val before  = runtime.primaryMetrics

    runtime.update(FontConfig(codeFontFamily = "Monospaced", fontSize = 18.0f, uiFontSize = 28.0f)).unsafeRunSync()

    runtime.primaryMetrics shouldBe runtime.codeMetrics
    runtime.primaryMetrics should not be before
  }

  it should "keep primaryMetrics stable when only text or UI font metrics change" in {
    val runtime = RuntimeDisplayState
      .create(
        FontConfig(
          codeFontFamily = "Monospaced",
          textFontFamily = "SansSerif",
          fontSize = 14.0f,
          textFontSize = 18.0f,
          uiFontSize = 20.0f
        )
      )
      .unsafeRunSync()

    val metricsA = runtime.primaryMetrics

    runtime
      .update(
        FontConfig(
          codeFontFamily = "Monospaced",
          textFontFamily = "Dialog",
          fontSize = 14.0f,
          textFontSize = 30.0f,
          uiFontSize = 32.0f
        )
      )
      .unsafeRunSync()

    runtime.primaryMetrics shouldBe metricsA
  }

  it should "keep UI font family independent from code and text font family changes" in {
    val runtime = RuntimeDisplayState
      .create(
        FontConfig(
          codeFontFamily = Font.MONOSPACED,
          textFontFamily = Font.SERIF,
          uiFontFamily = Font.SANS_SERIF,
          fontSize = 14.0f,
          uiFontSize = 12.0f
        )
      )
      .unsafeRunSync()
    val originalUiFont    = runtime.uiFont
    val originalUiMetrics = runtime.uiMetrics

    runtime
      .update(
        FontConfig(
          codeFontFamily = Font.DIALOG_INPUT,
          textFontFamily = Font.DIALOG,
          uiFontFamily = Font.SANS_SERIF,
          fontSize = 18.0f,
          uiFontSize = 12.0f
        )
      )
      .unsafeRunSync()

    runtime.codeFont.getFamily should not be originalUiFont.getFamily
    runtime.textFont.getFamily should not be originalUiFont.getFamily
    runtime.uiFont.getFamily shouldBe originalUiFont.getFamily
    runtime.uiMetrics shouldBe originalUiMetrics
  }

  it should "expose fonts and metrics as one immutable snapshot" in {
    val runtime = RuntimeDisplayState
      .create(FontConfig(codeFontFamily = "Monospaced", fontSize = 12.0f, uiFontSize = 14.0f))
      .unsafeRunSync()

    val before = runtime.snapshot

    runtime
      .update(FontConfig(codeFontFamily = "Monospaced", fontSize = 24.0f, uiFontSize = 28.0f))
      .unsafeRunSync()

    // A reader holding one snapshot cannot observe a font from one generation beside a metric from
    // another, which is exactly what six independently-set references allowed.
    before.codeFont.getSize2D shouldBe 12.0f
    before.codeMetrics shouldBe CellMetrics.fromFont(before.codeFont)
    before.uiMetrics shouldBe CellMetrics.fromFont(before.uiFont)

    val after = runtime.snapshot
    after.codeFont.getSize2D shouldBe 24.0f
    after.codeMetrics shouldBe CellMetrics.fromFont(after.codeFont)
    after.uiMetrics shouldBe CellMetrics.fromFont(after.uiFont)
  }

  it should "keep every accessor agreeing with the current snapshot" in {
    val runtime = RuntimeDisplayState
      .create(FontConfig(codeFontFamily = "Monospaced", fontSize = 12.0f, uiFontSize = 14.0f))
      .unsafeRunSync()

    runtime
      .update(FontConfig(codeFontFamily = "Monospaced", fontSize = 24.0f, uiFontSize = 28.0f))
      .unsafeRunSync()

    val display = runtime.snapshot
    runtime.codeFont shouldBe display.codeFont
    runtime.textFont shouldBe display.textFont
    runtime.uiFont shouldBe display.uiFont
    runtime.codeMetrics shouldBe display.codeMetrics
    runtime.textMetrics shouldBe display.textMetrics
    runtime.uiMetrics shouldBe display.uiMetrics
    runtime.primaryMetrics shouldBe display.codeMetrics
  }

  it should "always produce valid metrics after update" in {
    val runtime = RuntimeDisplayState.create(FontConfig(fontSize = 12.0f, uiFontSize = 14.0f)).unsafeRunSync()

    runtime.codeMetrics.isValid shouldBe true
    runtime.textMetrics.isValid shouldBe true
    runtime.uiMetrics.isValid shouldBe true

    runtime.update(FontConfig(codeFontFamily = "Monospaced", fontSize = 18.0f, uiFontSize = 15.0f)).unsafeRunSync()

    runtime.codeMetrics.isValid shouldBe true
    runtime.textMetrics.isValid shouldBe true
    runtime.uiMetrics.isValid shouldBe true
  }
