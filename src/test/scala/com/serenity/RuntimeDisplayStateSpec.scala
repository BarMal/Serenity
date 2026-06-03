package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.lsp.config.LanguageId
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.app.RuntimeDisplayState
import com.serenity.ui.layout.CellMetrics
import com.serenity.state.models.{AppState, BufferId, Focus, StartupPage, SurfaceContent, SurfaceId, SurfacePlacement, SurfacePresentation, UiSurface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class RuntimeDisplayStateSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given org.typelevel.log4cats.Logger[IO] =
    LoggerFactory[IO].getLogger(using LoggerName("RuntimeDisplayStateSpec"))

  "RuntimeDisplayState" should "derive cell metrics from the current runtime font" in {
    val runtime = RuntimeDisplayState.create(FontConfig(fontSize = 12.0f)).unsafeRunSync()

    runtime.codeMetrics shouldBe CellMetrics.fromFont(runtime.codeFont)
    runtime.textMetrics shouldBe CellMetrics.fromFont(runtime.textFont)
  }

  it should "refresh both runtime fonts and metrics when the font config changes" in {
    val runtime        = RuntimeDisplayState.create(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val originalCodeFont   = runtime.codeFont
    val originalCodeMetric = runtime.codeMetrics

    runtime.update(FontConfig(codeFontFamily = "Monospaced", textFontFamily = "SansSerif", fontSize = 18.0f)).unsafeRunSync()

    runtime.codeFont.getSize2D shouldBe 18.0f
    runtime.textFont.getSize2D shouldBe 18.0f
    runtime.codeMetrics shouldBe CellMetrics.fromFont(runtime.codeFont)
    runtime.textMetrics shouldBe CellMetrics.fromFont(runtime.textFont)
    runtime.codeFont.getSize2D should not be originalCodeFont.getSize2D
    runtime.codeMetrics should not be originalCodeMetric
  }

  it should "use the code font for code-like active buffers" in {
    val runtime = RuntimeDisplayState.create(
      FontConfig(codeFontFamily = "Monospaced", textFontFamily = "SansSerif")
    ).unsafeRunSync()

    val state = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        BufferId(0),
        AppState.initial.buffers(BufferId(0)).copy(language = Some(LanguageId.Scala))
      )
    )

    runtime.fontFor(state) shouldBe runtime.codeFont
    runtime.metricsFor(state) shouldBe runtime.codeMetrics
  }

  it should "use the text font for non-code active buffers" in {
    val runtime = RuntimeDisplayState.create(
      FontConfig(codeFontFamily = "Monospaced", textFontFamily = "SansSerif")
    ).unsafeRunSync()

    val codeState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        BufferId(0),
        AppState.initial.buffers(BufferId(0)).copy(language = Some(LanguageId.Markdown))
      )
    )

    runtime.fontFor(codeState) shouldBe runtime.textFont
    runtime.metricsFor(codeState) shouldBe runtime.textMetrics
  }

  it should "keep using the code font while a command runner surface is focused over a code buffer" in {
    val runtime = RuntimeDisplayState.create(
      FontConfig(codeFontFamily = "Monospaced", textFontFamily = "SansSerif")
    ).unsafeRunSync()

    val baseState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        BufferId(0),
        AppState.initial.buffers(BufferId(0)).copy(language = Some(LanguageId.Scala))
      )
    )
    val state = baseState.copy(
      focus = Focus.Surface(SurfaceId("command-runner")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(com.serenity.command.CommandRunner.empty),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )

    runtime.fontFor(state) shouldBe runtime.codeFont
    runtime.metricsFor(state) shouldBe runtime.codeMetrics
  }

  it should "use the code font for structured chrome surfaces even over non-code buffers" in {
    val runtime = RuntimeDisplayState.create(
      FontConfig(codeFontFamily = "Monospaced", textFontFamily = "SansSerif")
    ).unsafeRunSync()

    val markdownState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        BufferId(0),
        AppState.initial.buffers(BufferId(0)).copy(language = Some(LanguageId.Markdown))
      ),
      focus = Focus.Surface(SurfaceId("startup")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("startup"),
          SurfaceContent.StartPage(StartupPage("Serenity", List("New", "Restore"))),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )

    runtime.fontFor(markdownState) shouldBe runtime.codeFont
    runtime.metricsFor(markdownState) shouldBe runtime.codeMetrics
  }
