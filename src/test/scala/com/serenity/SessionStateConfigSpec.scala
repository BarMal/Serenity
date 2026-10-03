package com.serenity

import java.awt.Color

import _root_.io.circe.syntax.*
import com.serenity.config.*
import com.serenity.config.AppConfigOps.*
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.rope.Balance
import com.serenity.session.SessionState
import com.serenity.session.given
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SessionStateConfigSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "SessionState" should "preserve config fields through JSON round trip" in {
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig(
          editorConfig = EditorConfig(
            fontConfig = com.serenity.ui.fonts.FontLoader.FontConfig(
              codeFontFamily = "Monospaced",
              textFontFamily = "SansSerif",
              uiFontFamily = "Dialog",
              fontSize = 15.0f,
              textFontSize = 16.0f,
              uiFontSize = 13.0f,
              enableLigatures = false,
              textLigatures = true,
              uiLigatures = true
            )
          ),
          surfaceConfig = SurfaceConfig(
            commandRunnerVisibleRows = Some(9),
            commandRunnerItemGapRows = Some(1),
            commandRunnerCursorGapRows = Some(3),
            renderFpsTarget = RenderFpsTarget.Fps120,
            showLineNumbers = false
          ),
          cursorConfig = CursorConfig(
            mode = CursorMode.Blink,
            colors = CursorColorConfig(
              active = Some(Color(0x22, 0x44, 0x88)),
              inactive = Some(Color(0x88, 0x44, 0x22, 0x99))
            )
          ),
          statusLine = StatusLineConfig(List(StatusSegment.Position, StatusSegment.Title), StatusLinePlacement.Pinned),
          windowConfig = WindowConfig(
            chromeMode = WindowChromeMode.Custom,
            preferredSize = Some(PreferredWindowSize(1400, 900))
          ),
          documentConfig = DocumentConfig(
            markdownViewMode = MarkdownViewMode.InlineLens,
            defaultMode = DefaultDocumentMode.Markdown
          ),
          interfaceConfig = InterfaceConfig(
            density = InterfaceDensity.Spacious,
            elementGap = Some(3),
            outlineThicknessPx = 4
          ),
          languageToolsConfig = LanguageToolsConfig(
            lspUserConfig = LspUserConfig(
              servers = Some(
                Map(
                  LanguageId.Scala.id -> LspServerOverride(
                    command = Some("custom-metals"),
                    args = Some(List("--stdio")),
                    enabled = Some(true)
                  )
                )
              )
            ),
            spellCheck = SpellCheckConfig(
              enabled = true,
              languages = List("en", "fr"),
              dictionaryPaths = List("C:\\Dictionaries\\en_US.dic"),
              additionalWords = List("serenity")
            )
          )
        )
      )
    )

    val decoded = SessionState.fromAppState(appState).asJson.as[SessionState].toOption.get

    decoded.config.surfaceConfig.commandRunnerVisibleRows shouldBe Some(9)
    decoded.config.surfaceConfig.commandRunnerItemGapRows shouldBe Some(1)
    decoded.config.surfaceConfig.commandRunnerCursorGapRows shouldBe Some(3)
    decoded.config.surfaceConfig.renderFpsTarget shouldBe RenderFpsTarget.Fps120
    decoded.config.cursorConfig shouldBe CursorConfig(
      mode = CursorMode.Blink,
      colors = CursorColorConfig(
        active = Some(Color(0x22, 0x44, 0x88)),
        inactive = Some(Color(0x88, 0x44, 0x22, 0x99))
      )
    )
    decoded.config.statusLine shouldBe
      StatusLineConfig(List(StatusSegment.Position, StatusSegment.Title), StatusLinePlacement.Pinned)
    decoded.config.windowConfig shouldBe WindowConfig(
      chromeMode = WindowChromeMode.Custom,
      preferredSize = Some(PreferredWindowSize(1400, 900))
    )
    decoded.config.documentConfig shouldBe DocumentConfig(
      markdownViewMode = MarkdownViewMode.InlineLens,
      defaultMode = DefaultDocumentMode.Markdown
    )
    decoded.config.interfaceConfig shouldBe InterfaceConfig(
      density = InterfaceDensity.Spacious,
      elementGap = Some(3),
      outlineThicknessPx = 4
    )
    decoded.config.editorConfig.fontConfig.codeFontFamily shouldBe "Monospaced"
    decoded.config.editorConfig.fontConfig.textFontFamily shouldBe "SansSerif"
    decoded.config.editorConfig.fontConfig.uiFontFamily shouldBe "Dialog"
    decoded.config.editorConfig.fontConfig.codeFontSize shouldBe 15.0f
    decoded.config.editorConfig.fontConfig.textFontSize shouldBe 16.0f
    decoded.config.editorConfig.fontConfig.uiFontSize shouldBe 13.0f
    decoded.config.editorConfig.fontConfig.codeLigatures shouldBe false
    decoded.config.editorConfig.fontConfig.textLigatures shouldBe true
    decoded.config.editorConfig.fontConfig.uiLigatures shouldBe true
    decoded.config.surfaceConfig.showLineNumbers shouldBe false
    decoded.config.languageToolsConfig.lspUserConfig.servers.map(_(LanguageId.Scala.id)) shouldBe Some(
      LspServerOverride(
        command = Some("custom-metals"),
        args = Some(List("--stdio")),
        enabled = Some(true)
      )
    )
    decoded.config.languageToolsConfig.spellCheck shouldBe SpellCheckConfig(
      enabled = true,
      languages = List("en", "fr"),
      dictionaryPaths = List("C:\\Dictionaries\\en_US.dic"),
      additionalWords = List("serenity")
    )
  }

  it should "round-trip visualLineCursorNavigation disabled" in {
    val original = SessionState.fromAppState(
      AppState.initial.copy(persisted =
        AppState.initial.persisted.copy(config = AppConfig.default.withVisualLineCursorNavigation(false))
      )
    )

    val decoded = original.asJson.as[SessionState]

    decoded.toOption.map(_.config.surfaceConfig.visualLineCursorNavigation) shouldBe Some(false)
  }

  it should "round-trip native-themed window chrome" in {
    val original = SessionState.fromAppState(
      AppState.initial.copy(persisted =
        AppState.initial.persisted.copy(config = AppConfig.default.withWindowChromeMode(WindowChromeMode.NativeThemed))
      )
    )

    val decoded = original.asJson.as[SessionState]

    decoded.toOption.map(_.config.windowChromeMode) shouldBe Some(WindowChromeMode.NativeThemed)
  }

  it should "round-trip automatic window chrome" in {
    val original = SessionState.fromAppState(
      AppState.initial.copy(persisted =
        AppState.initial.persisted.copy(config = AppConfig.default.withWindowChromeMode(WindowChromeMode.Auto))
      )
    )

    original.asJson.as[SessionState].toOption.map(_.config.windowChromeMode) shouldBe Some(WindowChromeMode.Auto)
  }

  it should "round trip decimal floating-surface spacing" in {
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default
          .withUiElementGap(Some(0.75))
          .withCommandRunnerItemGapRows(Some(0.25))
          .withCommandRunnerCursorGapRows(Some(0.5))
      )
    )

    val decoded = SessionState.fromAppState(state).asJson.as[SessionState].toOption.get

    decoded.config.uiElementGap shouldBe Some(0.75)
    decoded.config.surfaceConfig.commandRunnerItemGapRows shouldBe Some(0.25)
    decoded.config.surfaceConfig.commandRunnerCursorGapRows shouldBe Some(0.5)
  }
