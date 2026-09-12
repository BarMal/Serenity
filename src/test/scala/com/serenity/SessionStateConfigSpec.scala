package com.serenity

import java.awt.Color

import _root_.io.circe.syntax.*
import com.serenity.animation.{AnimationConfig, TransitionKind, TransitionScope}
import com.serenity.config.*
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import com.serenity.rope.Balance
import com.serenity.session.given
import com.serenity.session.SessionState
import com.serenity.state.models.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SessionStateConfigSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "SessionState" should "preserve config fields including blurRadius and backgroundStyle through JSON round trip" in {
    val appState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig(
          editorConfig = EditorConfig(
            characterAnimation = AnimationConfig.quick,
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
            blurRadius = 0.42f,
            backgroundStyle = BackgroundStyle.GlassLike,
            materialPreset = MaterialPreset.Crystal,
            motionPreset = MotionPreset.Reduced,
            elementTransitionSpeedScale = 1.75,
            editorTextTransitionSpeedScale = Some(0.5),
            commandRunnerTransitionSpeedScale = Some(2.25),
            uiTransitionSpeedScale = Some(1.25),
            cursorTransitionSpeedScale = Some(0.75),
            commandRunnerTransitionKind = Some(TransitionKind.OutlineThenContent),
            panelOpenTransitionKind = Some(TransitionKind.DirectionalSweep),
            panelCloseTransitionKind = Some(TransitionKind.Disabled),
            uiAnimation = AnimationConfig.subtle,
            motionConfiguration = Some(
              MotionConfig(
                MotionAccessibility.Off,
                MotionPreset.Smooth,
                Map(
                  MotionFamily.CommandSurfaces -> MotionFamilyConfig(
                    transitionKind = TransitionKind.TypedText,
                    animation = AnimationConfig.subtle,
                    speedScale = 0.5
                  ),
                  MotionFamily.PinnedPanels -> MotionFamilyConfig(
                    transitionKind = TransitionKind.DirectionalSweep,
                    animation = AnimationConfig.smooth,
                    speedScale = 1.0,
                    transitionOverrides = Map(
                      TransitionScope.PanelOpen  -> TransitionKind.DirectionalSweep,
                      TransitionScope.PanelClose -> TransitionKind.Disabled
                    )
                  )
                )
              )
            ),
            commandRunnerVisibleRows = Some(9),
            commandRunnerItemGapRows = 1,
            commandRunnerCursorGapRows = Some(3),
            renderFpsTarget = RenderFpsTarget.Fps120,
            showLineNumbers = false,
            showGutter = false
          ),
          cursorConfig = CursorConfig(
            mode = CursorMode.Breathe,
            colors = CursorColorConfig(
              active = Some(Color(0x22, 0x44, 0x88)),
              inactive = Some(Color(0x88, 0x44, 0x22, 0x99))
            ),
            infoBarSegments = List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title),
            infoBarPlacement = CursorInfoBarPlacement.PinnedBottom
          ),
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
            elementGap = 3,
            cornerRadiusPx = 12,
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

    decoded.config.surfaceConfig.blurRadius shouldBe 0.42f
    decoded.config.surfaceConfig.backgroundStyle shouldBe BackgroundStyle.GlassLike
    decoded.config.surfaceConfig.materialPreset shouldBe MaterialPreset.Crystal
    decoded.config.surfaceConfig.motionPreset shouldBe MotionPreset.Reduced
    decoded.config.surfaceConfig.elementTransitionSpeedScale shouldBe 1.75
    decoded.config.surfaceConfig.editorTextTransitionSpeedScale shouldBe Some(0.5)
    decoded.config.surfaceConfig.commandRunnerTransitionSpeedScale shouldBe Some(2.25)
    decoded.config.surfaceConfig.uiTransitionSpeedScale shouldBe Some(1.25)
    decoded.config.surfaceConfig.cursorTransitionSpeedScale shouldBe Some(0.75)
    decoded.config.surfaceConfig.commandRunnerTransitionKind shouldBe Some(TransitionKind.OutlineThenContent)
    decoded.config.surfaceConfig.panelOpenTransitionKind shouldBe Some(TransitionKind.DirectionalSweep)
    decoded.config.surfaceConfig.panelCloseTransitionKind shouldBe Some(TransitionKind.Disabled)
    decoded.config.surfaceConfig.uiAnimation shouldBe AnimationConfig.subtle
    decoded.config.surfaceConfig.motionConfiguration shouldBe appState.persisted.config.surfaceConfig.motionConfiguration
    decoded.config.surfaceConfig.commandRunnerVisibleRows shouldBe Some(9)
    decoded.config.surfaceConfig.commandRunnerItemGapRows shouldBe 1
    decoded.config.surfaceConfig.commandRunnerCursorGapRows shouldBe Some(3)
    decoded.config.surfaceConfig.renderFpsTarget shouldBe RenderFpsTarget.Fps120
    decoded.config.cursorConfig shouldBe CursorConfig(
      mode = CursorMode.Breathe,
      colors = CursorColorConfig(
        active = Some(Color(0x22, 0x44, 0x88)),
        inactive = Some(Color(0x88, 0x44, 0x22, 0x99))
      ),
      infoBarSegments = List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title),
      infoBarPlacement = CursorInfoBarPlacement.PinnedBottom
    )
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
      elementGap = 3,
      cornerRadiusPx = 12,
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
    decoded.config.surfaceConfig.showGutter shouldBe false
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
    decoded.config.editorConfig.characterAnimation.map(_.steps) shouldBe
      AnimationConfig.quick.map(_.steps)
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
          .withUiElementGap(0.75)
          .withCommandRunnerItemGapRows(0.25)
          .withCommandRunnerCursorGapRows(Some(0.5))
      )
    )

    val decoded = SessionState.fromAppState(state).asJson.as[SessionState].toOption.get

    decoded.config.uiElementGap shouldBe 0.75
    decoded.config.surfaceConfig.commandRunnerItemGapRows shouldBe 0.25
    decoded.config.surfaceConfig.commandRunnerCursorGapRows shouldBe Some(0.5)
  }
