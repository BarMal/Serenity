package com.serenity

import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.command.CommandRunnerOptionSelections
import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerOptionSelectionsSpec extends AnyFlatSpec with Matchers:

  it should "select the current global motion accessibility override" in {
    val selections = CommandRunnerOptionSelections.default(
      AppConfig.default.withMotionAccessibility(MotionAccessibility.Off)
    )

    selections("motion-accessibility") shouldBe 2
  }

  it should "select the command runner key-hints footer state" in {
    val enabled = CommandRunnerOptionSelections.default(AppConfig.default)
    enabled("command-runner-key-hints") shouldBe 0

    val disabled = CommandRunnerOptionSelections.default(AppConfig.default.withCommandRunnerShowKeyHints(false))
    disabled("command-runner-key-hints") shouldBe 1
  }

  "CommandRunnerOptionSelections" should "derive option indices from current app config" in {
    val codeFont = FontLoader.availableMonospaceFamilies.drop(1).headOption.getOrElse("missing-code-font")
    val textFont = FontLoader.availableTextFamilies.drop(1).headOption.getOrElse("missing-text-font")
    val uiFont   = FontLoader.availableUiFamilies.drop(1).headOption.getOrElse("missing-ui-font")
    val config = AppConfig.default.copy(
      surfaceConfig = AppConfig.default.surfaceConfig.copy(
        materialPreset = MaterialPreset.Crystal,
        postProcessingEffect = PostProcessingEffect.ScanlinesAndGlow,
        uiShadowsEnabled = false,
        motionPreset = MotionPreset.Expressive,
        commandRunnerAnimation = AnimationConfig.quick,
        uiAnimation = AnimationConfig.subtle,
        editorInsertionTransitionKind = TransitionKind.Disabled,
        commandRunnerTransitionKind = Some(TransitionKind.OutlineThenContent),
        panelOpenTransitionKind = Some(TransitionKind.OutlineThenContent),
        panelCloseTransitionKind = Some(TransitionKind.DirectionalSweep),
        backgroundStyle = BackgroundStyle.GlassLike,
        contextualToolbarDisplayMode = ToolbarDisplayMode.TextOnly,
        showLineNumbers = false,
        showGutter = false,
        wordWrapEnabled = false,
        contextualToolbarEnabled = false
      ),
      cursorConfig = CursorConfig(
        mode = CursorMode.Breathe,
        infoBarSegments = List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title),
        infoBarPlacement = CursorInfoBarPlacement.PinnedBottom
      ),
      documentConfig = DocumentConfig(
        markdownViewMode = MarkdownViewMode.InlineLens,
        defaultMode = DefaultDocumentMode.RichText
      ),
      interfaceConfig = InterfaceConfig(density = InterfaceDensity.Compact),
      windowConfig = WindowConfig(chromeMode = WindowChromeMode.NativeThemed),
      languageToolsConfig = LanguageToolsConfig(spellCheck = SpellCheckConfig(enabled = true)),
      editorConfig = EditorConfig(
        characterAnimation = AnimationConfig.subtle,
        fontConfig = AppConfig.default.editorConfig.fontConfig.copy(
          codeFontFamily = codeFont,
          textFontFamily = textFont,
          uiFontFamily = uiFont,
          textScaleMode = TextScaleMode.Off,
          enableLigatures = false,
          textLigatures = false,
          uiLigatures = true
        )
      )
    )

    val selections = CommandRunnerOptionSelections.default(config)

    selections("material-preset") shouldBe 3
    selections("post-processing") shouldBe 3
    selections("ui-shadows") shouldBe 0
    selections("motion-preset") shouldBe 3
    selections("command-runner-fade") shouldBe 3
    selections("ui-animation") shouldBe 1
    selections("editor-text-transition") shouldBe 4
    selections("command-runner-transition") shouldBe 3
    selections("panel-open-transition") shouldBe 3
    selections("panel-close-transition") shouldBe 1
    selections("cursor-mode") shouldBe 1
    selections("cursor-info-bar-title") shouldBe 0
    selections("cursor-info-bar-position") shouldBe 0
    selections("cursor-info-bar-word-count") shouldBe 1
    selections("cursor-info-bar-char-count") shouldBe 1
    selections("cursor-info-bar-reading-time") shouldBe 1
    selections("cursor-info-bar-placement") shouldBe 1
    selections("background-style") shouldBe 3
    selections("interface-density") shouldBe 0
    selections("window-chrome") shouldBe 2
    selections("markdown-view") shouldBe 2
    selections("default-document-mode") shouldBe 2
    selections("contextual-toolbar-display") shouldBe 1
    selections("spellcheck-enabled") shouldBe 1
    selections("line-numbers") shouldBe 1
    selections("gutter") shouldBe 1
    selections("line-wrap") shouldBe 1
    selections("word-wrap") shouldBe 1
    selections("contextual-toolbar") shouldBe 1
    selections("command-runner-key-hints") shouldBe 0
    selections("code-font") shouldBe FontLoader.availableMonospaceFamilies.indexOf(codeFont)
    selections("text-font") shouldBe FontLoader.availableTextFamilies.indexOf(textFont)
    selections("ui-font") shouldBe FontLoader.availableUiFamilies.indexOf(uiFont)
    selections("text-scale-mode") shouldBe 2
    selections("code-ligatures") shouldBe 1
    selections("text-ligatures") shouldBe 1
    selections("ui-ligatures") shouldBe 0
  }

  it should "fall back to the first font option when a configured family is unavailable" in {
    val config = AppConfig.default.withFontConfig(
      AppConfig.default.editorConfig.fontConfig.copy(
        codeFontFamily = "Unavailable Code",
        textFontFamily = "Unavailable Text",
        uiFontFamily = "Unavailable UI"
      )
    )

    val selections = CommandRunnerOptionSelections.default(config)

    selections("code-font") shouldBe 0
    selections("text-font") shouldBe 0
    selections("ui-font") shouldBe 0
  }

  it should "show custom as the selected motion preset for manually edited motion settings" in {
    val config = AppConfig.default.copy(
      surfaceConfig = AppConfig.default.surfaceConfig.copy(
        motionPreset = MotionPreset.Custom,
        commandRunnerAnimation = None,
        commandRunnerTransitionKind = Some(TransitionKind.TypedText),
        editorInsertionTransitionKind = TransitionKind.TypedText
      )
    )

    val selections = CommandRunnerOptionSelections.default(config)

    selections("motion-preset") shouldBe 4
    selections("command-runner-fade") shouldBe 0
    selections("command-runner-transition") shouldBe 1
    selections("editor-text-transition") shouldBe 1
  }
