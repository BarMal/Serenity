package com.serenity.command

import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.TextScaleMode

object CommandRunnerOptionSelections:

  def default(config: AppConfig): Map[String, Int] =
    Map(
      "animation-mode"            -> animationModeIndex(config),
      "material-preset"           -> materialPresetIndex(config.materialPreset),
      "motion-preset"             -> motionPresetIndex(config.motionPreset),
      "command-runner-fade"       -> commandRunnerFadeIndex(config.commandRunnerAnimation),
      "ui-animation"              -> animationPresetIndex(config.uiAnimation),
      "render-fps"                -> renderFpsTargetIndex(config.renderFpsTarget),
      "editor-text-transition"    -> editorTextTransitionIndex(config.editorInsertionTransitionKind),
      "command-runner-transition" -> panelTransitionIndex(config.effectiveCommandRunnerTransitionKind),
      "panel-open-transition"     -> panelTransitionIndex(config.effectivePanelOpenTransitionKind),
      "panel-close-transition"    -> panelTransitionIndex(config.effectivePanelCloseTransitionKind),
      "cursor-mode"               -> cursorModeIndex(config.cursorMode),
      "cursor-info-bar"           -> cursorInfoBarModeIndex(config.cursorInfoBarMode),
      "cursor-info-bar-placement" -> cursorInfoBarPlacementIndex(config.cursorInfoBarPlacement),
      "background-style"          -> backgroundStyleIndex(config.backgroundStyle),
      "interface-density"         -> interfaceDensityIndex(config.interfaceDensity),
      "markdown-view"             -> markdownViewModeIndex(config.markdownViewMode),
      "default-document-mode"     -> defaultDocumentModeIndex(config.defaultDocumentMode),
      "spellcheck-enabled"        -> spellCheckEnabledIndex(config.spellCheck.enabled),
      "line-numbers"              -> enabledIndex(config.showLineNumbers),
      "gutter"                    -> enabledIndex(config.showGutter),
      "line-wrap"                 -> enabledIndex(config.wordWrapEnabled),
      "word-wrap"                 -> enabledIndex(config.wordWrapEnabled),
      "focused-text-body"         -> enabledIndex(config.focusedTextBodyEnabled),
      "code-font"                 -> codeFontIndex(config.fontConfig.codeFontFamily),
      "text-font"                 -> textFontIndex(config.fontConfig.textFontFamily),
      "ui-font"                   -> uiFontIndex(config.fontConfig.uiFontFamily),
      "text-scale-mode"           -> textScaleModeIndex(config.fontConfig.textScaleMode),
      "code-ligatures"            -> ligaturesIndex(config.fontConfig.codeLigatures),
      "text-ligatures"            -> ligaturesIndex(config.fontConfig.textLigatures),
      "ui-ligatures"              -> ligaturesIndex(config.fontConfig.uiLigatures)
    )

  private def animationModeIndex(config: AppConfig): Int =
    config.characterAnimation match
      case None                                                          => 0
      case Some(animation) if AnimationConfig.subtle.contains(animation) => 1
      case _                                                             => 2

  private def cursorModeIndex(mode: CursorMode): Int =
    mode match
      case CursorMode.Blink   => 0
      case CursorMode.Breathe => 1

  private def cursorInfoBarModeIndex(mode: CursorInfoBarMode): Int =
    mode match
      case CursorInfoBarMode.Off      => 0
      case CursorInfoBarMode.Position => 1
      case CursorInfoBarMode.Detailed => 2

  private def cursorInfoBarPlacementIndex(placement: CursorInfoBarPlacement): Int =
    placement match
      case CursorInfoBarPlacement.Floating     => 0
      case CursorInfoBarPlacement.PinnedBottom => 1

  private def backgroundStyleIndex(style: BackgroundStyle): Int =
    style match
      case BackgroundStyle.Solid       => 0
      case BackgroundStyle.Transparent => 1
      case BackgroundStyle.Frosted     => 2
      case BackgroundStyle.GlassLike   => 3

  private def interfaceDensityIndex(density: InterfaceDensity): Int =
    density match
      case InterfaceDensity.Compact     => 0
      case InterfaceDensity.Comfortable => 1
      case InterfaceDensity.Spacious    => 2

  private def materialPresetIndex(preset: MaterialPreset): Int =
    preset match
      case MaterialPreset.Solid   => 0
      case MaterialPreset.Clear   => 1
      case MaterialPreset.Frosted => 2
      case MaterialPreset.Crystal => 3
      case MaterialPreset.Custom  => 4

  private def motionPresetIndex(preset: MotionPreset): Int =
    preset match
      case MotionPreset.Reduced    => 0
      case MotionPreset.Subtle     => 1
      case MotionPreset.Smooth     => 2
      case MotionPreset.Expressive => 3
      case MotionPreset.Custom     => 4

  private def commandRunnerFadeIndex(animation: Option[AnimationConfig]): Int =
    animationPresetIndex(animation)

  private def animationPresetIndex(animation: Option[AnimationConfig]): Int =
    animation match
      case None                                                  => 0
      case Some(value) if AnimationConfig.subtle.contains(value) => 1
      case Some(value) if AnimationConfig.smooth.contains(value) => 2
      case Some(value) if AnimationConfig.quick.contains(value)  => 3
      case Some(_)                                               => 2

  private def renderFpsTargetIndex(target: RenderFpsTarget): Int =
    target match
      case RenderFpsTarget.Fps30    => 0
      case RenderFpsTarget.Fps60    => 1
      case RenderFpsTarget.Fps90    => 2
      case RenderFpsTarget.Fps120   => 3
      case RenderFpsTarget.Uncapped => 4

  private def editorTextTransitionIndex(kind: TransitionKind): Int =
    kind match
      case TransitionKind.Fade                   => 0
      case TransitionKind.TypedText              => 1
      case TransitionKind.DirectionalSweep       => 2
      case TransitionKind.LineAndCharacterTandem => 3
      case TransitionKind.Disabled               => 4
      case TransitionKind.OutlineThenContent     => 0

  private def panelTransitionIndex(kind: TransitionKind): Int =
    kind match
      case TransitionKind.Fade                   => 0
      case TransitionKind.DirectionalSweep       => 1
      case TransitionKind.LineAndCharacterTandem => 2
      case TransitionKind.OutlineThenContent     => 3
      case TransitionKind.Disabled               => 4
      case TransitionKind.TypedText              => 1

  private def markdownViewModeIndex(mode: MarkdownViewMode): Int =
    mode match
      case MarkdownViewMode.Source       => 0
      case MarkdownViewMode.SplitPreview => 1
      case MarkdownViewMode.InlineLens   => 2

  private def defaultDocumentModeIndex(mode: DefaultDocumentMode): Int =
    mode match
      case DefaultDocumentMode.PlainText => 0
      case DefaultDocumentMode.Markdown  => 1
      case DefaultDocumentMode.RichText  => 2

  private def spellCheckEnabledIndex(enabled: Boolean): Int =
    if enabled then 1 else 0

  private def enabledIndex(enabled: Boolean): Int =
    if enabled then 0 else 1

  private def codeFontIndex(family: String): Int =
    FontLoader.availableMonospaceFamilies.indexOf(family) match
      case -1    => 0
      case index => index

  private def textFontIndex(family: String): Int =
    FontLoader.availableTextFamilies.indexOf(family) match
      case -1    => 0
      case index => index

  private def uiFontIndex(family: String): Int =
    FontLoader.availableUiFamilies.indexOf(family) match
      case -1    => 0
      case index => index

  private def textScaleModeIndex(mode: TextScaleMode): Int =
    mode match
      case TextScaleMode.Auto   => 0
      case TextScaleMode.Manual => 1
      case TextScaleMode.Off    => 2

  private def ligaturesIndex(enabled: Boolean): Int =
    if enabled then 0 else 1
