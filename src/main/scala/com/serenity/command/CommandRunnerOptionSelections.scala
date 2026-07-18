package com.serenity.command

import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.TextScaleMode

object CommandRunnerOptionSelections:

  def default(config: AppConfig): Map[String, Int] =
    val editorConfig        = config.editorConfig
    val surfaceConfig       = config.surfaceConfig
    val cursorConfig        = config.cursorConfig
    val documentConfig      = config.documentConfig
    val interfaceConfig     = config.interfaceConfig
    val languageToolsConfig = config.languageToolsConfig

    Map(
      "animation-mode"  -> animationModeIndex(editorConfig.characterAnimation),
      "material-preset" -> materialPresetIndex(surfaceConfig.materialPreset),
      "post-processing" -> postProcessingEffectIndex(surfaceConfig.postProcessingEffect),
      "motion-preset"   -> motionPresetIndex(surfaceConfig.motionPreset),
      "motion-accessibility" -> motionAccessibilityIndex(
        surfaceConfig.motionConfiguration.fold(MotionAccessibility.Standard)(_.accessibility)
      ),
      "command-runner-fade"        -> commandRunnerFadeIndex(surfaceConfig.commandRunnerAnimation),
      "ui-animation"               -> animationPresetIndex(surfaceConfig.uiAnimation),
      "render-fps"                 -> renderFpsTargetIndex(surfaceConfig.renderFpsTarget),
      "editor-text-transition"     -> editorTextTransitionIndex(surfaceConfig.editorInsertionTransitionKind),
      "command-runner-transition"  -> panelTransitionIndex(config.effectiveCommandRunnerTransitionKind),
      "panel-open-transition"      -> panelTransitionIndex(config.effectivePanelOpenTransitionKind),
      "panel-close-transition"     -> panelTransitionIndex(config.effectivePanelCloseTransitionKind),
      "cursor-mode"                -> cursorModeIndex(cursorConfig.mode),
      "cursor-info-bar"            -> cursorInfoBarModeIndex(cursorConfig.infoBarMode),
      "cursor-info-bar-placement"  -> cursorInfoBarPlacementIndex(cursorConfig.infoBarPlacement),
      "background-style"           -> backgroundStyleIndex(surfaceConfig.backgroundStyle),
      "interface-density"          -> interfaceDensityIndex(interfaceConfig.density),
      "window-chrome"              -> windowChromeModeIndex(config.windowChromeMode),
      "markdown-view"              -> markdownViewModeIndex(documentConfig.markdownViewMode),
      "default-document-mode"      -> defaultDocumentModeIndex(documentConfig.defaultMode),
      "spellcheck-enabled"         -> spellCheckEnabledIndex(languageToolsConfig.spellCheck.enabled),
      "line-numbers"               -> enabledIndex(surfaceConfig.showLineNumbers),
      "gutter"                     -> enabledIndex(surfaceConfig.showGutter),
      "line-wrap"                  -> enabledIndex(surfaceConfig.wordWrapEnabled),
      "word-wrap"                  -> enabledIndex(surfaceConfig.wordWrapEnabled),
      "focused-text-body"          -> enabledIndex(surfaceConfig.focusedTextBodyEnabled),
      "contextual-toolbar"         -> enabledIndex(surfaceConfig.contextualToolbarEnabled),
      "contextual-toolbar-display" -> contextualToolbarDisplayModeIndex(surfaceConfig.contextualToolbarDisplayMode),
      "code-font"                  -> codeFontIndex(editorConfig.fontConfig.codeFontFamily),
      "text-font"                  -> textFontIndex(editorConfig.fontConfig.textFontFamily),
      "ui-font"                    -> uiFontIndex(editorConfig.fontConfig.uiFontFamily),
      "text-scale-mode"            -> textScaleModeIndex(editorConfig.fontConfig.textScaleMode),
      "code-ligatures"             -> ligaturesIndex(editorConfig.fontConfig.codeLigatures),
      "text-ligatures"             -> ligaturesIndex(editorConfig.fontConfig.textLigatures),
      "ui-ligatures"               -> ligaturesIndex(editorConfig.fontConfig.uiLigatures)
    )

  private def animationModeIndex(animation: Option[AnimationConfig]): Int =
    animation match
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

  private def windowChromeModeIndex(mode: WindowChromeMode): Int =
    mode match
      case WindowChromeMode.Native       => 0
      case WindowChromeMode.NativeThemed => 1
      case WindowChromeMode.Custom       => 2

  private def materialPresetIndex(preset: MaterialPreset): Int =
    preset match
      case MaterialPreset.Solid   => 0
      case MaterialPreset.Clear   => 1
      case MaterialPreset.Frosted => 2
      case MaterialPreset.Crystal => 3
      case MaterialPreset.Custom  => 4

  private def postProcessingEffectIndex(effect: PostProcessingEffect): Int =
    effect match
      case PostProcessingEffect.Off       => 0
      case PostProcessingEffect.Scanlines => 1
      case PostProcessingEffect.Glow      => 2

  private def motionPresetIndex(preset: MotionPreset): Int =
    preset match
      case MotionPreset.Reduced    => 0
      case MotionPreset.Subtle     => 1
      case MotionPreset.Smooth     => 2
      case MotionPreset.Expressive => 3
      case MotionPreset.Custom     => 4

  private def motionAccessibilityIndex(accessibility: MotionAccessibility): Int =
    accessibility match
      case MotionAccessibility.Standard => 0
      case MotionAccessibility.Reduced  => 1
      case MotionAccessibility.Off      => 2

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

  private def contextualToolbarDisplayModeIndex(mode: ToolbarDisplayMode): Int =
    mode match
      case ToolbarDisplayMode.IconOnly    => 0
      case ToolbarDisplayMode.TextOnly    => 1
      case ToolbarDisplayMode.IconAndText => 2

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
