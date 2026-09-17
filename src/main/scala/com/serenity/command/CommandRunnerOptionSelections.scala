package com.serenity.command

import com.serenity.animation.sprite.SpriteFrameCycle
import com.serenity.animation.{AnimationConfig, TransitionKind, TransitionScope}
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
      "material-preset" -> materialPresetIndex(surfaceConfig.materialPreset),
      "post-processing" -> postProcessingEffectIndex(surfaceConfig.postProcessingEffect),
      "ui-shadows"      -> enabledIndex(surfaceConfig.uiShadowsEnabled),
      "motion-preset"   -> motionPresetIndex(surfaceConfig.motionPreset),
      "motion-accessibility" -> motionAccessibilityIndex(
        surfaceConfig.motionConfiguration.fold(MotionAccessibility.Standard)(_.accessibility)
      ),
      "command-runner-fade"       -> commandRunnerFadeIndex(surfaceConfig.commandRunnerAnimation),
      "ui-animation"              -> animationPresetIndex(surfaceConfig.uiAnimation),
      "render-fps"                -> renderFpsTargetIndex(surfaceConfig.renderFpsTarget),
      "render-damage-granularity" -> renderDamageGranularityIndex(surfaceConfig.renderDamageGranularity),
      "editor-text-transition" -> editorTextTransitionIndex(
        configuredTransitionKind(surfaceConfig, MotionFamily.EditorText, TransitionScope.EditorInsertion)
      ),
      "command-runner-transition" -> panelTransitionIndex(
        configuredTransitionKind(surfaceConfig, MotionFamily.CommandSurfaces, TransitionScope.CommandRunner)
      ),
      "panel-open-transition" -> panelTransitionIndex(
        configuredTransitionKind(surfaceConfig, MotionFamily.PinnedPanels, TransitionScope.PanelOpen)
      ),
      "panel-close-transition" -> panelTransitionIndex(
        configuredTransitionKind(surfaceConfig, MotionFamily.PinnedPanels, TransitionScope.PanelClose)
      ),
      "cursor-mode"                -> cursorModeIndex(cursorConfig.mode),
      "status-placement"           -> statusPlacementIndex(config.statusLine.placement),
      "status-position"            -> enabledIndex(config.statusLine.segments.contains(StatusSegment.Position)),
      "status-title"               -> enabledIndex(config.statusLine.segments.contains(StatusSegment.Title)),
      "status-language"            -> enabledIndex(config.statusLine.segments.contains(StatusSegment.Language)),
      "status-mode"                -> enabledIndex(config.statusLine.segments.contains(StatusSegment.Mode)),
      "status-word-count"          -> enabledIndex(config.statusLine.segments.contains(StatusSegment.WordCount)),
      "status-char-count"          -> enabledIndex(config.statusLine.segments.contains(StatusSegment.CharCount)),
      "status-reading-time"        -> enabledIndex(config.statusLine.segments.contains(StatusSegment.ReadingTime)),
      "background-style"           -> backgroundStyleIndex(surfaceConfig.backgroundStyle),
      "interface-density"          -> interfaceDensityIndex(interfaceConfig.density),
      "window-chrome"              -> windowChromeModeIndex(config.windowChromeMode),
      "companion-sprite-enabled"   -> enabledIndex(config.companionSpriteConfig.enabled),
      "companion-sprite-typing-cycle" -> spriteFrameCycleIndex(config.companionSpriteConfig.typingCycle),
      "visual-flair-level"         -> visualFlairLevelIndex(config.visualFlairLevel),
      "markdown-view"              -> markdownViewModeIndex(documentConfig.markdownViewMode),
      "default-document-mode"      -> defaultDocumentModeIndex(documentConfig.defaultMode),
      "spellcheck-enabled"         -> enabledIndex(languageToolsConfig.spellCheck.enabled),
      "app-mode"                   -> appModeIndex(config.appMode),
      "settings-show-all"          -> enabledIndex(config.showAllSettingsRegardlessOfMode),
      "line-numbers"               -> enabledIndex(surfaceConfig.showLineNumbers),
      "line-number-side"           -> lineNumberSideIndex(surfaceConfig.lineNumberLayout.side),
      "line-wrap"                  -> enabledIndex(surfaceConfig.wordWrapEnabled),
      "visual-line-navigation"     -> enabledIndex(surfaceConfig.visualLineCursorNavigation),
      "typewriter-scrolling"       -> enabledIndex(surfaceConfig.typewriterScrollingEnabled),
      "focused-text-body"          -> enabledIndex(surfaceConfig.focusedTextBodyEnabled),
      "contextual-toolbar"         -> enabledIndex(surfaceConfig.contextualToolbarEnabled),
      "contextual-toolbar-display" -> contextualToolbarDisplayModeIndex(surfaceConfig.contextualToolbarDisplayMode),
      "command-runner-key-hints"   -> enabledIndex(surfaceConfig.commandRunnerShowKeyHints),
      "code-font"                  -> codeFontIndex(editorConfig.fontConfig.codeFontFamily),
      "text-font"                  -> textFontIndex(editorConfig.fontConfig.textFontFamily),
      "ui-font"                    -> uiFontIndex(editorConfig.fontConfig.uiFontFamily),
      "text-scale-mode"            -> textScaleModeIndex(editorConfig.fontConfig.textScaleMode),
      "code-ligatures"             -> ligaturesIndex(editorConfig.fontConfig.codeLigatures),
      "text-ligatures"             -> ligaturesIndex(editorConfig.fontConfig.textLigatures),
      "ui-ligatures"               -> ligaturesIndex(editorConfig.fontConfig.uiLigatures)
    )

  private def configuredTransitionKind(
    surfaceConfig: SurfaceConfig,
    family: MotionFamily,
    scope: TransitionScope
  ): TransitionKind =
    surfaceConfig.motionConfiguration.fold {
      family match
        case MotionFamily.EditorText      => surfaceConfig.editorInsertionTransitionKind
        case MotionFamily.CommandSurfaces => surfaceConfig.commandRunnerTransitionKind.getOrElse(TransitionKind.Fade)
        case MotionFamily.PinnedPanels =>
          scope match
            case TransitionScope.PanelOpen =>
              surfaceConfig.panelOpenTransitionKind.getOrElse(TransitionKind.OutlineThenContent)
            case TransitionScope.PanelClose => surfaceConfig.panelCloseTransitionKind.getOrElse(TransitionKind.Fade)
            case _                          => TransitionKind.Fade
        case _ => TransitionKind.Fade
    } { configuration =>
      configuration
        .withFallback(MotionConfig.fromLegacy(surfaceConfig, configuration.baseline))
        .families(family)
        .transitionKindFor(scope)
    }

  private def cursorModeIndex(mode: CursorMode): Int =
    mode match
      case CursorMode.Blink   => 0
      case CursorMode.Breathe => 1

  private def statusPlacementIndex(placement: StatusLinePlacement): Int =
    placement match
      case StatusLinePlacement.Pinned   => 0
      case StatusLinePlacement.Floating => 1
      case StatusLinePlacement.Off      => 2

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

  private def lineNumberSideIndex(side: LineNumberSide): Int =
    side match
      case LineNumberSide.Left  => 0
      case LineNumberSide.Right => 1
      case LineNumberSide.Both  => 2

  private def windowChromeModeIndex(mode: WindowChromeMode): Int =
    mode match
      case WindowChromeMode.Auto         => 0
      case WindowChromeMode.Native       => 1
      case WindowChromeMode.NativeThemed => 2
      case WindowChromeMode.Custom       => 3

  private def spriteFrameCycleIndex(cycle: SpriteFrameCycle): Int =
    cycle match
      case SpriteFrameCycle.Cycle => 0
      case SpriteFrameCycle.Pulse => 1
      case SpriteFrameCycle.Blink => 2

  private def visualFlairLevelIndex(level: VisualFlairLevel): Int =
    level match
      case VisualFlairLevel.Full    => 0
      case VisualFlairLevel.Reduced => 1
      case VisualFlairLevel.Off     => 2

  private def materialPresetIndex(preset: MaterialPreset): Int =
    preset match
      case MaterialPreset.Solid   => 0
      case MaterialPreset.Clear   => 1
      case MaterialPreset.Frosted => 2
      case MaterialPreset.Crystal => 3
      case MaterialPreset.Custom  => 4

  private def postProcessingEffectIndex(effect: PostProcessingEffect): Int =
    effect match
      case PostProcessingEffect.Off              => 0
      case PostProcessingEffect.Scanlines        => 1
      case PostProcessingEffect.Glow             => 2
      case PostProcessingEffect.ScanlinesAndGlow => 3

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

  private def renderDamageGranularityIndex(granularity: RenderDamageGranularity): Int =
    granularity match
      case RenderDamageGranularity.Rows  => 0
      case RenderDamageGranularity.Cells => 1

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

  private def appModeIndex(mode: AppMode): Int =
    mode match
      case AppMode.Code  => 0
      case AppMode.Prose => 1

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
