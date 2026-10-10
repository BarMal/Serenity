package com.serenity.command

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
      "render-fps"                 -> renderFpsTargetIndex(surfaceConfig.renderFpsTarget),
      "render-damage-granularity"  -> renderDamageGranularityIndex(surfaceConfig.renderDamageGranularity),
      "cursor-mode"                -> cursorModeIndex(cursorConfig.mode),
      "status-placement"           -> statusPlacementIndex(config.statusLine.placement),
      "status-position"            -> enabledIndex(config.statusLine.segments.contains(StatusSegment.Position)),
      "status-title"               -> enabledIndex(config.statusLine.segments.contains(StatusSegment.Title)),
      "status-language"            -> enabledIndex(config.statusLine.segments.contains(StatusSegment.Language)),
      "status-mode"                -> enabledIndex(config.statusLine.segments.contains(StatusSegment.Mode)),
      "status-word-count"          -> enabledIndex(config.statusLine.segments.contains(StatusSegment.WordCount)),
      "status-char-count"          -> enabledIndex(config.statusLine.segments.contains(StatusSegment.CharCount)),
      "status-reading-time"        -> enabledIndex(config.statusLine.segments.contains(StatusSegment.ReadingTime)),
      "status-word-goal"           -> enabledIndex(config.statusLine.segments.contains(StatusSegment.WordGoal)),
      "status-line-ending"         -> enabledIndex(config.statusLine.segments.contains(StatusSegment.LineEnding)),
      "interface-density"          -> interfaceDensityIndex(interfaceConfig.density),
      "window-chrome"              -> windowChromeModeIndex(config.windowChromeMode),
      "markdown-view"              -> markdownViewModeIndex(documentConfig.markdownViewMode),
      "default-document-mode"      -> defaultDocumentModeIndex(documentConfig.defaultMode),
      "drop-caps-enabled"          -> enabledIndex(documentConfig.dropCapsEnabled),
      "auto-save-mode"             -> autoSaveModeIndex(config.autoSaveConfig.mode),
      "follow-system-theme"        -> enabledIndex(config.themeFollowConfig.followSystem),
      "spellcheck-enabled"         -> enabledIndex(languageToolsConfig.spellCheck.enabled),
      "app-mode"                   -> appModeIndex(config.appMode),
      "settings-show-all"          -> enabledIndex(config.showAllSettingsRegardlessOfMode),
      "panel-escape-code"          -> panelEscapeTargetIndex(config.inputConfig.panelEscapeReturnsTo.code),
      "panel-escape-prose"         -> panelEscapeTargetIndex(config.inputConfig.panelEscapeReturnsTo.prose),
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

  private def cursorModeIndex(mode: CursorMode): Int =
    mode match
      case CursorMode.Blink => 0

  private def statusPlacementIndex(placement: StatusLinePlacement): Int =
    placement match
      case StatusLinePlacement.Pinned   => 0
      case StatusLinePlacement.Floating => 1
      case StatusLinePlacement.Off      => 2

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

  private def markdownViewModeIndex(mode: MarkdownViewMode): Int =
    mode match
      case MarkdownViewMode.Source       => 0
      case MarkdownViewMode.SplitPreview => 1
      case MarkdownViewMode.InlineLens   => 2
      case MarkdownViewMode.LivePreview  => 3
      case MarkdownViewMode.Read         => 4

  private def autoSaveModeIndex(mode: AutoSaveMode): Int =
    mode match
      case AutoSaveMode.Off            => 0
      case AutoSaveMode.AfterDelay     => 1
      case AutoSaveMode.OnFocusChange  => 2
      case AutoSaveMode.OnWindowChange => 3

  private def defaultDocumentModeIndex(mode: DefaultDocumentMode): Int =
    mode match
      case DefaultDocumentMode.PlainText => 0
      case DefaultDocumentMode.Markdown  => 1
      case DefaultDocumentMode.RichText  => 2

  private def appModeIndex(mode: AppMode): Int =
    mode match
      case AppMode.Code  => 0
      case AppMode.Prose => 1

  private def panelEscapeTargetIndex(target: PanelEscapeTarget): Int =
    target match
      case PanelEscapeTarget.Editor   => 0
      case PanelEscapeTarget.Previous => 1

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
