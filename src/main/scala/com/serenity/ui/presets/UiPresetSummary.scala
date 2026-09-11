package com.serenity.ui.presets

import com.serenity.animation.TransitionKind
import com.serenity.config.*
import com.serenity.state.models.*

/** Preview/summary string generation used to display a [[UiPreset]] in the UI. */
private[presets] object UiPresetSummary:

  def previewHint(preset: UiPreset): String =
    List(
      Some(documentModeSummary(preset.config)),
      Option(preset.themeName).filter(_.nonEmpty),
      Some(s"${preset.config.surfaceConfig.motionPreset.configKey} motion"),
      Some(s"${textRevealSummary(preset.config.surfaceConfig.editorInsertionTransitionKind)} text reveal"),
      Some(s"${preset.config.surfaceConfig.materialPreset.configKey} material"),
      Some(s"${backgroundStyleSummary(preset.config.surfaceConfig.backgroundStyle)} background"),
      Some(s"${preset.config.interfaceDensity.configKey} density"),
      Some(proseFontSummary(preset.config)),
      paneCountSummary(preset.targetEditorPaneCount),
      panelSummary(preset.pinnedPanels)
    ).flatten.mkString("; ")

  private def documentModeSummary(config: AppConfig): String =
    config.defaultDocumentMode match
      case DefaultDocumentMode.RichText =>
        "rich text default"
      case DefaultDocumentMode.Markdown =>
        config.markdownViewMode match
          case MarkdownViewMode.Source       => "markdown source default"
          case MarkdownViewMode.SplitPreview => "markdown split preview"
          case MarkdownViewMode.InlineLens   => "markdown inline lens"
      case DefaultDocumentMode.PlainText =>
        "plain text default"

  private def proseFontSummary(config: AppConfig): String =
    s"${config.editorConfig.fontConfig.textFontFamily} ${formatPointSize(config.editorConfig.fontConfig.textFontSize)} prose"

  private def paneCountSummary(targetEditorPaneCount: Option[Int]): Option[String] =
    targetEditorPaneCount.collect {
      case 1     => "1 editor pane"
      case count => s"$count editor panes"
    }

  private def textRevealSummary(kind: TransitionKind): String =
    kind match
      case TransitionKind.Disabled               => "off"
      case TransitionKind.Fade                   => "fade"
      case TransitionKind.TypedText              => "typed"
      case TransitionKind.DirectionalSweep       => "directional"
      case TransitionKind.LineAndCharacterTandem => "tandem"
      case TransitionKind.OutlineThenContent     => "outline"

  private def backgroundStyleSummary(style: BackgroundStyle): String =
    style match
      case BackgroundStyle.Solid       => "solid"
      case BackgroundStyle.Transparent => "transparent"
      case BackgroundStyle.Frosted     => "frosted"
      case BackgroundStyle.GlassLike   => "glass"

  private def formatPointSize(size: Float): String =
    if size == size.round.toFloat then size.toInt.toString + "pt"
    else f"$size%.1fpt"

  private def panelSummary(panels: List[SessionPinnedPanel]): Option[String] =
    Option(panels.map(panel => s"${panel.position} ${panelContentName(panel.content)} ${panel.size}").mkString(", "))
      .filter(_.nonEmpty)

  private def panelContentName(content: SessionPanelContent): String =
    content match
      case SessionPanelContent.DirectoryTree(_, _, _) => "files"
      case SessionPanelContent.Terminal(_, _)         => "terminal"
      case SessionPanelContent.Outline(_)             => "outline"
      case SessionPanelContent.Comments(_)            => "comments"
      case SessionPanelContent.Diagnostics(_)         => "diagnostics"
      case SessionPanelContent.MarkdownPreview(_, _)  => "markdown preview"
      case SessionPanelContent.CompanionSprite        => "companion sprite"
