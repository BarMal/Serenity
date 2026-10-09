package com.serenity.ui.presets

import java.awt.Font

import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*

/** The five concrete built-in `UiPreset` definitions and their name-based lookup. */
private[presets] object BuiltInUiPresets:

  val builtIns: List[UiPreset] =
    List(writingPreset, documentationPreset, codePreset, compactPreset, reviewPreset)

  def builtInNames: List[String] =
    builtIns.map(_.name)

  def builtIn(name: String): Option[UiPreset] =
    builtIns.find(_.name.equalsIgnoreCase(name.trim))

  private def writingPreset: UiPreset =
    UiPreset(
      name = "Writing",
      config = AppConfig.default
        .withAppMode(AppMode.Prose)
        .withLineNumbers(false)
        .withStatusLine(
          StatusLineConfig(List(StatusSegment.WordCount, StatusSegment.WordGoal), StatusLinePlacement.Floating)
        )
        .withPaneHeaders(false)
        .withDefaultDocumentMode(DefaultDocumentMode.RichText)
        .withInterfaceDensity(InterfaceDensity.Spacious)
        .withProseMeasure(Some(ProseMeasure.Default))
        .withSmartPunctuation(true)
        .withSpellCheck(AppConfig.default.languageToolsConfig.spellCheck.copy(enabled = true))
        .withTypewriterScrolling(true)
        .withFocusedTextBody(true)
        .withFontConfig(
          AppConfig.default.editorConfig.fontConfig.copy(
            textFontFamily = Font.SERIF,
            textFontSize = 18.0f,
            uiFontSize = 13.0f
          )
        ),
      targetEditorPaneCount = Some(1)
    )

  private def documentationPreset: UiPreset =
    UiPreset(
      name = "Documentation",
      config = AppConfig.default
        .withAppMode(AppMode.Prose)
        .withLineNumbers(true)
        .withoutStatusLine
        .withPaneHeaders(false)
        .withMarkdownViewMode(MarkdownViewMode.SplitPreview)
        .withDefaultDocumentMode(DefaultDocumentMode.Markdown)
        .withFontConfig(
          AppConfig.default.editorConfig.fontConfig.copy(
            textFontFamily = Font.SANS_SERIF,
            textFontSize = 14.0f,
            fontSize = 13.0f
          )
        ),
      targetEditorPaneCount = Some(1)
    )

  private def codePreset: UiPreset =
    UiPreset(
      name = "Code",
      config = AppConfig.default
        .withAppMode(AppMode.Code)
        .withLineNumbers(true)
        .withInterfaceDensity(InterfaceDensity.Compact)
        .withSyntaxHighlighting(true)
        .withFontConfig(FontConfig()),
      dockedPanels = List(
        SessionDockedPanel(
          "code-directory-tree",
          SessionPinnedPanel(
            PanelPosition.Left,
            32,
            SessionPanelContent.DirectoryTree(".", selectedPath = None, expandedPaths = Nil)
          )
        )
      )
    )

  private def compactPreset: UiPreset =
    UiPreset(
      name = "Compact",
      config = AppConfig.default
        .withAppMode(AppMode.Code)
        .withLineNumbers(true)
        .withPaneHeaders(true)
        .withWordWrap(false)
        .withContextualToolbarEnabled(false)
        .withInterfaceDensity(InterfaceDensity.Compact)
        .withSyntaxHighlighting(true)
        .withFontConfig(FontConfig()),
      targetEditorPaneCount = Some(1)
    )

  private def reviewPreset: UiPreset =
    UiPreset(
      name = "Review",
      config = AppConfig.default
        .withAppMode(AppMode.Code)
        .withLineNumbers(true)
        .withInterfaceDensity(InterfaceDensity.Comfortable)
        .withStatusLineSegments(List(StatusSegment.Position, StatusSegment.Title, StatusSegment.Mode)),
      dockedPanels = List(
        SessionDockedPanel(
          "review-outline",
          SessionPinnedPanel(PanelPosition.Left, 30, SessionPanelContent.Outline(Nil))
        ),
        SessionDockedPanel(
          "review-diagnostics",
          SessionPinnedPanel(PanelPosition.Bottom, 10, SessionPanelContent.Diagnostics(Nil))
        )
      )
    )
