package com.serenity.ui.presets

import java.awt.Font

import com.serenity.animation.TransitionKind
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.{withEditorInsertionTransitionKind, withMotionPreset}
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme

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
        .withLineNumbers(false)
        .withGutter(false)
        .withPaneHeaders(false)
        .withMotionPreset(MotionPreset.Subtle)
        .withEditorInsertionTransitionKind(TransitionKind.TypedText)
        .withMaterialPreset(MaterialPreset.Frosted)
        .withDefaultDocumentMode(DefaultDocumentMode.RichText)
        .withInterfaceDensity(InterfaceDensity.Spacious)
        .withTextAreaInsets(TextAreaInsets.fromPercent(22.0, 22.0))
        .withFontConfig(
          AppConfig.default.editorConfig.fontConfig.copy(
            textFontFamily = Font.SERIF,
            textFontSize = 18.0f,
            uiFontSize = 13.0f
          )
        )
        .withCursorInfoBarSegments(List(CursorInfoBarSegment.Position)),
      themeName = Theme.dark.name,
      targetEditorPaneCount = Some(1)
    )

  private def documentationPreset: UiPreset =
    UiPreset(
      name = "Documentation",
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(false)
        .withPaneHeaders(false)
        .withMotionPreset(MotionPreset.Subtle)
        .withEditorInsertionTransitionKind(TransitionKind.LineAndCharacterTandem)
        .withMarkdownViewMode(MarkdownViewMode.SplitPreview)
        .withDefaultDocumentMode(DefaultDocumentMode.Markdown)
        .withFontConfig(
          AppConfig.default.editorConfig.fontConfig.copy(
            textFontFamily = Font.SANS_SERIF,
            textFontSize = 14.0f,
            fontSize = 13.0f
          )
        ),
      themeName = Theme.dark.name,
      targetEditorPaneCount = Some(1)
    )

  private def codePreset: UiPreset =
    UiPreset(
      name = "Code",
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(true)
        .withMotionPreset(MotionPreset.Reduced)
        .withEditorInsertionTransitionKind(TransitionKind.Disabled)
        .withInterfaceDensity(InterfaceDensity.Compact)
        .withSyntaxHighlighting(true)
        .withFontConfig(FontConfig()),
      themeName = Theme.dark.name,
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
        .withLineNumbers(true)
        .withGutter(true)
        .withPaneHeaders(true)
        .withWordWrap(false)
        .withContextualToolbarEnabled(false)
        .withMotionPreset(MotionPreset.Reduced)
        .withEditorInsertionTransitionKind(TransitionKind.Disabled)
        .withInterfaceDensity(InterfaceDensity.Compact)
        .withSyntaxHighlighting(true)
        .withFontConfig(FontConfig()),
      themeName = Theme.dark.name,
      targetEditorPaneCount = Some(1)
    )

  private def reviewPreset: UiPreset =
    UiPreset(
      name = "Review",
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(true)
        .withMotionPreset(MotionPreset.Reduced)
        .withEditorInsertionTransitionKind(TransitionKind.Disabled)
        .withInterfaceDensity(InterfaceDensity.Comfortable)
        .withCursorInfoBarSegments(List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title)),
      themeName = Theme.dark.name,
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
