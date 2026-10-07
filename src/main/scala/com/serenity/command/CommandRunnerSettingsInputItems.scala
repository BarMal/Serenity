package com.serenity.command

import com.serenity.config.*
import com.serenity.frontend.FrontendCapabilities

/** Builds the flat list of command-runner settings input items from the current config.
  *
  * `build` delegates each settings category to a sibling `CommandRunnerSettings*Items` object in this package -- split
  * out to keep every file under the architecture size targets. [[CommandRunnerSettingsTextParsing]] holds the small
  * text-parsing helpers shared across this object and those siblings alike.
  */
object CommandRunnerSettingsInputItems:

  def parseRichTextFontFamily(text: String): Option[CommandIntent] =
    CommandRunnerSettingsTextParsing
      .nonEmptyText(text)
      .map(commandIntentArg => CommandIntent.RichText(RichTextIntent.SetRichTextFontFamily(commandIntentArg)))

  def parseRichTextColor(text: String): Option[CommandIntent] =
    normalizeHexColor(text).map(commandIntentArg =>
      CommandIntent.RichText(RichTextIntent.SetRichTextColor(commandIntentArg))
    )

  /** The config-derived display strings and sub-configs every settings-category builder below needs. Split out of
    * `build` to keep both under the architecture size targets.
    */
  final private case class DerivedValues(
      inputConfig: InputConfig,
      codeFontSizeValue: String,
      textFontSizeValue: String,
      uiFontSizeValue: String,
      textScaleValue: String,
      textAreaLeftValue: String,
      textAreaRightValue: String,
      textAreaTopValue: String,
      textAreaBottomValue: String,
      elementGapValue: String,
      outlineThicknessValue: String,
      lineNumberMarginLeftValue: String,
      lineNumberMarginRightValue: String,
      lineNumberPaddingValue: String,
      columnCountValue: String,
      columnGapValue: String,
      spellCheck: SpellCheckConfig
  )

  private def derivedValues(config: AppConfig, capabilities: FrontendCapabilities): DerivedValues =
    val editorConfig        = config.editorConfig
    val surfaceConfig       = config.surfaceConfig
    val interfaceConfig     = config.interfaceConfig
    val languageToolsConfig = config.languageToolsConfig
    DerivedValues(
      inputConfig = config.inputConfig,
      codeFontSizeValue = editorConfig.fontConfig.codeFontSize.toString,
      textFontSizeValue = editorConfig.fontConfig.textFontSize.toString,
      uiFontSizeValue = editorConfig.fontConfig.uiFontSize.toString,
      textScaleValue = f"${editorConfig.fontConfig.textScaleMultiplier}%.2f",
      textAreaLeftValue = f"${surfaceConfig.textAreaInsets.leftPercent}%.1f",
      textAreaRightValue = f"${surfaceConfig.textAreaInsets.rightPercent}%.1f",
      textAreaTopValue = f"${surfaceConfig.textAreaInsets.topPercent}%.1f",
      textAreaBottomValue = f"${surfaceConfig.textAreaInsets.bottomPercent}%.1f",
      // Unset resolves to the same surface-specific number `AppState.effectiveUiElementGap` and its
      // `effectiveLineNumberMarginLeft`/`effectiveLineNumberPadding` siblings would show -- a GUI cell of breathing
      // room, or the TUI's existing zero -- rather than the literal string "auto" (issue #1621 carve-out).
      elementGapValue =
        interfaceConfig.elementGap.fold(formatDecimal(if capabilities.isCellGrid then 0.0 else 1.0))(formatDecimal),
      outlineThicknessValue = interfaceConfig.outlineThicknessPx.toString,
      lineNumberMarginLeftValue =
        surfaceConfig.lineNumberLayout.marginLeft.fold(if capabilities.isCellGrid then "0" else "1")(_.toString),
      lineNumberMarginRightValue = surfaceConfig.lineNumberLayout.marginRight.toString,
      lineNumberPaddingValue =
        surfaceConfig.lineNumberLayout.padding.fold(if capabilities.isCellGrid then "0" else "1")(_.toString),
      columnCountValue = surfaceConfig.columnCount.fold("auto")(_.toString),
      columnGapValue = surfaceConfig.columnGap.toString,
      spellCheck = languageToolsConfig.spellCheck.normalized
    )

  def build(
    config: AppConfig,
    capabilities: FrontendCapabilities = FrontendCapabilities.gui
  ): List[CommandSurfaceItem.InputItem] =
    val v = derivedValues(config, capabilities)

    val commentItems = List(
      CommandSurfaceItem.InputItem(
        id = "document-comment",
        label = "Document Comment",
        hint = "Comment text",
        currentValue = "",
        kind = CommandSurfaceItem.InputKind.FreeText,
        parse = text =>
          CommandRunnerSettingsTextParsing
            .nonEmptyText(text)
            .map(commandIntentArg => CommandIntent.Comments(CommentsIntent.AddDocumentComment(commandIntentArg))),
        category = CommandCategory.Edit
      ),
      CommandSurfaceItem.InputItem(
        id = "reply-document-comment",
        label = "Reply to Document Comment",
        hint = "Reply text",
        currentValue = "",
        kind = CommandSurfaceItem.InputKind.FreeText,
        parse = text =>
          CommandRunnerSettingsTextParsing
            .nonEmptyText(text)
            .map(commandIntentArg => CommandIntent.Comments(CommentsIntent.ReplyToDocumentComment(commandIntentArg))),
        category = CommandCategory.Edit
      ),
      CommandSurfaceItem.InputItem(
        id = "add-placeholder",
        label = "Add Placeholder",
        hint = "Note for the placeholder",
        currentValue = "",
        kind = CommandSurfaceItem.InputKind.FreeText,
        parse = text =>
          CommandRunnerSettingsTextParsing
            .nonEmptyText(text)
            .map(commandIntentArg => CommandIntent.Placeholders(PlaceholderIntent.AddPlaceholder(commandIntentArg))),
        category = CommandCategory.Edit
      ),
      CommandSurfaceItem.InputItem(
        id = "word-goal",
        label = "Word Goal",
        hint = "Target word count, or \"auto\"/blank to clear",
        currentValue = config.documentConfig.wordGoal.fold("")(_.toString),
        kind = CommandSurfaceItem.InputKind.FreeText,
        parse = text =>
          text.trim.toLowerCase match
            case "" | "auto" | "off" | "none" =>
              Some(CommandIntent.Settings(SettingsIntent.StatusLine(StatusLineIntent.SetWordGoal(None))))
            case trimmed =>
              trimmed.toIntOption
                .filter(_ > 0)
                .map(goal =>
                  CommandIntent.Settings(SettingsIntent.StatusLine(StatusLineIntent.SetWordGoal(Some(goal))))
                ),
        category = CommandCategory.Settings
      )
    )

    commentItems ++
      CommandRunnerSettingsInputItemsPresets.presetCreateItems ++
      CommandRunnerSettingsInputItemsPresets.presetManageItems ++
      CommandRunnerSettingsInputItemsRichText.richTextItems ++
      CommandRunnerSettingsInputItemsTextAreaAndSpellCheck.textAreaItems(
        v.textAreaLeftValue,
        v.textAreaRightValue,
        v.textAreaTopValue,
        v.textAreaBottomValue
      ) ++
      CommandRunnerSettingsInputItemsTextAreaAndSpellCheck.columnItems(v.columnCountValue, v.columnGapValue) ++
      CommandRunnerSettingsInputItemsTextAreaAndSpellCheck.spellCheckItems(v.spellCheck) ++
      CommandRunnerSettingsInputItemsUiLayout.uiSpacingItems(
        v.elementGapValue,
        v.outlineThicknessValue
      ) ++
      CommandRunnerSettingsInputItemsUiLayout.lineNumberSpacingItems(
        v.lineNumberMarginLeftValue,
        v.lineNumberMarginRightValue,
        v.lineNumberPaddingValue
      ) ++
      CommandRunnerSettingsInputItemsInputAndFont.inputItems(v.inputConfig.wheelScrollLines) ++
      CommandRunnerSettingsInputItemsInputAndFont.fontSizeItems(
        v.codeFontSizeValue,
        v.textFontSizeValue,
        v.uiFontSizeValue,
        v.textScaleValue
      ) ++
      CommandRunnerSettingsKeymapItems.buildKeymapInputItems(v.inputConfig)

  private[command] def normalizeHexColor(text: String): Option[String] =
    val normalized = text.trim.stripPrefix("#")
    Option
      .when(normalized.length == 6 && normalized.forall(isHexDigit))("#" + normalized.toLowerCase)

  private[command] def isHexDigit(char: Char): Boolean =
    char.isDigit ||
      (char >= 'a' && char <= 'f') ||
      (char >= 'A' && char <= 'F')

  private[command] def formatDecimal(value: Double): String =
    if value.isWhole then value.toLong.toString else value.toString
