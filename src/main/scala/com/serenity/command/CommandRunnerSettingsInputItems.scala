package com.serenity.command

import com.serenity.animation.WindowSitterConfig
import com.serenity.config.*

/** Builds the flat list of command-runner settings input items from the current config.
  *
  * `build` delegates each settings category to a sibling `CommandRunnerSettings*Items` object in this package -- split
  * out to keep every file under the architecture size targets. The small text-parsing helpers below stay here,
  * `private[command]`, since they are shared across those siblings.
  */
object CommandRunnerSettingsInputItems:

  def parseRichTextFontFamily(text: String): Option[CommandIntent] =
    nonEmptyText(text).map(commandIntentArg =>
      CommandIntent.RichText(RichTextIntent.SetRichTextFontFamily(commandIntentArg))
    )

  def parseRichTextColor(text: String): Option[CommandIntent] =
    normalizeHexColor(text).map(commandIntentArg =>
      CommandIntent.RichText(RichTextIntent.SetRichTextColor(commandIntentArg))
    )

  /** The config-derived display strings and sub-configs every settings-category builder below needs. Split out of
    * `build` to keep both under the architecture size targets.
    */
  final private case class DerivedValues(
      inputConfig: InputConfig,
      durationValue: String,
      stepsValue: String,
      blurValue: String,
      codeFontSizeValue: String,
      textFontSizeValue: String,
      uiFontSizeValue: String,
      textScaleValue: String,
      textAreaLeftValue: String,
      textAreaRightValue: String,
      textAreaTopValue: String,
      textAreaBottomValue: String,
      editorTextSpeedScaleValue: String,
      commandRunnerSpeedScaleValue: String,
      uiSpeedScaleValue: String,
      cursorSpeedScaleValue: String,
      speedScaleValue: String,
      elementGapValue: String,
      cornerRadiusValue: String,
      outlineThicknessValue: String,
      commandRowsValue: String,
      commandItemGapRowsValue: String,
      commandCursorGapRowsValue: String,
      spellCheck: SpellCheckConfig,
      sitterConfig: WindowSitterConfig
  )

  private def derivedValues(config: AppConfig): DerivedValues =
    val editorConfig        = config.editorConfig
    val surfaceConfig       = config.surfaceConfig
    val interfaceConfig     = config.interfaceConfig
    val languageToolsConfig = config.languageToolsConfig
    DerivedValues(
      inputConfig = config.inputConfig,
      durationValue = editorConfig.characterAnimation.map(_.durationMs.toString).getOrElse("0"),
      stepsValue = editorConfig.characterAnimation.map(_.steps.toString).getOrElse("0"),
      blurValue = surfaceConfig.blurRadius.toString,
      codeFontSizeValue = editorConfig.fontConfig.codeFontSize.toString,
      textFontSizeValue = editorConfig.fontConfig.textFontSize.toString,
      uiFontSizeValue = editorConfig.fontConfig.uiFontSize.toString,
      textScaleValue = f"${editorConfig.fontConfig.textScaleMultiplier}%.2f",
      textAreaLeftValue = f"${surfaceConfig.textAreaInsets.leftPercent}%.1f",
      textAreaRightValue = f"${surfaceConfig.textAreaInsets.rightPercent}%.1f",
      textAreaTopValue = f"${surfaceConfig.textAreaInsets.topPercent}%.1f",
      textAreaBottomValue = f"${surfaceConfig.textAreaInsets.bottomPercent}%.1f",
      editorTextSpeedScaleValue = f"${config.effectiveEditorTextTransitionSpeedScale}%.2f",
      commandRunnerSpeedScaleValue = f"${config.effectiveCommandRunnerTransitionSpeedScale}%.2f",
      uiSpeedScaleValue = f"${config.effectiveUiTransitionSpeedScale}%.2f",
      cursorSpeedScaleValue = f"${config.effectiveCursorTransitionSpeedScale}%.2f",
      speedScaleValue = f"${surfaceConfig.elementTransitionSpeedScale}%.2f",
      elementGapValue = formatDecimal(interfaceConfig.elementGap),
      cornerRadiusValue = interfaceConfig.cornerRadiusPx.toString,
      outlineThicknessValue = interfaceConfig.outlineThicknessPx.toString,
      commandRowsValue = surfaceConfig.commandRunnerVisibleRows.map(_.toString).getOrElse("auto"),
      commandItemGapRowsValue = formatDecimal(surfaceConfig.commandRunnerItemGapRows),
      commandCursorGapRowsValue = surfaceConfig.commandRunnerCursorGapRows.map(formatDecimal).getOrElse("auto"),
      spellCheck = languageToolsConfig.spellCheck.normalized,
      sitterConfig = config.windowSitterConfig
    )

  def build(config: AppConfig): List[CommandSurfaceItem.InputItem] =
    val v = derivedValues(config)

    val commentItems = List(
      CommandSurfaceItem.InputItem(
        id = "document-comment",
        label = "Document Comment",
        hint = "Comment text",
        currentValue = "",
        isDecimal = false,
        parse = text =>
          nonEmptyText(text).map(commandIntentArg =>
            CommandIntent.Comments(CommentsIntent.AddDocumentComment(commandIntentArg))
          ),
        category = CommandCategory.Edit,
        acceptsFreeText = true
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
      CommandRunnerSettingsInputItemsTextAreaAndSpellCheck.spellCheckItems(v.spellCheck) ++
      CommandRunnerSettingsInputItemsMotion.animationTimingItems(
        v.durationValue,
        v.stepsValue,
        v.speedScaleValue,
        v.editorTextSpeedScaleValue
      ) ++
      CommandRunnerSettingsInputItemsMotion.speedScaleItems(
        v.commandRunnerSpeedScaleValue,
        v.uiSpeedScaleValue,
        v.cursorSpeedScaleValue,
        v.blurValue
      ) ++
      CommandRunnerSettingsInputItemsUiLayout.uiSpacingItems(
        v.elementGapValue,
        v.cornerRadiusValue,
        v.outlineThicknessValue
      ) ++
      CommandRunnerSettingsInputItemsUiLayout.commandRunnerLayoutItems(
        v.commandRowsValue,
        v.commandItemGapRowsValue,
        v.commandCursorGapRowsValue
      ) ++
      CommandRunnerSettingsInputItemsWindowSitterAndFont.windowSitterAndInputItems(
        v.sitterConfig,
        v.inputConfig.wheelScrollLines
      ) ++
      CommandRunnerSettingsInputItemsWindowSitterAndFont.fontSizeItems(
        v.codeFontSizeValue,
        v.textFontSizeValue,
        v.uiFontSizeValue,
        v.textScaleValue
      ) ++
      CommandRunnerSettingsKeymapItems.buildKeymapInputItems(v.inputConfig)

  private[command] def nonEmptyText(text: String): Option[String] =
    Option(text.trim).filter(_.nonEmpty)

  private[command] def namedPair(text: String): Option[(String, String)] =
    text.split("->", 2).toList match
      case source :: target :: Nil =>
        for
          normalizedSource <- nonEmptyText(source)
          normalizedTarget <- nonEmptyText(target)
        yield (normalizedSource, normalizedTarget)
      case _ =>
        None

  private[command] def normalizeHexColor(text: String): Option[String] =
    val normalized = text.trim.stripPrefix("#")
    Option
      .when(normalized.length == 6 && normalized.forall(isHexDigit))("#" + normalized.toLowerCase)

  private[command] def isHexDigit(char: Char): Boolean =
    char.isDigit ||
      (char >= 'a' && char <= 'f') ||
      (char >= 'A' && char <= 'F')

  private[command] def nonEmptyCommaList(text: String): Option[List[String]] =
    Option(commaList(text)).filter(_.nonEmpty)

  private[command] def commaList(text: String): List[String] =
    text
      .split(",")
      .toList
      .map(_.trim.toLowerCase)
      .filter(_.nonEmpty)
      .distinct

  private[command] def commaListPreserveCase(text: String): List[String] =
    text
      .split(",")
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinct

  private[command] def formatDecimal(value: Double): String =
    if value.isWhole then value.toLong.toString else value.toString
