package com.serenity.command

import com.serenity.config.*

/** Builds the flat list of command-runner settings input items from the current config.
  *
  * `build` delegates each settings category to a sibling `CommandRunnerSettings*Items` object in this package --
  * split out to keep every file under the architecture size targets. The small text-parsing helpers below stay
  * here, `private[command]`, since they are shared across those siblings.
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

  def build(config: AppConfig): List[CommandSurfaceItem.InputItem] =
    val editorConfig        = config.editorConfig
    val inputConfig         = config.inputConfig
    val surfaceConfig       = config.surfaceConfig
    val interfaceConfig     = config.interfaceConfig
    val languageToolsConfig = config.languageToolsConfig

    val durationValue       = editorConfig.characterAnimation.map(_.durationMs.toString).getOrElse("0")
    val stepsValue          = editorConfig.characterAnimation.map(_.steps.toString).getOrElse("0")
    val blurValue           = surfaceConfig.blurRadius.toString
    val codeFontSizeValue   = editorConfig.fontConfig.codeFontSize.toString
    val textFontSizeValue   = editorConfig.fontConfig.textFontSize.toString
    val uiFontSizeValue     = editorConfig.fontConfig.uiFontSize.toString
    val textScaleValue      = f"${editorConfig.fontConfig.textScaleMultiplier}%.2f"
    val textAreaLeftValue   = f"${surfaceConfig.textAreaInsets.leftPercent}%.1f"
    val textAreaRightValue  = f"${surfaceConfig.textAreaInsets.rightPercent}%.1f"
    val textAreaTopValue    = f"${surfaceConfig.textAreaInsets.topPercent}%.1f"
    val textAreaBottomValue = f"${surfaceConfig.textAreaInsets.bottomPercent}%.1f"
    val speedScaleValue     = f"${surfaceConfig.elementTransitionSpeedScale}%.2f"
    val editorTextSpeedScaleValue =
      f"${config.effectiveEditorTextTransitionSpeedScale}%.2f"
    val commandRunnerSpeedScaleValue =
      f"${config.effectiveCommandRunnerTransitionSpeedScale}%.2f"
    val uiSpeedScaleValue         = f"${config.effectiveUiTransitionSpeedScale}%.2f"
    val cursorSpeedScaleValue     = f"${config.effectiveCursorTransitionSpeedScale}%.2f"
    val elementGapValue           = formatDecimal(interfaceConfig.elementGap)
    val cornerRadiusValue         = interfaceConfig.cornerRadiusPx.toString
    val outlineThicknessValue     = interfaceConfig.outlineThicknessPx.toString
    val commandRowsValue          = surfaceConfig.commandRunnerVisibleRows.map(_.toString).getOrElse("auto")
    val commandItemGapRowsValue   = formatDecimal(surfaceConfig.commandRunnerItemGapRows)
    val commandCursorGapRowsValue = surfaceConfig.commandRunnerCursorGapRows.map(formatDecimal).getOrElse("auto")
    val spellCheck                = languageToolsConfig.spellCheck.normalized
    val sitterConfig              = config.windowSitterConfig

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
        textAreaLeftValue,
        textAreaRightValue,
        textAreaTopValue,
        textAreaBottomValue
      ) ++
      CommandRunnerSettingsInputItemsTextAreaAndSpellCheck.spellCheckItems(spellCheck) ++
      CommandRunnerSettingsInputItemsMotion.animationTimingItems(
        durationValue,
        stepsValue,
        speedScaleValue,
        editorTextSpeedScaleValue
      ) ++
      CommandRunnerSettingsInputItemsMotion.speedScaleItems(
        commandRunnerSpeedScaleValue,
        uiSpeedScaleValue,
        cursorSpeedScaleValue,
        blurValue
      ) ++
      CommandRunnerSettingsInputItemsUiLayout.uiSpacingItems(
        elementGapValue,
        cornerRadiusValue,
        outlineThicknessValue
      ) ++
      CommandRunnerSettingsInputItemsUiLayout.commandRunnerLayoutItems(
        commandRowsValue,
        commandItemGapRowsValue,
        commandCursorGapRowsValue
      ) ++
      CommandRunnerSettingsInputItemsWindowSitterAndFont.windowSitterAndInputItems(
        sitterConfig,
        inputConfig.wheelScrollLines
      ) ++
      CommandRunnerSettingsInputItemsWindowSitterAndFont.fontSizeItems(
        codeFontSizeValue,
        textFontSizeValue,
        uiFontSizeValue,
        textScaleValue
      ) ++
      CommandRunnerSettingsKeymapItems.buildKeymapInputItems(inputConfig)

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
