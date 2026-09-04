package com.serenity.command

import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader

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

    val richTextItems = List(
      CommandSurfaceItem.InputItem(
        id = "rich-text-font-family",
        label = "Selection Font Family",
        hint = "Family name",
        currentValue = "",
        isDecimal = false,
        parse = parseRichTextFontFamily,
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "rich-text-font-size",
        label = "Selection Font Size",
        hint = "Points (1.0-144.0)",
        currentValue = "",
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 1.0f && v <= 144.0f)
            .map(commandIntentArg => CommandIntent.RichText(RichTextIntent.SetRichTextFontSize(commandIntentArg))),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "rich-text-color",
        label = "Selection Text Colour",
        hint = "#RRGGBB",
        currentValue = "",
        isDecimal = false,
        parse = parseRichTextColor,
        category = CommandCategory.Settings,
        acceptsFreeText = true
      )
    )

    val presetItems = List(
      CommandSurfaceItem.InputItem(
        id = "ui-preset-save-as-new",
        label = "Save As New Preset",
        hint = "New preset name",
        currentValue = "",
        isDecimal = false,
        parse = text =>
          nonEmptyText(text).map(commandIntentArg =>
            CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew(commandIntentArg))
          ),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-apply",
        label = "Apply Preset",
        hint = "Preset name",
        currentValue = "",
        isDecimal = false,
        parse = text =>
          nonEmptyText(text).map(commandIntentArg =>
            CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(commandIntentArg))
          ),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-overwrite",
        label = "Overwrite Preset",
        hint = "Existing custom preset name",
        currentValue = "",
        isDecimal = false,
        parse = text =>
          nonEmptyText(text).map(commandIntentArg =>
            CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset(commandIntentArg))
          ),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-duplicate",
        label = "Duplicate Preset",
        hint = "Source -> Copy",
        currentValue = "",
        isDecimal = false,
        parse = text =>
          namedPair(text).map {
            case (sourceName, targetName) =>
              CommandIntent.UiPresets(UiPresetsIntent.DuplicateUiPreset(sourceName, targetName))
          },
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-rename",
        label = "Rename Preset",
        hint = "Current -> New",
        currentValue = "",
        isDecimal = false,
        parse = text =>
          namedPair(text).map {
            case (sourceName, targetName) =>
              CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset(sourceName, targetName))
          },
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-delete",
        label = "Delete Preset",
        hint = "Preset name",
        currentValue = "",
        isDecimal = false,
        parse = text =>
          nonEmptyText(text).map(commandIntentArg =>
            CommandIntent.UiPresets(UiPresetsIntent.DeleteUiPreset(commandIntentArg))
          ),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-reset",
        label = "Reset Preset",
        hint = "Built-in preset name",
        currentValue = "",
        isDecimal = false,
        parse = text =>
          nonEmptyText(text).map(commandIntentArg =>
            CommandIntent.UiPresets(UiPresetsIntent.ResetUiPreset(commandIntentArg))
          ),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      )
    )

    val textAreaItems = List(
      CommandSurfaceItem.InputItem(
        id = "text-area-left",
        label = "Left Text Margin",
        hint = "Percent (0-45)",
        currentValue = textAreaLeftValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value => value >= 0.0 && value <= 45.0)
            .map(value =>
              CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetTextAreaLeftInset(value / 100.0)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "text-area-right",
        label = "Right Text Margin",
        hint = "Percent (0-45)",
        currentValue = textAreaRightValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value => value >= 0.0 && value <= 45.0)
            .map(value =>
              CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetTextAreaRightInset(value / 100.0)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "text-area-top",
        label = "Top Text Margin",
        hint = "Percent (0-45)",
        currentValue = textAreaTopValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value => value >= 0.0 && value <= 45.0)
            .map(value =>
              CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetTextAreaTopInset(value / 100.0)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "text-area-bottom",
        label = "Bottom Text Margin",
        hint = "Percent (0-45)",
        currentValue = textAreaBottomValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value => value >= 0.0 && value <= 45.0)
            .map(value =>
              CommandIntent
                .Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetTextAreaBottomInset(value / 100.0)))
            ),
        category = CommandCategory.Settings
      )
    )

    val spellCheckItems = List(
      CommandSurfaceItem.InputItem(
        id = "spellcheck-languages",
        label = "Spell Check Languages",
        hint = "Comma-separated codes",
        currentValue = spellCheck.languages.mkString(","),
        isDecimal = false,
        parse = text =>
          nonEmptyCommaList(text).map(commandIntentArg =>
            CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckLanguages(commandIntentArg)))
          ),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "spellcheck-dictionaries",
        label = "Spell Check Dictionaries",
        hint = "Comma-separated .dic paths",
        currentValue = spellCheck.dictionaryPaths.mkString(","),
        isDecimal = false,
        parse = text =>
          Some(
            CommandIntent.Settings(
              SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckDictionaryPaths(commaListPreserveCase(text)))
            )
          ),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "spellcheck-words",
        label = "Accepted Words",
        hint = "Comma-separated words",
        currentValue = spellCheck.additionalWords.mkString(","),
        isDecimal = false,
        parse = text =>
          Some(CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckWords(commaList(text))))),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      )
    )

    val numericItems = List(
      CommandSurfaceItem.InputItem(
        id = "animation-duration",
        label = "Animation Duration",
        hint = "Milliseconds (0-10000)",
        currentValue = durationValue,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(v => v >= 0 && v <= 10000)
            .map(commandIntentArg =>
              CommandIntent
                .Settings(SettingsIntent.General(GeneralSettingsIntent.SetAnimationDuration(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "animation-steps",
        label = "Animation Steps",
        hint = "Steps (0-100)",
        currentValue = stepsValue,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(v => v >= 0 && v <= 100)
            .map(commandIntentArg =>
              CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAnimationSteps(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "element-transition-speed-scale",
        label = "Motion Speed Scale",
        hint = "Scale (0.0-4.0)",
        currentValue = speedScaleValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value =>
              value >= AppConfig.MinElementTransitionSpeedScale &&
                value <= AppConfig.MaxElementTransitionSpeedScale
            )
            .map(commandIntentArg =>
              CommandIntent
                .Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "editor-text-speed-scale",
        label = "Editor Text Speed",
        hint = "Editor text scale (0.0-4.0)",
        currentValue = editorTextSpeedScaleValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value =>
              value >= AppConfig.MinElementTransitionSpeedScale &&
                value <= AppConfig.MaxElementTransitionSpeedScale
            )
            .map(commandIntentArg =>
              CommandIntent
                .Settings(SettingsIntent.Motion(MotionIntent.SetEditorTextTransitionSpeedScale(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "command-runner-speed-scale",
        label = "Command Runner Speed",
        hint = "Command runner scale (0.0-4.0)",
        currentValue = commandRunnerSpeedScaleValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value =>
              value >= AppConfig.MinElementTransitionSpeedScale &&
                value <= AppConfig.MaxElementTransitionSpeedScale
            )
            .map(commandIntentArg =>
              CommandIntent
                .Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerTransitionSpeedScale(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-speed-scale",
        label = "Panel/UI Speed",
        hint = "Panel/UI scale (0.0-4.0)",
        currentValue = uiSpeedScaleValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value =>
              value >= AppConfig.MinElementTransitionSpeedScale &&
                value <= AppConfig.MaxElementTransitionSpeedScale
            )
            .map(commandIntentArg =>
              CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetUiTransitionSpeedScale(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "cursor-speed-scale",
        label = "Cursor Speed",
        hint = "Cursor scale (0.0-4.0)",
        currentValue = cursorSpeedScaleValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value =>
              value >= AppConfig.MinElementTransitionSpeedScale &&
                value <= AppConfig.MaxElementTransitionSpeedScale
            )
            .map(commandIntentArg =>
              CommandIntent
                .Settings(SettingsIntent.Motion(MotionIntent.SetCursorTransitionSpeedScale(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "blur-radius",
        label = "Blur Radius",
        hint = "Strength (0.0-1.0)",
        currentValue = blurValue,
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 0.0f && v <= 1.0f)
            .map(commandIntentArg =>
              CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetBlurRadius(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-element-gap",
        label = "UI Element Gap",
        hint = s"Cells, decimals supported (${AppConfig.MinUiElementGap}-${AppConfig.MaxUiElementGap})",
        currentValue = elementGapValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value => value >= AppConfig.MinUiElementGap && value <= AppConfig.MaxUiElementGap)
            .map(commandIntentArg =>
              CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiElementGap(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-corner-radius",
        label = "UI Corner Radius",
        hint = s"Pixels (${AppConfig.MinUiCornerRadiusPx}-${AppConfig.MaxUiCornerRadiusPx})",
        currentValue = cornerRadiusValue,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(value => value >= AppConfig.MinUiCornerRadiusPx && value <= AppConfig.MaxUiCornerRadiusPx)
            .map(commandIntentArg =>
              CommandIntent
                .Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiCornerRadiusPx(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-outline-thickness",
        label = "UI Outline Thickness",
        hint = s"Pixels (${AppConfig.MinUiOutlineThicknessPx}-${AppConfig.MaxUiOutlineThicknessPx})",
        currentValue = outlineThicknessValue,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(value => value >= AppConfig.MinUiOutlineThicknessPx && value <= AppConfig.MaxUiOutlineThicknessPx)
            .map(commandIntentArg =>
              CommandIntent
                .Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiOutlineThicknessPx(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "command-runner-visible-rows",
        label = "Visible Commands",
        hint =
          s"Command rows (${AppConfig.MinCommandRunnerVisibleRows}-${AppConfig.MaxCommandRunnerVisibleRows}) or auto",
        currentValue = commandRowsValue,
        isDecimal = false,
        parse = text =>
          val normalized = text.trim.toLowerCase
          if normalized == "auto" then
            Some(CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerVisibleRows(None))))
          else
            normalized.toIntOption
              .filter(value =>
                value >= AppConfig.MinCommandRunnerVisibleRows &&
                  value <= AppConfig.MaxCommandRunnerVisibleRows
              )
              .map(value =>
                CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerVisibleRows(Some(value))))
              )
        ,
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "command-runner-item-gap-rows",
        label = "Command Item Spacing",
        hint =
          s"Rows, decimals supported (${AppConfig.MinCommandRunnerItemGapRows}-${AppConfig.MaxCommandRunnerItemGapRows})",
        currentValue = commandItemGapRowsValue,
        isDecimal = true,
        parse = text =>
          text.trim.toDoubleOption
            .filter(value =>
              value >= AppConfig.MinCommandRunnerItemGapRows &&
                value <= AppConfig.MaxCommandRunnerItemGapRows
            )
            .map(commandIntentArg =>
              CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerItemGapRows(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "command-runner-cursor-gap-rows",
        label = "Command Cursor Spacing",
        hint =
          s"Rows, decimals supported (${AppConfig.MinCommandRunnerCursorGapRows}-${AppConfig.MaxCommandRunnerCursorGapRows}) or auto",
        currentValue = commandCursorGapRowsValue,
        isDecimal = true,
        parse = text =>
          val normalized = text.trim.toLowerCase
          if normalized == "auto" then
            Some(CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerCursorGapRows(None))))
          else
            normalized.toDoubleOption
              .filter(value =>
                value >= AppConfig.MinCommandRunnerCursorGapRows &&
                  value <= AppConfig.MaxCommandRunnerCursorGapRows
              )
              .map(value =>
                CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerCursorGapRows(Some(value))))
              )
        ,
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "window-sitter-frames",
        label = "Sitter Frames",
        hint = "Comma-separated glyphs",
        currentValue = sitterConfig.frames.mkString(","),
        isDecimal = false,
        parse = text =>
          nonEmptyCommaList(text).map(values =>
            CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterFrames(values.toVector)))
          ),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "window-sitter-active-ticks",
        label = "Sitter Duration",
        hint = "Animation ticks (1-120)",
        currentValue = sitterConfig.activeTicks.toString,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(value => value >= 1 && value <= 120)
            .map(commandIntentArg =>
              CommandIntent
                .Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterActiveTicks(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "window-sitter-fast-active-ticks",
        label = "Fast Sitter Duration",
        hint = "Fast-typing ticks (1-240)",
        currentValue = sitterConfig.fastActiveTicks.toString,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(value => value >= 1 && value <= 240)
            .map(commandIntentArg =>
              CommandIntent.Settings(
                SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterFastActiveTicks(commandIntentArg))
              )
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "window-sitter-fast-threshold-ms",
        label = "Fast Typing Threshold",
        hint = "Milliseconds (1-5000)",
        currentValue = sitterConfig.fastTypingThresholdMs.toString,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(value => value >= 1 && value <= 5000)
            .map(commandIntentArg =>
              CommandIntent.Settings(
                SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterFastTypingThresholdMs(commandIntentArg))
              )
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "wheel-scroll-lines",
        label = "Wheel Scroll Lines",
        hint = "Lines per mouse-wheel notch (1-50)",
        currentValue = inputConfig.wheelScrollLines.toString,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(value => value >= 1 && value <= 50)
            .map(commandIntentArg =>
              CommandIntent
                .Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWheelScrollLines(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "code-font-size",
        label = "Code Font Size",
        hint = "Points (8.0-48.0)",
        currentValue = codeFontSizeValue,
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 8.0f && v <= 48.0f)
            .map(commandIntentArg =>
              CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeFontSize(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "text-font-size",
        label = "Prose Font Size",
        hint = "Points (8.0-48.0)",
        currentValue = textFontSizeValue,
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 8.0f && v <= 48.0f)
            .map(commandIntentArg =>
              CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextFontSize(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-font-size",
        label = "UI Font Size",
        hint = "Points (8.0-48.0)",
        currentValue = uiFontSizeValue,
        isDecimal = true,
        parse = text =>
          text.toFloatOption
            .filter(v => v >= 8.0f && v <= 48.0f)
            .map(commandIntentArg =>
              CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetUiFontSize(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "text-scale",
        label = "Text Scale",
        hint = s"Multiplier (${FontLoader.FontConfig.MinTextScale}-${FontLoader.FontConfig.MaxTextScale})",
        currentValue = textScaleValue,
        isDecimal = true,
        parse = text =>
          text.toDoubleOption
            .filter(value =>
              value >= FontLoader.FontConfig.MinTextScale &&
                value <= FontLoader.FontConfig.MaxTextScale
            )
            .map(commandIntentArg =>
              CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextScaleMultiplier(commandIntentArg)))
            ),
        category = CommandCategory.Settings
      )
    )

    commentItems ++ presetItems ++ richTextItems ++ textAreaItems ++ spellCheckItems ++ numericItems ++
      buildKeymapInputItems(inputConfig)

  private def nonEmptyText(text: String): Option[String] =
    Option(text.trim).filter(_.nonEmpty)

  private def namedPair(text: String): Option[(String, String)] =
    text.split("->", 2).toList match
      case source :: target :: Nil =>
        for
          normalizedSource <- nonEmptyText(source)
          normalizedTarget <- nonEmptyText(target)
        yield (normalizedSource, normalizedTarget)
      case _ =>
        None

  private def normalizeHexColor(text: String): Option[String] =
    val normalized = text.trim.stripPrefix("#")
    Option
      .when(normalized.length == 6 && normalized.forall(isHexDigit))("#" + normalized.toLowerCase)

  private def isHexDigit(char: Char): Boolean =
    char.isDigit ||
      (char >= 'a' && char <= 'f') ||
      (char >= 'A' && char <= 'F')

  private def nonEmptyCommaList(text: String): Option[List[String]] =
    Option(commaList(text)).filter(_.nonEmpty)

  private def commaList(text: String): List[String] =
    text
      .split(",")
      .toList
      .map(_.trim.toLowerCase)
      .filter(_.nonEmpty)
      .distinct

  private def commaListPreserveCase(text: String): List[String] =
    text
      .split(",")
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinct

  private def buildKeymapInputItems(config: InputConfig): List[CommandSurfaceItem.InputItem] =
    val globalActions = List(HotkeyAction.ToggleCommandRunner, HotkeyAction.FileSearch) ++
      HotkeyAction.values.toList.filterNot(action =>
        action == HotkeyAction.ToggleCommandRunner || action == HotkeyAction.FileSearch
      )
    val items = globalActions.map(action =>
      bindingInputItem(
        s"keymap-global-${action.configKey}",
        if action == HotkeyAction.OpenFile then "Open Document" else keymapLabel(action.configKey),
        config.hotkeyConfig.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetGlobalHotkey(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetGlobalHotkey(action))
      )
    ) ++ EditorKeyAction.values.toList.map(action =>
      bindingInputItem(
        s"keymap-editor-${action.configKey}",
        keymapLabel(action.configKey),
        config.focusedKeymapConfig.editor.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetEditorKeyBinding(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetEditorKeyBinding(action))
      )
    ) ++ CommandRunnerKeyAction.values.toList.map(action =>
      bindingInputItem(
        s"keymap-command-runner-${action.configKey}",
        keymapLabel(action.configKey),
        config.focusedKeymapConfig.commandRunner.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetCommandRunnerKeyBinding(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetCommandRunnerKeyBinding(action))
      )
    ) ++ ModalKeyAction.values.toList.map(action =>
      bindingInputItem(
        s"keymap-modal-${action.configKey}",
        keymapLabel(action.configKey),
        config.focusedKeymapConfig.modal.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetModalKeyBinding(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetModalKeyBinding(action))
      )
    ) ++ PanelKeyAction.values.toList.map(action =>
      bindingInputItem(
        s"keymap-panel-${action.configKey}",
        keymapLabel(action.configKey),
        config.focusedKeymapConfig.panel.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetPanelKeyBinding(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetPanelKeyBinding(action))
      )
    ) ++ PeekKeyAction.values.toList.map(action =>
      bindingInputItem(
        s"keymap-peek-${action.configKey}",
        keymapLabel(action.configKey),
        config.focusedKeymapConfig.peek.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetPeekKeyBinding(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetPeekKeyBinding(action))
      )
    )
    val primaryIds = List(
      "keymap-global-command_palette",
      "keymap-global-file_search",
      "keymap-editor-page_down",
      "keymap-command-runner-submit",
      "keymap-modal-dismiss",
      "keymap-panel-activate",
      "keymap-peek-accept"
    )
    primaryIds.flatMap(id => items.find(_.id == id)) ++ items.filterNot(item => primaryIds.contains(item.id))

  private def formatDecimal(value: Double): String =
    if value.isWhole then value.toLong.toString else value.toString

  private def keymapLabel(configKey: String): String =
    configKey.split("_").toList.map(_.capitalize).mkString(" ")

  private def bindingInputItem(
    id: String,
    label: String,
    currentValue: Option[String],
    parse: String => CommandIntent,
    reset: CommandIntent
  ): CommandSurfaceItem.InputItem =
    CommandSurfaceItem.InputItem(
      id = id,
      label = label,
      hint = "Binding or default",
      currentValue = currentValue.getOrElse(""),
      isDecimal = false,
      parse = text => parseBindingText(text, parse, reset),
      category = CommandCategory.Settings,
      acceptsBindingText = true
    )

  private def parseBindingText(
    text: String,
    parse: String => CommandIntent,
    reset: CommandIntent
  ): Option[CommandIntent] =
    text.trim.toLowerCase match
      case "default" | "reset" => Some(reset)
      case binding             => HotkeyTrigger.parse(binding).map(_ => parse(binding))
