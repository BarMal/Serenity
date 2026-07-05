package com.serenity.command

import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader

object CommandRunnerSettingsInputItems:

  def build(config: AppConfig): List[CommandSurfaceItem.InputItem] =
    val durationValue       = config.characterAnimation.map(_.durationMs.toString).getOrElse("0")
    val stepsValue          = config.characterAnimation.map(_.steps.toString).getOrElse("0")
    val blurValue           = config.blurRadius.toString
    val codeFontSizeValue   = config.fontConfig.codeFontSize.toString
    val textFontSizeValue   = config.fontConfig.textFontSize.toString
    val uiFontSizeValue     = config.fontConfig.uiFontSize.toString
    val textScaleValue      = f"${config.fontConfig.textScaleMultiplier}%.2f"
    val textAreaLeftValue   = f"${config.textAreaInsets.leftPercent}%.1f"
    val textAreaRightValue  = f"${config.textAreaInsets.rightPercent}%.1f"
    val textAreaTopValue    = f"${config.textAreaInsets.topPercent}%.1f"
    val textAreaBottomValue = f"${config.textAreaInsets.bottomPercent}%.1f"
    val speedScaleValue     = f"${config.elementTransitionSpeedScale}%.2f"
    val editorTextSpeedScaleValue =
      f"${config.effectiveEditorTextTransitionSpeedScale}%.2f"
    val commandRunnerSpeedScaleValue =
      f"${config.effectiveCommandRunnerTransitionSpeedScale}%.2f"
    val uiSpeedScaleValue     = f"${config.effectiveUiTransitionSpeedScale}%.2f"
    val cursorSpeedScaleValue = f"${config.effectiveCursorTransitionSpeedScale}%.2f"
    val elementGapValue       = config.uiElementGap.toString
    val cornerRadiusValue     = config.uiCornerRadiusPx.toString
    val commandRowsValue      = config.commandRunnerVisibleRows.map(_.toString).getOrElse("auto")
    val spellCheck            = config.spellCheck.normalized

    val commentItems = List(
      CommandSurfaceItem.InputItem(
        id = "document-comment",
        label = "Document Comment",
        hint = "Comment text",
        currentValue = "",
        isDecimal = false,
        parse = text => nonEmptyText(text).map(CommandIntent.AddDocumentComment(_)),
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
        parse = text => nonEmptyText(text).map(CommandIntent.SetRichTextFontFamily(_)),
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
            .map(CommandIntent.SetRichTextFontSize(_)),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "rich-text-color",
        label = "Selection Text Colour",
        hint = "#RRGGBB",
        currentValue = "",
        isDecimal = false,
        parse = text => normalizeHexColor(text).map(CommandIntent.SetRichTextColor(_)),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      )
    )

    val presetItems = List(
      CommandSurfaceItem.InputItem(
        id = "ui-preset-create",
        label = "Create Preset",
        hint = "New preset name",
        currentValue = "",
        isDecimal = false,
        parse = text => nonEmptyText(text).map(CommandIntent.SaveUiPreset(_)),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-save",
        label = "Save Current Preset",
        hint = "Preset name",
        currentValue = "",
        isDecimal = false,
        parse = text => nonEmptyText(text).map(CommandIntent.SaveUiPreset(_)),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-apply",
        label = "Apply Preset",
        hint = "Preset name",
        currentValue = "",
        isDecimal = false,
        parse = text => nonEmptyText(text).map(CommandIntent.ApplyUiPreset(_)),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-duplicate",
        label = "Duplicate Preset",
        hint = "Source -> Copy",
        currentValue = "",
        isDecimal = false,
        parse = text => namedPair(text).map(CommandIntent.DuplicateUiPreset.apply),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-rename",
        label = "Rename Preset",
        hint = "Current -> New",
        currentValue = "",
        isDecimal = false,
        parse = text => namedPair(text).map(CommandIntent.RenameUiPreset.apply),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-delete",
        label = "Delete Preset",
        hint = "Preset name",
        currentValue = "",
        isDecimal = false,
        parse = text => nonEmptyText(text).map(CommandIntent.DeleteUiPreset(_)),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-preset-reset",
        label = "Reset Preset",
        hint = "Built-in preset name",
        currentValue = "",
        isDecimal = false,
        parse = text => nonEmptyText(text).map(CommandIntent.ResetUiPreset(_)),
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
            .map(value => CommandIntent.SetTextAreaLeftInset(value / 100.0)),
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
            .map(value => CommandIntent.SetTextAreaRightInset(value / 100.0)),
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
            .map(value => CommandIntent.SetTextAreaTopInset(value / 100.0)),
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
            .map(value => CommandIntent.SetTextAreaBottomInset(value / 100.0)),
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
        parse = text => nonEmptyCommaList(text).map(CommandIntent.SetSpellCheckLanguages(_)),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "spellcheck-dictionaries",
        label = "Spell Check Dictionaries",
        hint = "Comma-separated .dic paths",
        currentValue = spellCheck.dictionaryPaths.mkString(","),
        isDecimal = false,
        parse = text => Some(CommandIntent.SetSpellCheckDictionaryPaths(commaListPreserveCase(text))),
        category = CommandCategory.Settings,
        acceptsFreeText = true
      ),
      CommandSurfaceItem.InputItem(
        id = "spellcheck-words",
        label = "Accepted Words",
        hint = "Comma-separated words",
        currentValue = spellCheck.additionalWords.mkString(","),
        isDecimal = false,
        parse = text => Some(CommandIntent.SetSpellCheckWords(commaList(text))),
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
            .map(CommandIntent.SetAnimationDuration(_)),
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
            .map(CommandIntent.SetAnimationSteps(_)),
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
            .map(CommandIntent.SetElementTransitionSpeedScale(_)),
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
            .map(CommandIntent.SetEditorTextTransitionSpeedScale(_)),
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
            .map(CommandIntent.SetCommandRunnerTransitionSpeedScale(_)),
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
            .map(CommandIntent.SetUiTransitionSpeedScale(_)),
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
            .map(CommandIntent.SetCursorTransitionSpeedScale(_)),
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
            .map(CommandIntent.SetBlurRadius(_)),
        category = CommandCategory.Settings
      ),
      CommandSurfaceItem.InputItem(
        id = "ui-element-gap",
        label = "UI Element Gap",
        hint = s"Cells (${AppConfig.MinUiElementGap}-${AppConfig.MaxUiElementGap})",
        currentValue = elementGapValue,
        isDecimal = false,
        parse = text =>
          text.toIntOption
            .filter(value => value >= AppConfig.MinUiElementGap && value <= AppConfig.MaxUiElementGap)
            .map(CommandIntent.SetUiElementGap(_)),
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
            .map(CommandIntent.SetUiCornerRadiusPx(_)),
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
          if normalized == "auto" then Some(CommandIntent.SetCommandRunnerVisibleRows(None))
          else
            normalized.toIntOption
              .filter(value =>
                value >= AppConfig.MinCommandRunnerVisibleRows &&
                  value <= AppConfig.MaxCommandRunnerVisibleRows
              )
              .map(value => CommandIntent.SetCommandRunnerVisibleRows(Some(value)))
        ,
        category = CommandCategory.Settings,
        acceptsFreeText = true
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
            .map(CommandIntent.SetCodeFontSize(_)),
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
            .map(CommandIntent.SetTextFontSize(_)),
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
            .map(CommandIntent.SetUiFontSize(_)),
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
            .map(CommandIntent.SetTextScaleMultiplier(_)),
        category = CommandCategory.Settings
      )
    )

    commentItems ++ presetItems ++ richTextItems ++ textAreaItems ++ spellCheckItems ++ numericItems ++
      buildKeymapInputItems(config)

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

  private def buildKeymapInputItems(config: AppConfig): List[CommandSurfaceItem.InputItem] =
    List(
      bindingInputItem(
        id = "keymap-global-command_palette",
        label = "Command Palette",
        currentValue = config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).headOption.map(_.render),
        parse = binding => CommandIntent.SetGlobalHotkey(HotkeyAction.ToggleCommandRunner, binding),
        reset = CommandIntent.ResetGlobalHotkey(HotkeyAction.ToggleCommandRunner)
      ),
      bindingInputItem(
        id = "keymap-global-file_search",
        label = "File Search",
        currentValue = config.hotkeyConfig.bindingsFor(HotkeyAction.FileSearch).headOption.map(_.render),
        parse = binding => CommandIntent.SetGlobalHotkey(HotkeyAction.FileSearch, binding),
        reset = CommandIntent.ResetGlobalHotkey(HotkeyAction.FileSearch)
      ),
      bindingInputItem(
        id = "keymap-editor-page_down",
        label = "Editor Page Down",
        currentValue = config.focusedKeymapConfig.editor.bindingsFor(EditorKeyAction.PageDown).headOption.map(_.render),
        parse = binding => CommandIntent.SetEditorKeyBinding(EditorKeyAction.PageDown, binding),
        reset = CommandIntent.ResetEditorKeyBinding(EditorKeyAction.PageDown)
      ),
      bindingInputItem(
        id = "keymap-command-runner-submit",
        label = "Command Submit",
        currentValue =
          config.focusedKeymapConfig.commandRunner.bindingsFor(CommandRunnerKeyAction.Submit).headOption.map(_.render),
        parse = binding => CommandIntent.SetCommandRunnerKeyBinding(CommandRunnerKeyAction.Submit, binding),
        reset = CommandIntent.ResetCommandRunnerKeyBinding(CommandRunnerKeyAction.Submit)
      ),
      bindingInputItem(
        id = "keymap-modal-dismiss",
        label = "Modal Dismiss",
        currentValue = config.focusedKeymapConfig.modal.bindingsFor(ModalKeyAction.Dismiss).headOption.map(_.render),
        parse = binding => CommandIntent.SetModalKeyBinding(ModalKeyAction.Dismiss, binding),
        reset = CommandIntent.ResetModalKeyBinding(ModalKeyAction.Dismiss)
      ),
      bindingInputItem(
        id = "keymap-panel-activate",
        label = "Panel Activate",
        currentValue = config.focusedKeymapConfig.panel.bindingsFor(PanelKeyAction.Activate).headOption.map(_.render),
        parse = binding => CommandIntent.SetPanelKeyBinding(PanelKeyAction.Activate, binding),
        reset = CommandIntent.ResetPanelKeyBinding(PanelKeyAction.Activate)
      ),
      bindingInputItem(
        id = "keymap-peek-accept",
        label = "Peek Accept",
        currentValue = config.focusedKeymapConfig.peek.bindingsFor(PeekKeyAction.Accept).headOption.map(_.render),
        parse = binding => CommandIntent.SetPeekKeyBinding(PeekKeyAction.Accept, binding),
        reset = CommandIntent.ResetPeekKeyBinding(PeekKeyAction.Accept)
      )
    )

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
