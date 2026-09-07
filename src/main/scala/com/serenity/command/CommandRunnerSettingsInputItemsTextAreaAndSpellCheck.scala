package com.serenity.command

import com.serenity.config.*

/** Editor text-area margin and spell-check input items. Split out of `CommandRunnerSettingsInputItems.build` to keep
  * both under the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsInputItemsTextAreaAndSpellCheck:

  private[command] def textAreaItems(
    textAreaLeftValue: String,
    textAreaRightValue: String,
    textAreaTopValue: String,
    textAreaBottomValue: String
  ): List[CommandSurfaceItem.InputItem] = List(
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

  private[command] def spellCheckItems(spellCheck: SpellCheckConfig): List[CommandSurfaceItem.InputItem] = List(
    CommandSurfaceItem.InputItem(
      id = "spellcheck-languages",
      label = "Spell Check Languages",
      hint = "Comma-separated codes",
      currentValue = spellCheck.languages.mkString(","),
      isDecimal = false,
      parse = text =>
        CommandRunnerSettingsInputItems
          .nonEmptyCommaList(text)
          .map(commandIntentArg =>
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
            SettingsIntent.SpellCheck(
              SpellCheckIntent.SetSpellCheckDictionaryPaths(CommandRunnerSettingsInputItems.commaListPreserveCase(text))
            )
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
        Some(
          CommandIntent
            .Settings(
              SettingsIntent.SpellCheck(
                SpellCheckIntent.SetSpellCheckWords(CommandRunnerSettingsInputItems.commaList(text))
              )
            )
        ),
      category = CommandCategory.Settings,
      acceptsFreeText = true
    )
  )
