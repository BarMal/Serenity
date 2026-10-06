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
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toDoubleOption
          .filter(value => value >= 0.0 && value <= 45.0)
          .map(value =>
            CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetTextAreaLeftInset(value / 100.0)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(f"${AppConfig.default.surfaceConfig.textAreaInsets.leftPercent}%.1f")
    ),
    CommandSurfaceItem.InputItem(
      id = "text-area-right",
      label = "Right Text Margin",
      hint = "Percent (0-45)",
      currentValue = textAreaRightValue,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toDoubleOption
          .filter(value => value >= 0.0 && value <= 45.0)
          .map(value =>
            CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetTextAreaRightInset(value / 100.0)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(f"${AppConfig.default.surfaceConfig.textAreaInsets.rightPercent}%.1f")
    ),
    CommandSurfaceItem.InputItem(
      id = "text-area-top",
      label = "Top Text Margin",
      hint = "Percent (0-45)",
      currentValue = textAreaTopValue,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toDoubleOption
          .filter(value => value >= 0.0 && value <= 45.0)
          .map(value =>
            CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetTextAreaTopInset(value / 100.0)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(f"${AppConfig.default.surfaceConfig.textAreaInsets.topPercent}%.1f")
    ),
    CommandSurfaceItem.InputItem(
      id = "text-area-bottom",
      label = "Bottom Text Margin",
      hint = "Percent (0-45)",
      currentValue = textAreaBottomValue,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toDoubleOption
          .filter(value => value >= 0.0 && value <= 45.0)
          .map(value =>
            CommandIntent
              .Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetTextAreaBottomInset(value / 100.0)))
          ),
      defaultValue = Some(f"${AppConfig.default.surfaceConfig.textAreaInsets.bottomPercent}%.1f"),
      category = CommandCategory.Settings
    )
  )

  /** Multi-column e-reader layout (issue #1338, Phase 2 / slice 4): count-driven columns, always visible regardless of
    * whether column mode is on. "columns" is Auto-inclusive (`stepFromAuto`, mirroring `columnCount`'s own
    * Auto/`Some(n)` split); "column-gap" is a plain non-negative cell count, like `columnGap` itself.
    */
  private[command] def columnItems(
    columnCountValue: String,
    columnGapValue: String
  ): List[CommandSurfaceItem.InputItem] =
    List(
      CommandSurfaceItem.InputItem(
        id = "columns",
        label = "Columns",
        hint = "Column count, or auto (1+)",
        currentValue = columnCountValue,
        kind = CommandSurfaceItem.InputKind.Numeric(decimal = false),
        parse = text =>
          text.trim.toLowerCase match
            case "auto" | "0" =>
              Some(CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(None))))
            case trimmed =>
              trimmed.toIntOption
                .filter(_ >= 1)
                .map(count =>
                  CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnCount(Some(count))))
                ),
        category = CommandCategory.Settings,
        defaultValue = Some("auto"),
        stepFromAuto = Some(1)
      ),
      CommandSurfaceItem.InputItem(
        id = "column-gap",
        label = "Column Gap",
        hint = "Cells between columns",
        currentValue = columnGapValue,
        kind = CommandSurfaceItem.InputKind.Numeric(decimal = false),
        parse = text =>
          text.toIntOption
            .filter(_ >= 0)
            .map(cells => CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetColumnGap(cells)))),
        category = CommandCategory.Settings,
        defaultValue = Some(AppConfig.default.surfaceConfig.columnGap.toString)
      )
    )

  private[command] def spellCheckItems(spellCheck: SpellCheckConfig): List[CommandSurfaceItem.InputItem] = List(
    CommandSurfaceItem.InputItem(
      id = "spellcheck-languages",
      label = "Spell Check Languages",
      hint = "Comma-separated codes, e.g. en-GB or en-US",
      currentValue = spellCheck.languages.mkString(","),
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        CommandRunnerSettingsTextParsing
          .nonEmptyCommaList(text)
          .map(commandIntentArg =>
            CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckLanguages(commandIntentArg)))
          ),
      category = CommandCategory.Settings
    ),
    CommandSurfaceItem.InputItem(
      id = "spellcheck-dictionaries",
      label = "Spell Check Dictionaries",
      hint = "Comma-separated .dic paths",
      currentValue = spellCheck.dictionaryPaths.mkString(","),
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        Some(
          CommandIntent.Settings(
            SettingsIntent.SpellCheck(
              SpellCheckIntent.SetSpellCheckDictionaryPaths(
                CommandRunnerSettingsTextParsing.commaListPreserveCase(text)
              )
            )
          )
        ),
      category = CommandCategory.Settings
    ),
    CommandSurfaceItem.InputItem(
      id = "spellcheck-words",
      label = "Accepted Words",
      hint = "Comma-separated words",
      currentValue = spellCheck.additionalWords.mkString(","),
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = text =>
        Some(
          CommandIntent
            .Settings(
              SettingsIntent.SpellCheck(
                SpellCheckIntent.SetSpellCheckWords(CommandRunnerSettingsTextParsing.commaList(text))
              )
            )
        ),
      category = CommandCategory.Settings
    )
  )
