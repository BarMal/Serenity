package com.serenity.command

/** Rich-text selection formatting input items (font family, size, colour). Split out of
  * `CommandRunnerSettingsInputItems.build` to keep both under the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsInputItemsRichText:

  private[command] def richTextItems: List[CommandSurfaceItem.InputItem] = List(
    CommandSurfaceItem.InputItem(
      id = "rich-text-font-family",
      label = "Selection Font Family",
      hint = "Family name",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = CommandRunnerSettingsInputItems.parseRichTextFontFamily,
      category = CommandCategory.Settings
    ),
    CommandSurfaceItem.InputItem(
      id = "rich-text-font-size",
      label = "Selection Font Size",
      hint = "Points (1.0-144.0)",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
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
      kind = CommandSurfaceItem.InputKind.FreeText,
      parse = CommandRunnerSettingsInputItems.parseRichTextColor,
      category = CommandCategory.Settings
    )
  )
