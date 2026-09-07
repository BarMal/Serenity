package com.serenity.command

/** Rich-text selection formatting input items (font family, size, colour). Split out of
  * `CommandRunnerSettingsInputItems.build` to keep both under the architecture size targets -- see that
  * object's doc.
  */
private[command] object CommandRunnerSettingsInputItemsRichText:

  private[command] def richTextItems: List[CommandSurfaceItem.InputItem] = List(
  CommandSurfaceItem.InputItem(
    id = "rich-text-font-family",
    label = "Selection Font Family",
    hint = "Family name",
    currentValue = "",
    isDecimal = false,
    parse = CommandRunnerSettingsInputItems.parseRichTextFontFamily,
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
    parse = CommandRunnerSettingsInputItems.parseRichTextColor,
    category = CommandCategory.Settings,
    acceptsFreeText = true
  )
  )
