package com.serenity.command

/** Rich-text selection formatting input items (size, colour) that still need typed values. Split out of
  * `CommandRunnerSettingsInputItems.build` to keep both under the architecture size targets -- see that object's doc.
  * Font family is a picker now (`CommandRunnerSettingsItems.richTextFontGroupItem`, issue #1060), not built here.
  */
private[command] object CommandRunnerSettingsInputItemsRichText:

  private[command] def richTextItems: List[CommandSurfaceItem.InputItem] = List(
    // issue #1060: matches the 8.0-48.0 range every other font-size setting (code/prose/UI) uses -- there was no
    // documented reason selection formatting needed a 1.0-144.0 ceiling three times as wide as everywhere else.
    CommandSurfaceItem.InputItem(
      id = "rich-text-font-size",
      label = "Selection Font Size",
      hint = "Points (8.0-48.0)",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toFloatOption
          .filter(v => v >= 8.0f && v <= 48.0f)
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
