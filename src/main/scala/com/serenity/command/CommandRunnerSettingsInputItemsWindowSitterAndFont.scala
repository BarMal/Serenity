package com.serenity.command

import com.serenity.animation.WindowSitterConfig
import com.serenity.ui.fonts.FontLoader

/** Window-sitter behavior and font-size input items. Split out of `CommandRunnerSettingsInputItems.build` to keep both
  * under the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsInputItemsWindowSitterAndFont:

  private[command] def windowSitterAndInputItems(
    sitterConfig: WindowSitterConfig,
    wheelScrollLines: Int
  ): List[CommandSurfaceItem.InputItem] = List(
    CommandSurfaceItem.InputItem(
      id = "window-sitter-frames",
      label = "Sitter Frames",
      hint = "Comma-separated glyphs",
      currentValue = sitterConfig.frames.mkString(","),
      isDecimal = false,
      parse = text =>
        CommandRunnerSettingsInputItems
          .nonEmptyCommaList(text)
          .map(values =>
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
      currentValue = wheelScrollLines.toString,
      isDecimal = false,
      parse = text =>
        text.toIntOption
          .filter(value => value >= 1 && value <= 50)
          .map(commandIntentArg =>
            CommandIntent
              .Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWheelScrollLines(commandIntentArg)))
          ),
      category = CommandCategory.Settings
    )
  )

  private[command] def fontSizeItems(
    codeFontSizeValue: String,
    textFontSizeValue: String,
    uiFontSizeValue: String,
    textScaleValue: String
  ): List[CommandSurfaceItem.InputItem] = List(
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
