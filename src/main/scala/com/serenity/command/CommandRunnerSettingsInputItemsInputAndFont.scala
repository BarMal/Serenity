package com.serenity.command

import com.serenity.config.{AppConfig, AutoSaveConfig}
import com.serenity.ui.fonts.FontLoader

/** Mouse-input and font-size input items. Split out of `CommandRunnerSettingsInputItems.build` to keep both under the
  * architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsInputItemsInputAndFont:

  private[command] def inputItems(wheelScrollLines: Int): List[CommandSurfaceItem.InputItem] = List(
    CommandSurfaceItem.InputItem(
      id = "wheel-scroll-lines",
      label = "Wheel Scroll Lines",
      hint = "Lines per mouse-wheel notch (1-50)",
      currentValue = wheelScrollLines.toString,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = false),
      parse = text =>
        text.toIntOption
          .filter(value => value >= 1 && value <= 50)
          .map(commandIntentArg =>
            CommandIntent
              .Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetWheelScrollLines(commandIntentArg)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(AppConfig.default.inputConfig.wheelScrollLines.toString)
    )
  )

  private[command] def autoSaveItems(delayMillis: Long): List[CommandSurfaceItem.InputItem] = List(
    CommandSurfaceItem.InputItem(
      id = "auto-save-delay",
      label = "Auto Save Delay",
      hint = s"Milliseconds after the last edit before After Delay saves (at least ${AutoSaveConfig.MinDelayMillis})",
      currentValue = delayMillis.toString,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = false),
      parse = text =>
        text.trim.toLongOption
          .filter(_ >= AutoSaveConfig.MinDelayMillis)
          .map(millis =>
            CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAutoSaveDelayMillis(millis)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(AutoSaveConfig.DefaultDelayMillis.toString)
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
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toFloatOption
          .filter(v => v >= 8.0f && v <= 48.0f)
          .map(commandIntentArg =>
            CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeFontSize(commandIntentArg)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(AppConfig.default.editorConfig.fontConfig.codeFontSize.toString)
    ),
    CommandSurfaceItem.InputItem(
      id = "text-font-size",
      label = "Prose Font Size",
      hint = "Points (8.0-48.0)",
      currentValue = textFontSizeValue,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toFloatOption
          .filter(v => v >= 8.0f && v <= 48.0f)
          .map(commandIntentArg =>
            CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextFontSize(commandIntentArg)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(AppConfig.default.editorConfig.fontConfig.textFontSize.toString)
    ),
    CommandSurfaceItem.InputItem(
      id = "ui-font-size",
      label = "UI Font Size",
      hint = "Points (8.0-48.0)",
      currentValue = uiFontSizeValue,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toFloatOption
          .filter(v => v >= 8.0f && v <= 48.0f)
          .map(commandIntentArg =>
            CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetUiFontSize(commandIntentArg)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(AppConfig.default.editorConfig.fontConfig.uiFontSize.toString)
    ),
    CommandSurfaceItem.InputItem(
      id = "text-scale",
      label = "Text Scale",
      hint = s"Multiplier (${FontLoader.FontConfig.MinTextScale}-${FontLoader.FontConfig.MaxTextScale})",
      currentValue = textScaleValue,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toDoubleOption
          .filter(value =>
            value >= FontLoader.FontConfig.MinTextScale &&
              value <= FontLoader.FontConfig.MaxTextScale
          )
          .map(commandIntentArg =>
            CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextScaleMultiplier(commandIntentArg)))
          ),
      category = CommandCategory.Settings,
      defaultValue = Some(f"${AppConfig.default.editorConfig.fontConfig.textScaleMultiplier}%.2f")
    )
  )
