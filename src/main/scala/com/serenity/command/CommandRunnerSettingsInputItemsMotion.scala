package com.serenity.command

import com.serenity.config.*

/** Animation timing and transition speed-scale input items. Split out of `CommandRunnerSettingsInputItems.build`
  * to keep both under the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsInputItemsMotion:

  private[command] def animationTimingItems(
    durationValue: String,
    stepsValue: String,
    speedScaleValue: String,
    editorTextSpeedScaleValue: String
  ): List[CommandSurfaceItem.InputItem] = List(
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
  )

  private[command] def speedScaleItems(
    commandRunnerSpeedScaleValue: String,
    uiSpeedScaleValue: String,
    cursorSpeedScaleValue: String,
    blurValue: String
  ): List[CommandSurfaceItem.InputItem] = List(
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
  )
