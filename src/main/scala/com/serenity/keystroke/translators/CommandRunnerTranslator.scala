package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}

class CommandRunnerTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[CommandRunnerEvent]:

  override def converters = List(
    LocalKeymapConverters.converter(appConfig.inputConfig.focusedKeymapConfig.commandRunner.bindings),
    commandRunnerCharacterConverter
  )

  private val commandRunnerCharacterConverter: PartialFunction[KeyStrokeInfo, CommandRunnerEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(char), modifiers)
        if TextCharacterConverters.isPlainOrShiftOnly(modifiers) =>
      RunnerInsertChar(char)
  }
