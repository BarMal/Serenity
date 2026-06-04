package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

class CommandRunnerTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[CommandRunnerEvent]:

  override def converters = List(LocalKeymapConverters.converter(appConfig.focusedKeymapConfig.commandRunner.bindings), commandRunnerCharacterConverter)

  private val commandRunnerCharacterConverter: PartialFunction[KeyStrokeInfo, CommandRunnerEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(char), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      RunnerInsertChar(char)
  }
