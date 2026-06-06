package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

class SingleLineFormTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[ModalInputEvent]:

  override def converters = List(
    LocalKeymapConverters.converter(appConfig.focusedKeymapConfig.modal.bindings),
    singleLineFormCharacterConverter
  )

  private val singleLineFormCharacterConverter: PartialFunction[KeyStrokeInfo, ModalInputEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(char), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      ModalInsertChar(char)
  }
