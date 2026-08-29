package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}

class SingleLineFormTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[ModalInputEvent]:

  override def converters = List(
    LocalKeymapConverters.converter(appConfig.inputConfig.focusedKeymapConfig.modal.bindings),
    singleLineFormCharacterConverter
  )

  private val singleLineFormCharacterConverter: PartialFunction[KeyStrokeInfo, ModalInputEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(char), modifiers)
        if TextCharacterConverters.isPlainOrShiftOnly(modifiers) =>
      ModalInsertChar(char)
  }
