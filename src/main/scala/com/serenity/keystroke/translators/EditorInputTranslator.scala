package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.EditorEvent

class EditorInputTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[EditorEvent]:

  override def converters = List(
    LocalKeymapConverters.converter(appConfig.inputConfig.focusedKeymapConfig.editor.bindings),
    TextCharacterConverters.characterConverter
  )
