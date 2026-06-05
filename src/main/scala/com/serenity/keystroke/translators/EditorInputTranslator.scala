package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.TextEntryEvent

class EditorInputTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[TextEntryEvent]:

  override def converters = List(
    LocalKeymapConverters.converter(appConfig.focusedKeymapConfig.editor.bindings),
    TextCharacterConverters.characterConverter
  )
