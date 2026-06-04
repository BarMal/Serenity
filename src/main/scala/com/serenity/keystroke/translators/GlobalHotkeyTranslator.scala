package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.Event

class GlobalHotkeyTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[Event]:

  override def converters = List(
    TextHotkeyConverters.hotkeyConverter(appConfig)
  )
