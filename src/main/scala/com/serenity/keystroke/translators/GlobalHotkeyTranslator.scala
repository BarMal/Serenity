package com.serenity.keystroke.translators

import com.serenity.keystroke.events.Event

class GlobalHotkeyTranslator extends Translator[Event]:

  override def converters = List(
    TextHotkeyConverters.hotkeyConverter
  )
