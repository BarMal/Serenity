package com.serenity.keystroke.translators

import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.TextEntryEvent

class TextEntryTranslator extends Translator[TextEntryEvent]:

  override def converters: List[PartialFunction[KeyStrokeInfo, TextEntryEvent]] = List(
    TextHotkeyConverters.hotkeyConverter,
    TextCharacterConverters.characterConverter,
    TextNavigationConverters.navigationConverter,
    TextDeletionConverters.deletionConverter
  )
