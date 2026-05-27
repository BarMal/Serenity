package com.serenity.keystroke.translators

import com.serenity.keystroke.events.TextEntryEvent

class EditorInputTranslator extends Translator[TextEntryEvent]:

  override def converters = List(
    TextCharacterConverters.characterConverter,
    TextNavigationConverters.navigationConverter,
    TextDeletionConverters.deletionConverter
  )
