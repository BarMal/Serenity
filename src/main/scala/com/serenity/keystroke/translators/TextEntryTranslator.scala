package com.serenity.keystroke.translators

import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event

class TextEntryTranslator extends Translator[Event]:

  private val delegate = CompositeTranslator(
    new GlobalHotkeyTranslator(),
    new EditorInputTranslator()
  )

  override def converters: List[PartialFunction[KeyStrokeInfo, Event]] =
    delegate.converters
