package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event

class TextEntryTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[Event]:

  private val delegate = CompositeTranslator(
    new GlobalHotkeyTranslator(appConfig),
    new EditorInputTranslator(appConfig)
  )

  override def converters: List[PartialFunction[KeyStrokeInfo, Event]] =
    delegate.converters
