package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.PeekInputEvent
import com.serenity.keystroke.events.PeekInputEvent.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

class PeekOverlayTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[PeekInputEvent]:

  override def converters =
    List(
      LocalKeymapConverters.converter(appConfig.inputConfig.focusedKeymapConfig.peek.bindings),
      peekCharacterConverter
    )

  private val peekCharacterConverter: PartialFunction[KeyStrokeInfo, PeekInputEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(_), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      OtherInput
  }
