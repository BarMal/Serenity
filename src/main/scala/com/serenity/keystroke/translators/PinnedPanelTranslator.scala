package com.serenity.keystroke.translators

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.PanelInputEvent
import com.serenity.keystroke.events.PanelInputEvent.ReturnFocus
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

class PinnedPanelTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[PanelInputEvent]:

  override def converters =
    List(LocalKeymapConverters.converter(appConfig.focusedKeymapConfig.panel.bindings), panelCharacterConverter)

  private val panelCharacterConverter: PartialFunction[KeyStrokeInfo, PanelInputEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(_), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      ReturnFocus
  }
