package com.serenity.keystroke.translators

import com.serenity.config.{AppConfig, PanelKeyAction}
import com.serenity.keystroke.events.PanelInputEvent
import com.serenity.keystroke.events.PanelInputEvent.ReturnFocus
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

class PinnedPanelTranslator(appConfig: AppConfig = AppConfig.default) extends Translator[PanelInputEvent]:

  private val bindings = appConfig.inputConfig.focusedKeymapConfig.panel.bindings

  // Sessions saved before Escape had its own action still list it under return_focus as well, so Dismiss's own
  // bindings are tried first rather than leaving the tie to map order.
  override lazy val converters =
    List(
      LocalKeymapConverters.converter(bindings.view.filterKeys(_ == PanelKeyAction.Dismiss).toMap),
      LocalKeymapConverters.converter(bindings),
      panelCharacterConverter
    )

  private val panelCharacterConverter: PartialFunction[KeyStrokeInfo, PanelInputEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(_), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      ReturnFocus
  }
