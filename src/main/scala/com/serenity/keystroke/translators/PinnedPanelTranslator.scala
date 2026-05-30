package com.serenity.keystroke.translators

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.keystroke.events.PanelInputEvent.{Navigate, NoOp, ReturnFocus}

class PinnedPanelTranslator extends Translator[PanelInputEvent]:

  override def converters = List(DirectionalKeyConverter.arrowKeys(Navigate.apply), panelConverter)

  private val panelConverter: PartialFunction[KeyStrokeInfo, PanelInputEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(_), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      ReturnFocus
    case KeyStrokeInfo(InputKey.Backspace, _, _)   => ReturnFocus
    case KeyStrokeInfo(InputKey.Delete, _, _)      => ReturnFocus
    case KeyStrokeInfo(InputKey.Tab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      ReturnFocus
    case KeyStrokeInfo(InputKey.ReverseTab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      ReturnFocus
    case KeyStrokeInfo(InputKey.Escape, _, _)      => ReturnFocus
    case KeyStrokeInfo(InputKey.Enter, _, _)       => NoOp
  }
