package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.keystroke.events.PanelInputEvent.{Navigate, NoOp, ReturnFocus}

class PinnedPanelTranslator extends Translator[PanelInputEvent]:

  override def converters = List(DirectionalKeyConverter.arrowKeys(Navigate.apply), panelConverter)

  private val panelConverter: PartialFunction[KeyStrokeInfo, PanelInputEvent] = {
    case KeyStrokeInfo(KeyType.Character, Some(_), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      ReturnFocus
    case KeyStrokeInfo(KeyType.Backspace, _, _)   => ReturnFocus
    case KeyStrokeInfo(KeyType.Delete, _, _)      => ReturnFocus
    case KeyStrokeInfo(KeyType.Tab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      ReturnFocus
    case KeyStrokeInfo(KeyType.ReverseTab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      ReturnFocus
    case KeyStrokeInfo(KeyType.Escape, _, _)      => ReturnFocus
    case KeyStrokeInfo(KeyType.Enter, _, _)       => NoOp
  }
