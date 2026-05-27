package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.{Direction, PeekInputEvent}
import com.serenity.keystroke.events.PeekInputEvent.{Accept, Dismiss, Navigate, OtherInput}

class PeekOverlayTranslator extends Translator[PeekInputEvent]:

  override def converters = List(DirectionalKeyConverter.arrowKeys(Navigate.apply), peekConverter)

  private val peekConverter: PartialFunction[KeyStrokeInfo, PeekInputEvent] = {
    case KeyStrokeInfo(KeyType.Character, Some(_), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      OtherInput
    case KeyStrokeInfo(KeyType.Backspace, _, _)   => OtherInput
    case KeyStrokeInfo(KeyType.Delete, _, _)      => OtherInput
    case KeyStrokeInfo(KeyType.Tab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      OtherInput
    case KeyStrokeInfo(KeyType.ReverseTab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      OtherInput
    case KeyStrokeInfo(KeyType.Escape, _, _)      => Dismiss
    case KeyStrokeInfo(KeyType.Enter, _, _)       => Accept
  }
