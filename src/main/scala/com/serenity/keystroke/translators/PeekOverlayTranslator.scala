package com.serenity.keystroke.translators

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.keystroke.events.{Direction, PeekInputEvent}
import com.serenity.keystroke.events.PeekInputEvent.{Accept, Dismiss, Navigate, OtherInput}

class PeekOverlayTranslator extends Translator[PeekInputEvent]:

  override def converters = List(DirectionalKeyConverter.arrowKeys(Navigate.apply), peekConverter)

  private val peekConverter: PartialFunction[KeyStrokeInfo, PeekInputEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(_), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      OtherInput
    case KeyStrokeInfo(InputKey.Backspace, _, _)   => OtherInput
    case KeyStrokeInfo(InputKey.Delete, _, _)      => OtherInput
    case KeyStrokeInfo(InputKey.Tab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      OtherInput
    case KeyStrokeInfo(InputKey.ReverseTab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      OtherInput
    case KeyStrokeInfo(InputKey.Escape, _, _)      => Dismiss
    case KeyStrokeInfo(InputKey.Enter, _, _)       => Accept
  }
