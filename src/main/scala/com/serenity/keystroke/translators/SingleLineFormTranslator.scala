package com.serenity.keystroke.translators

import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

class SingleLineFormTranslator extends Translator[ModalInputEvent]:

  override def converters = List(DirectionalKeyConverter.arrowKeys(ModalNavigate.apply), singleLineFormConverter)

  private val singleLineFormConverter: PartialFunction[KeyStrokeInfo, ModalInputEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(char), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      ModalInsertChar(char)
    case KeyStrokeInfo(InputKey.Backspace, _, _) => ModalDeleteBackward
    case KeyStrokeInfo(InputKey.Tab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      ModalNextField
    case KeyStrokeInfo(InputKey.ReverseTab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      ModalPreviousField
    case KeyStrokeInfo(InputKey.Enter, _, _)  => ModalSubmit
    case KeyStrokeInfo(InputKey.Escape, _, _) => ModalDismiss
  }
