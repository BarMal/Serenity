package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.*

class SingleLineFormTranslator extends Translator[ModalInputEvent]:

  override def converters = List(DirectionalKeyConverter.arrowKeys(ModalNavigate.apply), singleLineFormConverter)

  private val singleLineFormConverter: PartialFunction[KeyStrokeInfo, ModalInputEvent] = {
    case KeyStrokeInfo(KeyType.Character, Some(char), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      ModalInsertChar(char)
    case KeyStrokeInfo(KeyType.Backspace, _, _)  => ModalDeleteBackward
    case KeyStrokeInfo(KeyType.Tab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      ModalNextField
    case KeyStrokeInfo(KeyType.ReverseTab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      ModalPreviousField
    case KeyStrokeInfo(KeyType.Enter, _, _)  => ModalSubmit
    case KeyStrokeInfo(KeyType.Escape, _, _) => ModalDismiss
  }
