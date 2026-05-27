package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.events.{InsertChar, NewLine, ReverseTabKey, TabKey, TextEntryEvent}
import com.serenity.keystroke.{KeyStrokeInfo, Modifier}

object TextCharacterConverters:

  val characterConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(KeyType.Character, Some(char), modifiers)
        if isAcceptableForTextEntry(modifiers) && isPrintableChar(char) =>
      InsertChar(char)
    case KeyStrokeInfo(KeyType.Tab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      TabKey
    case KeyStrokeInfo(KeyType.ReverseTab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      ReverseTabKey
    case KeyStrokeInfo(KeyType.Enter, _, _) =>
      NewLine
  }

  private def isPrintableChar(char: Char): Boolean =
    (char >= 32 && char <= 126) || char == '\t'

  private def isAcceptableForTextEntry(modifiers: Set[Modifier]): Boolean =
    modifiers.isEmpty || modifiers == Set(Modifier.Shift)
