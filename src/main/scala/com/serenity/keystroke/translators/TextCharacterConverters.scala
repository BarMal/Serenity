package com.serenity.keystroke.translators

import com.serenity.keystroke.events.{InsertChar, NewLine, ReverseTabKey, TabKey, TextEntryEvent}
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

object TextCharacterConverters:

  val characterConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(char), modifiers)
        if isAcceptableForTextEntry(modifiers) && isPrintableChar(char) =>
      InsertChar(char)
    case KeyStrokeInfo(InputKey.Tab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      TabKey
    case KeyStrokeInfo(InputKey.ReverseTab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      ReverseTabKey
    case KeyStrokeInfo(InputKey.Enter, _, _) =>
      NewLine
  }

  private def isPrintableChar(char: Char): Boolean =
    (char >= 32 && char <= 126) || char == '\t'

  private def isAcceptableForTextEntry(modifiers: Set[Modifier]): Boolean =
    modifiers.isEmpty || modifiers == Set(Modifier.Shift)
