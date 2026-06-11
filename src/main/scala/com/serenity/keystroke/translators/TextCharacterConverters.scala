package com.serenity.keystroke.translators

import com.serenity.keystroke.events.*
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
    char == '\t' || !Character.isISOControl(char)

  private def isAcceptableForTextEntry(modifiers: Set[Modifier]): Boolean =
    !modifiers.contains(Modifier.Ctrl) && modifiers.subsetOf(Set(Modifier.Shift, Modifier.Alt))
