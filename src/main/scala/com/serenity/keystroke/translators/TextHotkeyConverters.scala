package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{KeyStrokeInfo, Modifier}

object TextHotkeyConverters:

  val hotkeyConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(KeyType.Character, Some('s'), modifiers) if modifiers.contains(Modifier.Ctrl) => Save
    case KeyStrokeInfo(KeyType.Character, Some('q'), modifiers) if modifiers.contains(Modifier.Ctrl) => Quit
    case KeyStrokeInfo(KeyType.EOF, _, _) => Quit
    case KeyStrokeInfo(KeyType.Character, Some('z'), modifiers) if modifiers.contains(Modifier.Ctrl) => Undo
    case KeyStrokeInfo(KeyType.Character, Some('y'), modifiers) if modifiers.contains(Modifier.Ctrl) => Redo
    case KeyStrokeInfo(KeyType.Character, Some('c'), modifiers) if modifiers.contains(Modifier.Ctrl) => Copy
    case KeyStrokeInfo(KeyType.Character, Some('v'), modifiers) if modifiers.contains(Modifier.Ctrl) => Paste
    case KeyStrokeInfo(KeyType.Character, Some('x'), modifiers) if modifiers.contains(Modifier.Ctrl) => Cut
    case KeyStrokeInfo(KeyType.Character, Some('h'), modifiers) if modifiers.contains(Modifier.Ctrl) =>
      ToggleSyntaxHighlighting
    case KeyStrokeInfo(KeyType.Character, Some('o'), modifiers) if modifiers.contains(Modifier.Ctrl) => OpenFile
    case KeyStrokeInfo(KeyType.Character, Some('p'), modifiers) if modifiers.contains(Modifier.Ctrl) =>
      ToggleCommandRunner
    case KeyStrokeInfo(KeyType.Character, Some('t'), modifiers) if modifiers.contains(Modifier.Ctrl) => NewTab
    case KeyStrokeInfo(KeyType.Character, Some('w'), modifiers) if modifiers.contains(Modifier.Ctrl) => CloseTab
    case KeyStrokeInfo(KeyType.Tab, _, modifiers)
        if modifiers.contains(Modifier.Ctrl) && modifiers.contains(Modifier.Shift) =>
      PreviousTab
    case KeyStrokeInfo(KeyType.ReverseTab, _, modifiers) if modifiers.contains(Modifier.Ctrl) =>
      PreviousTab
    case KeyStrokeInfo(KeyType.Tab, _, modifiers) if modifiers.contains(Modifier.Ctrl) =>
      NextTab
  }
