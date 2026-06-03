package com.serenity.keystroke.translators

import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

object TextHotkeyConverters:

  val hotkeyConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some('s'), modifiers) if modifiers.contains(Modifier.Ctrl) => Save
    case KeyStrokeInfo(InputKey.Character, Some('q'), modifiers) if modifiers.contains(Modifier.Ctrl) => Quit
    case KeyStrokeInfo(InputKey.EOF, _, _)                                                            => Quit
    case KeyStrokeInfo(InputKey.Character, Some('z'), modifiers) if modifiers.contains(Modifier.Ctrl) => Undo
    case KeyStrokeInfo(InputKey.Character, Some('y'), modifiers) if modifiers.contains(Modifier.Ctrl) => Redo
    case KeyStrokeInfo(InputKey.Character, Some('c'), modifiers) if modifiers.contains(Modifier.Ctrl) => Copy
    case KeyStrokeInfo(InputKey.Character, Some('v'), modifiers) if modifiers.contains(Modifier.Ctrl) => Paste
    case KeyStrokeInfo(InputKey.Character, Some('x'), modifiers) if modifiers.contains(Modifier.Ctrl) => Cut
    case KeyStrokeInfo(InputKey.Character, Some('a'), modifiers) if modifiers.contains(Modifier.Ctrl) => SelectAll
    case KeyStrokeInfo(InputKey.Character, Some('h'), modifiers) if modifiers.contains(Modifier.Ctrl) =>
      ToggleSyntaxHighlighting
    case KeyStrokeInfo(InputKey.Character, Some('o'), modifiers) if modifiers.contains(Modifier.Ctrl) => OpenFile
    case KeyStrokeInfo(InputKey.Character, Some('p'), modifiers) if modifiers.contains(Modifier.Ctrl) =>
      ToggleCommandRunner
    case KeyStrokeInfo(InputKey.Character, Some('t'), modifiers) if modifiers.contains(Modifier.Ctrl) => NewTab
    case KeyStrokeInfo(InputKey.Character, Some('w'), modifiers) if modifiers.contains(Modifier.Ctrl) => CloseTab
    case KeyStrokeInfo(InputKey.Character, Some('f'), modifiers)
        if modifiers.contains(Modifier.Ctrl) && modifiers.contains(Modifier.Shift) =>
      FileSearch
    // Ctrl+Shift+Tab from Lanterna (terminal path): Tab key with both modifiers
    case KeyStrokeInfo(InputKey.Tab, _, modifiers)
        if modifiers.contains(Modifier.Ctrl) && modifiers.contains(Modifier.Shift) =>
      PreviousTab
    // Ctrl+Shift+Tab from Swing (ReverseTab + Ctrl), and Ctrl+ReverseTab from Lanterna
    case KeyStrokeInfo(InputKey.ReverseTab, _, modifiers) if modifiers.contains(Modifier.Ctrl) =>
      PreviousTab
    case KeyStrokeInfo(InputKey.Tab, _, modifiers) if modifiers.contains(Modifier.Ctrl) =>
      NextTab
  }
