package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{KeyStrokeInfo, Modifier}

class TextEntryTranslator extends Translator[TextEntryEvent]:

  override def converters: List[PartialFunction[KeyStrokeInfo, TextEntryEvent]] = List(
    characterConverter,
    navigationConverter,
    deletionConverter,
    hotkeyConverter
  )

  private val characterConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(KeyType.Character, Some(char), modifiers)
        if isAcceptableForTextEntry(modifiers) && isPrintableChar(char) =>
      InsertChar(char)
    case KeyStrokeInfo(KeyType.Tab, _, _)   => InsertChar('\t')
    case KeyStrokeInfo(KeyType.Enter, _, _) => NewLine
  }

  private val navigationConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(KeyType.ArrowLeft, _, _)  => MoveLeft
    case KeyStrokeInfo(KeyType.ArrowRight, _, _) => MoveRight
    case KeyStrokeInfo(KeyType.ArrowUp, _, _)    => MoveUp
    case KeyStrokeInfo(KeyType.ArrowDown, _, _)  => MoveDown
    case KeyStrokeInfo(KeyType.Home, _, _)       => MoveToStart
    case KeyStrokeInfo(KeyType.End, _, _)        => MoveToEnd
  }

  private val deletionConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(KeyType.Backspace, _, _) => DeleteBackward
    case KeyStrokeInfo(KeyType.Delete, _, _)    => DeleteForward
    case KeyStrokeInfo(KeyType.Escape, _, _)    => Escape
  }

  private val hotkeyConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(KeyType.Character, Some('s'), modifiers) if modifiers.contains(Modifier.Ctrl) => Save
    case KeyStrokeInfo(KeyType.Character, Some('q'), modifiers) if modifiers.contains(Modifier.Ctrl) => Quit
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
  }

  private def isPrintableChar(char: Char): Boolean =
    (char >= 32 && char <= 126) || char == '\t'

  private def isAcceptableForTextEntry(modifiers: Set[Modifier]): Boolean =
    // Accept no modifiers (lowercase letters, digits, most punctuation)
    // Accept only Shift modifier (uppercase letters, shifted symbols)
    modifiers.isEmpty || modifiers == Set(Modifier.Shift)
