package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*

class CommandRunnerTranslator extends Translator[CommandRunnerEvent]:

  override def converters = List(DirectionalKeyConverter.arrowKeys(RunnerNavigate.apply), commandRunnerConverter)

  private val commandRunnerConverter: PartialFunction[KeyStrokeInfo, CommandRunnerEvent] = {
    case KeyStrokeInfo(KeyType.Character, Some(char), modifiers)
        if modifiers.isEmpty || modifiers == Set(com.serenity.keystroke.Modifier.Shift) =>
      RunnerInsertChar(char)
    case KeyStrokeInfo(KeyType.Backspace, _, _) => RunnerDeleteBackward
    case KeyStrokeInfo(KeyType.Tab, _, modifiers) if !modifiers.contains(com.serenity.keystroke.Modifier.Ctrl) =>
      RunnerNextCategory
    case KeyStrokeInfo(KeyType.ReverseTab, _, modifiers) if !modifiers.contains(com.serenity.keystroke.Modifier.Ctrl) =>
      RunnerPreviousCategory
    case KeyStrokeInfo(KeyType.Enter, _, _)  => RunnerSubmit
    case KeyStrokeInfo(KeyType.Escape, _, _) => RunnerDismiss
  }
