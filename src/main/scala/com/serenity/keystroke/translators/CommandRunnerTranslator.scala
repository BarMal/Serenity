package com.serenity.keystroke.translators

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.keystroke.events.*

class CommandRunnerTranslator extends Translator[CommandRunnerEvent]:

  override def converters = List(DirectionalKeyConverter.arrowKeys(RunnerNavigate.apply), commandRunnerConverter)

  private val commandRunnerConverter: PartialFunction[KeyStrokeInfo, CommandRunnerEvent] = {
    case KeyStrokeInfo(InputKey.Character, Some(char), modifiers)
        if modifiers.isEmpty || modifiers == Set(Modifier.Shift) =>
      RunnerInsertChar(char)
    case KeyStrokeInfo(InputKey.Backspace, _, _) => RunnerDeleteBackward
    case KeyStrokeInfo(InputKey.Tab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      RunnerNextCategory
    case KeyStrokeInfo(InputKey.ReverseTab, _, modifiers) if !modifiers.contains(Modifier.Ctrl) =>
      RunnerPreviousCategory
    case KeyStrokeInfo(InputKey.Enter, _, _)  => RunnerSubmit
    case KeyStrokeInfo(InputKey.Escape, _, _) => RunnerDismiss
  }
