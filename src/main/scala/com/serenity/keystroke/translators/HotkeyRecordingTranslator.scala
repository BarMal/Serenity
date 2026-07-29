package com.serenity.keystroke.translators

import com.serenity.keystroke.events.{CommandRunnerEvent, RunnerDismiss, RunnerRecordBinding}
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}

/** Converts the next physical key stroke into a command-runner binding record. */
class HotkeyRecordingTranslator extends Translator[CommandRunnerEvent]:

  override val converters: List[PartialFunction[KeyStrokeInfo, CommandRunnerEvent]] = List {
    case KeyStrokeInfo(InputKey.Escape, _, _) => RunnerDismiss
    case info                                 => RunnerRecordBinding(info)
  }
