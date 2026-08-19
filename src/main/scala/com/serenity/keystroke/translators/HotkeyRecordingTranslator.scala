package com.serenity.keystroke.translators

import com.serenity.keystroke.events.{CommandRunnerEvent, RunnerDismiss, RunnerRecordBinding}
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}

/** Converts the next physical key stroke into a command-runner binding record.
  *
  * `now` is supplied by the caller so the stamp is visible at the construction site rather than hidden in the event's
  * constructor, and so a test can freeze it.
  */
class HotkeyRecordingTranslator(now: () => Long) extends Translator[CommandRunnerEvent]:

  override val converters: List[PartialFunction[KeyStrokeInfo, CommandRunnerEvent]] = List {
    case KeyStrokeInfo(InputKey.Escape, _, _) => RunnerDismiss
    case info                                 => RunnerRecordBinding(info, now())
  }
