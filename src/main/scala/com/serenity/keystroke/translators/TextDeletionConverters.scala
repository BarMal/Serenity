package com.serenity.keystroke.translators

import com.serenity.keystroke.InputKey
import com.serenity.keystroke.events.{DeleteBackward, DeleteForward, Escape, TextEntryEvent}
import com.serenity.keystroke.KeyStrokeInfo

object TextDeletionConverters:

  val deletionConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(InputKey.Backspace, _, _) => DeleteBackward
    case KeyStrokeInfo(InputKey.Delete, _, _)    => DeleteForward
    case KeyStrokeInfo(InputKey.Escape, _, _)    => Escape
  }
