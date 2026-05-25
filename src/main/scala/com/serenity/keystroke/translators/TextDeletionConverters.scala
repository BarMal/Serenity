package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.events.{DeleteBackward, DeleteForward, Escape, TextEntryEvent}
import com.serenity.keystroke.KeyStrokeInfo

object TextDeletionConverters:

  val deletionConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(KeyType.Backspace, _, _) => DeleteBackward
    case KeyStrokeInfo(KeyType.Delete, _, _)    => DeleteForward
    case KeyStrokeInfo(KeyType.Escape, _, _)    => Escape
  }
