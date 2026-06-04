package com.serenity.keystroke.translators

import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

object TextDeletionConverters:

  val deletionConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(InputKey.Backspace, _, modifiers) if modifiers.contains(Modifier.Ctrl) => DeleteWordBackward
    case KeyStrokeInfo(InputKey.Delete, _, modifiers) if modifiers.contains(Modifier.Ctrl)    => DeleteWordForward
    case KeyStrokeInfo(InputKey.Backspace, _, _) => DeleteBackward
    case KeyStrokeInfo(InputKey.Delete, _, _)    => DeleteForward
    case KeyStrokeInfo(InputKey.Escape, _, _)    => Escape
  }
