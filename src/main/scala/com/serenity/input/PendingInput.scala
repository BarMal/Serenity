package com.serenity.input

import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event

/** One input as an [[InputHandler]] queued it. A keystroke stays untranslated: which translator it reaches depends on
  * the focus the inputs queued ahead of it leave once applied.
  */
enum PendingInput:
  case Keystroke(info: KeyStrokeInfo)
  case Ready(event: Event)
