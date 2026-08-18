package com.serenity.keystroke.events

import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.translators.Translator

final case class UnhandledEvent[T <: Translator[?]](
    info: KeyStrokeInfo,
    handler: T
) extends SystemEvent
