package com.serenity.keystroke.events

import com.googlecode.lanterna.input.KeyStroke
import com.serenity.keystroke.translators.Translator

case class UnhandledEvent[T <: Translator[?]](
    keyStroke: KeyStroke,
    handler: T
) extends SystemEvent
