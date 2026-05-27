package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyStroke
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.{Event, UnhandledEvent}

trait Translator[+T <: Event]:

  def converters: List[PartialFunction[KeyStrokeInfo, T]]

  def translate(keyStroke: KeyStroke): Event =
    val keyStrokeInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
    converters
      .find(_.isDefinedAt(keyStrokeInfo))
      .map(_(keyStrokeInfo))
      .getOrElse(UnhandledEvent(keyStroke, this))
