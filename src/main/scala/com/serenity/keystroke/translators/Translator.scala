package com.serenity.keystroke.translators

import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.{Event, UnhandledEvent}

trait Translator[+T <: Event]:

  def converters: List[PartialFunction[KeyStrokeInfo, T]]

  def translate(info: KeyStrokeInfo): Event =
    converters
      .find(_.isDefinedAt(info))
      .map(_(info))
      .getOrElse(UnhandledEvent(info, this))
