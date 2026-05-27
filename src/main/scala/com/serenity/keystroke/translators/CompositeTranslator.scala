package com.serenity.keystroke.translators

import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event

class CompositeTranslator(
    translators: List[Translator[? <: Event]]
) extends Translator[Event]:

  override def converters: List[PartialFunction[KeyStrokeInfo, Event]] =
    translators.flatMap(_.converters.map(_.andThen(identity[Event])))

object CompositeTranslator:

  def apply(translators: Translator[? <: Event]*): CompositeTranslator =
    new CompositeTranslator(translators.toList)
