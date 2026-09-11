package com.serenity.keystroke.translators

import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event

class CompositeTranslator(
    translators: List[Translator[? <: Event]]
) extends Translator[Event]:

  override lazy val converters: List[PartialFunction[KeyStrokeInfo, Event]] =
    translators.flatMap(_.converters)

object CompositeTranslator:

  def apply(translators: Translator[? <: Event]*): CompositeTranslator =
    new CompositeTranslator(translators.toList)
