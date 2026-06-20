package com.serenity.keystroke.translators

import com.serenity.config.*
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event

object LocalKeymapConverters:

  def converter[A <: KeymapEventAction[E], E <: Event](
    bindings: Map[A, List[HotkeyTrigger]]
  ): PartialFunction[KeyStrokeInfo, E] =
    val flattened =
      bindings.toList.flatMap((action, triggers) => triggers.map(_ -> action.event))

    Function.unlift(info => flattened.collectFirst { case (trigger, event) if trigger.matches(info) => event })
