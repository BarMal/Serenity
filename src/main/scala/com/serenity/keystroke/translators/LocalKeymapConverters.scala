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

    // `HotkeyTrigger` is an exact-match key (see `HotkeyTrigger.matches`), so a `Map` gives O(1) dispatch in place of
    // the O(n) `collectFirst` scan this used to do per keystroke (issue #1465). Built with `foldLeft` rather than a
    // plain `.toMap` so that, on the rare conflicting binding (two actions sharing a trigger), the first one
    // encountered in `flattened` wins -- matching `collectFirst`'s original first-match-wins behavior -- instead of
    // `.toMap`'s last-write-wins.
    val lookup = flattened.foldLeft(Map.empty[HotkeyTrigger, E]) { case (acc, (trigger, event)) =>
      if acc.contains(trigger) then acc else acc + (trigger -> event)
    }

    Function.unlift(lookup.get)
