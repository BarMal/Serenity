package com.serenity.command.menu

import com.serenity.command.{CommandId, CommandKeyBindings}
import com.serenity.config.{HotkeyConfig, HotkeyTrigger}
import com.serenity.keystroke.InputKey

object MenuAccelerators:

  /** Each command's first key a menu can show, so user overrides and unbindings come through unchanged. */
  def of(hotkeys: HotkeyConfig): Map[CommandId, HotkeyTrigger] =
    CommandKeyBindings.triggers(hotkeys).flatMap((id, keys) => keys.find(isRepresentable).map(id -> _))

  def isRepresentable(trigger: HotkeyTrigger): Boolean =
    !trigger.isBareModifierChord && trigger.keyType != InputKey.EOF && trigger.keyType != InputKey.Unknown &&
      trigger.keyType != InputKey.ReverseTab
