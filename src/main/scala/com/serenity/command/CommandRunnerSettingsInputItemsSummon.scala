package com.serenity.command

import com.serenity.config.{ChromeElement, SummonConfig}

/** The rows that set the keys that summon hidden chrome: the base chord, the lock key and one letter per element. */
private[command] object CommandRunnerSettingsInputItemsSummon:

  private def chrome(intent: InterfaceChromeIntent): CommandIntent =
    CommandIntent.Settings(SettingsIntent.InterfaceChrome(intent))

  private val defaults = SummonConfig()

  private[command] def items(summon: SummonConfig): List[CommandSurfaceItem.InputItem] =
    modifiersItem(summon) :: lockItem(summon) :: ChromeElement.values.toList.map(letterItem(summon, _))

  private def modifiersItem(summon: SummonConfig): CommandSurfaceItem.InputItem =
    CommandSurfaceItem.InputItem(
      id = "summon-modifiers",
      label = "Summon Chord",
      hint = "Modifiers held with an element's letter, such as alt+shift or ctrl+alt",
      currentValue = SummonConfig.renderModifiers(summon.modifiers),
      kind = CommandSurfaceItem.InputKind.Binding,
      parse =
        text => SummonConfig.parseModifiers(text).map(mods => chrome(InterfaceChromeIntent.SetSummonModifiers(mods))),
      category = CommandCategory.Settings,
      defaultValue = Some(SummonConfig.renderModifiers(defaults.modifiers))
    )

  private def lockItem(summon: SummonConfig): CommandSurfaceItem.InputItem =
    CommandSurfaceItem.InputItem(
      id = "summon-lock",
      label = "Summon Lock Key",
      hint = "Pressed while holding a chord to keep the element shown: space, enter, tab, backspace or a letter",
      currentValue = SummonConfig.renderLock(summon.lock),
      kind = CommandSurfaceItem.InputKind.Binding,
      parse = text => SummonConfig.parseLock(text).map(lock => chrome(InterfaceChromeIntent.SetSummonLock(lock))),
      category = CommandCategory.Settings,
      defaultValue = Some(SummonConfig.renderLock(defaults.lock))
    )

  private def letterItem(summon: SummonConfig, element: ChromeElement): CommandSurfaceItem.InputItem =
    val name = element.configKey.split('_').map(_.capitalize).mkString(" ")
    CommandSurfaceItem.InputItem(
      id = s"summon-${element.configKey.replace('_', '-')}",
      label = s"Summon $name",
      hint = s"Letter held with the summon chord to show the ${name.toLowerCase}, or off",
      currentValue = SummonConfig.renderLetter(summon.letterOf(element)),
      kind = CommandSurfaceItem.InputKind.Binding,
      parse = text =>
        SummonConfig.parseLetter(text).map(letter => chrome(InterfaceChromeIntent.SetSummonLetter(element, letter))),
      category = CommandCategory.Settings,
      defaultValue = Some(SummonConfig.renderLetter(defaults.letterOf(element)))
    )
