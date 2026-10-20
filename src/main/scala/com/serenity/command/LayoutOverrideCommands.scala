package com.serenity.command

import com.serenity.config.{LayoutPin, UiLayout}

/** What the answers to a layout-override prompt run. Not in the palette: each exists to be run by a prompt that holds
  * the layout and the keys it asked about.
  */
object LayoutOverrideCommands:

  def useLayout(layout: UiLayout, pins: List[LayoutPin]): Command =
    confirm(layout, pins.map(_.key), s"Use the ${layout.configKey} layout for every setting the config file pins.")

  def keepMine(layout: UiLayout): Command =
    confirm(layout, Nil, s"Switch to the ${layout.configKey} layout and keep the settings the config file pins.")

  def chooseEach(layout: UiLayout): Command =
    Command.typed(
      "layout.review-override",
      s"Choose, setting by setting, whether the ${layout.configKey} layout replaces what the config file pins.",
      intent(InterfaceChromeIntent.ReviewUiLayoutOverride(layout))
    )

  private def confirm(layout: UiLayout, useLayoutKeys: List[String], description: String): Command =
    Command.typed(
      "layout.confirm-override",
      description,
      intent(InterfaceChromeIntent.ConfirmUiLayoutOverride(layout, useLayoutKeys))
    )

  private def intent(chrome: InterfaceChromeIntent): CommandIntent =
    CommandIntent.Settings(SettingsIntent.InterfaceChrome(chrome))
