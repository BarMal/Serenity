package com.serenity.keystroke.events

sealed trait ThemeEvent

final case class SwitchTheme(themeName: String) extends ThemeEvent
case object ReloadCurrentTheme                  extends ThemeEvent
case object ListAvailableThemes                 extends ThemeEvent
