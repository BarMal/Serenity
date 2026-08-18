package com.serenity.keystroke.events

/** Events related to theme management */
trait ThemeEvent extends AppEvent

final case class SwitchTheme(themeName: String) extends ThemeEvent
case object ReloadCurrentTheme                  extends ThemeEvent
case object ListAvailableThemes                 extends ThemeEvent
