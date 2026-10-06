package com.serenity.config

import com.serenity.ui.theme.appearance.OsAppearance

/** Whether the theme tracks the operating system's light, dark or high-contrast setting, and which theme each maps to.
  * Off by default so a theme the person picked is never replaced behind their back.
  */
final case class ThemeFollowConfig(
    followSystem: Boolean = false,
    lightTheme: String = "light",
    darkTheme: String = "dark",
    highContrastTheme: String = "high-contrast"
):

  /** `None` leaves the current theme alone: not following, or the OS did not say. */
  def themeFor(appearance: OsAppearance): Option[String] =
    Option.when(followSystem)(appearance).flatMap {
      case OsAppearance.Light        => Some(lightTheme)
      case OsAppearance.Dark         => Some(darkTheme)
      case OsAppearance.HighContrast => Some(highContrastTheme)
      case OsAppearance.Unknown      => None
    }
