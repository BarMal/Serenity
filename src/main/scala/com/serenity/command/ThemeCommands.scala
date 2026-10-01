package com.serenity.command

/** What picking a theme runs, from the settings menu or the theme chooser. */
object ThemeCommands:

  def applyTheme(name: String): Command =
    Command.typed(
      s"theme-${name.toLowerCase.replaceAll("[^a-z0-9]+", "-")}",
      s"Switch to the $name theme.",
      CommandIntent.Theme(ThemeIntent.ApplyTheme(name)),
      CommandCategory.Settings,
      label = name
    )
