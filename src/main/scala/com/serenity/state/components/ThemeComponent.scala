package com.serenity.state.components

import cats.effect.IO
import com.serenity.command.Command
import com.serenity.keystroke.events.*
import com.serenity.state.models.AppState
import com.serenity.ui.theme.config.AppThemeManager

class ThemeComponent(themeManager: AppThemeManager) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case SwitchTheme(themeName) =>
        // Create a command to switch theme
        val switchCommand = Command(
          name = s"Switch to theme: $themeName",
          description = s"Switch the application theme to $themeName",
          action = _ =>
            themeManager
              .switchTheme(themeName)
              .flatMap {
                case (newTheme, stateUpdate) =>
                  IO.blocking {
                    println(s"[THEME] Switched to theme: $themeName")
                  }
              }
              .handleError(ex => println(s"[THEME] Error switching theme: ${ex.getMessage}"))
        )
        ComponentResult.executeCommand(switchCommand)

      case ReloadCurrentTheme =>
        // Create a command to reload current theme
        val reloadCommand = Command(
          name = "Reload current theme",
          description = "Reload the current theme configuration",
          action = _ =>
            themeManager.reloadCurrentTheme
              .flatMap {
                case Some((newTheme, stateUpdate)) =>
                  IO.blocking {
                    println("[THEME] Theme reloaded successfully")
                  }
                case None =>
                  IO.blocking {
                    println("[THEME] No theme changes detected")
                  }
              }
              .handleError(ex => println(s"[THEME] Error reloading theme: ${ex.getMessage}"))
        )
        ComponentResult.executeCommand(reloadCommand)

      case ListAvailableThemes =>
        // Create a command to list themes
        val listCommand = Command(
          name = "List available themes",
          description = "Display all available themes",
          action = _ =>
            themeManager.listAvailableThemes
              .flatMap { themes =>
                IO.blocking {
                  println(s"Available themes: ${themes.mkString(", ")}")
                }
              }
              .handleError(ex => println(s"[THEME] Error listing themes: ${ex.getMessage}"))
        )
        ComponentResult.executeCommand(listCommand)

      case _ => ComponentResult.noChange
