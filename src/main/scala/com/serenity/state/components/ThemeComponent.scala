package com.serenity.state.components

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.state.models.AppState
import com.serenity.ui.theme.config.AppThemeManager

class ThemeComponent(themeManager: AppThemeManager) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case SwitchTheme(themeName) =>
        // Switch to the specified theme
        val (newTheme, stateUpdate) = themeManager.switchTheme(themeName).unsafeRunSync()
        ComponentResult.updateState(stateUpdate)

      case ReloadCurrentTheme =>
        // Reload the current theme configuration
        themeManager.reloadCurrentTheme.unsafeRunSync() match
          case Some((newTheme, stateUpdate)) => ComponentResult.updateState(stateUpdate)
          case None => ComponentResult.noChange

      case ListAvailableThemes =>
        // For now, just log available themes - in future could show in modal
        val themes = themeManager.listAvailableThemes.unsafeRunSync()
        println(s"Available themes: ${themes.mkString(", ")}")
        ComponentResult.noChange

      case _ => ComponentResult.noChange