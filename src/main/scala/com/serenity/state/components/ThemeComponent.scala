package com.serenity.state.components

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.state.models.AppState
import com.serenity.ui.theme.config.AppThemeManager

class ThemeComponent(themeManager: AppThemeManager) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case SwitchTheme(themeName) =>
        themeManager
          .switchTheme(themeName)
          .map { case (_, stateUpdate) => ComponentResult.updateState(stateUpdate) }
          .handleError(_ => ComponentResult.noChange)
          .unsafeRunSync()

      case ReloadCurrentTheme =>
        themeManager.reloadCurrentTheme
          .map {
            case Some((_, stateUpdate)) => ComponentResult.updateState(stateUpdate)
            case None                   => ComponentResult.noChange
          }
          .handleError(_ => ComponentResult.noChange)
          .unsafeRunSync()

      case ListAvailableThemes =>
        ComponentResult.noChange

      case _ => ComponentResult.noChange
