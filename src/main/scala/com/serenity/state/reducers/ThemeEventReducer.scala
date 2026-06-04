package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.AppState

object ThemeEventReducer:

  def reduce(event: ThemeEvent, state: AppState): ReducerResult =
    event match
      case SwitchTheme(themeName) =>
        ReducerResult.withEffect(state, AppEffect.SwitchTheme(themeName))
      case ReloadCurrentTheme =>
        ReducerResult.withEffect(state, AppEffect.ReloadTheme(state.theme.name))
      case ListAvailableThemes =>
        ReducerResult.withEffect(state, AppEffect.OpenThemePicker())
