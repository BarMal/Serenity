package com.serenity.state.reducers

import com.serenity.state.models.*
import com.serenity.ui.theme.Theme

object ThemeStateReducer:

  def toggleTarget(state: AppState): String =
    state.persisted.theme.name match
      case "light"                                    => "dark"
      case "dark"                                     => "light"
      case "default-light"                            => "default-dark"
      case "default-dark"                             => "default-light"
      case name if name.toLowerCase.contains("light") => "default-dark"
      case _                                          => "default-light"

  def applyTheme(theme: Theme, state: AppState): ReducerResult =
    ReducerResult.noEffects(state.copy(persisted = state.persisted.copy(theme = theme)))

  def withAvailableThemeNames(names: List[String], state: AppState): ReducerResult =
    ReducerResult.noEffects(
      state.copy(runtime =
        state.runtime.copy(themeDiscovery = state.runtime.themeDiscovery.copy(availableThemeNames = names))
      )
    )

  def withRequestedTheme(themeName: String, state: AppState): ReducerResult =
    ReducerResult.noEffects(
      state.copy(runtime =
        state.runtime.copy(themeDiscovery = state.runtime.themeDiscovery.copy(requestedThemeName = Some(themeName)))
      )
    )

  /** [[applyTheme]] for a load that finished off the dispatcher -- dropped if a newer theme was requested meanwhile. */
  def applyRequestedTheme(requestedName: String, theme: Theme, state: AppState): AppState =
    if isLatestRequest(requestedName, state) then applyTheme(theme, state).state else state

  private def isLatestRequest(requestedName: String, state: AppState): Boolean =
    state.runtime.themeDiscovery.requestedThemeName.contains(requestedName)
