package com.serenity.state.reducers

import com.serenity.config.AppConfigMotionOps.*
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

  /** A user-initiated switch animates from the outgoing theme; [[replaceTheme]] is the reload path, which doesn't. */
  def applyTheme(theme: Theme, state: AppState): ReducerResult =
    val transition =
      if state.persisted.theme == theme then None
      else
        state.persisted.config.scaledUiAnimation.map(config => ThemeTransition(state.persisted.theme, 0, config.steps))
    ReducerResult.noEffects(
      state.copy(
        persisted = state.persisted.copy(theme = theme),
        runtime = state.runtime.copy(themeTransition = transition)
      )
    )

  def replaceTheme(theme: Theme, state: AppState): ReducerResult =
    ReducerResult.noEffects(state.copy(persisted = state.persisted.copy(theme = theme)))

  def withAvailableThemeNames(names: List[String], state: AppState): ReducerResult =
    ReducerResult.noEffects(state.copy(runtime = state.runtime.copy(availableThemeNames = names)))

  def withRequestedTheme(themeName: String, state: AppState): ReducerResult =
    ReducerResult.noEffects(state.copy(runtime = state.runtime.copy(requestedThemeName = Some(themeName))))

  /** [[applyTheme]] for a load that finished off the dispatcher -- dropped if a newer theme was requested meanwhile. */
  def applyRequestedTheme(requestedName: String, theme: Theme, state: AppState): AppState =
    if isLatestRequest(requestedName, state) then applyTheme(theme, state).state else state

  /** [[replaceTheme]] for a reload that finished off the dispatcher, dropped like [[applyRequestedTheme]]. */
  def replaceRequestedTheme(requestedName: String, theme: Theme, state: AppState): AppState =
    if isLatestRequest(requestedName, state) then replaceTheme(theme, state).state else state

  private def isLatestRequest(requestedName: String, state: AppState): Boolean =
    state.runtime.requestedThemeName.contains(requestedName)
