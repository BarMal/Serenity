package com.serenity.state.models

/** Theme discovery/loading state one grab-bag category of `Runtime` used to hold directly (issue #1693): the theme
  * names found on disk (`availableThemeNames`) and the most recently requested theme name -- used to drop a load that
  * finishes after a newer request superseded it (`ThemeStateReducer.isLatestRequest`).
  */
final case class ThemeDiscoveryState(
    availableThemeNames: List[String] = Nil,
    requestedThemeName: Option[String] = None
)
