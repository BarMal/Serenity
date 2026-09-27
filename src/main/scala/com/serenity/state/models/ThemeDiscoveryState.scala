package com.serenity.state.models

/** Theme discovery/loading/transition state one grab-bag category of `Runtime` used to hold directly (issue #1693): the
  * theme names found on disk (`availableThemeNames`), the most recently requested theme name -- used to drop a load
  * that finishes after a newer request superseded it (`ThemeStateReducer.isLatestRequest`) -- and the in-flight
  * theme-switch transition consumed by the render/tick paths (`RendererEntryPoints.withEffectiveTheme`,
  * `StateManagerEditorCapability`'s tick-active check and advance). All three are written together from
  * `ThemeStateReducer`/`StateManagerSurfacePopupEffects`'s theme-listing effect and read back together wherever a frame
  * or tick needs to know whether a theme is still loading or blending in.
  */
final case class ThemeDiscoveryState(
    availableThemeNames: List[String] = Nil,
    requestedThemeName: Option[String] = None,
    transition: Option[ThemeTransition] = None
)
