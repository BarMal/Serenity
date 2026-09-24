package com.serenity.state.reducers

import com.serenity.state.models.*
import com.serenity.ui.theme.config.ThemeCreatorState

/** Floating popups opened below the cursor by commands: the theme picker, the theme creator and file search. */
object PopupSurfaceReducer:

  /** `None` until the theme list has loaded: an empty picker would have nothing to select. */
  def openThemePicker(themeNames: List[String], state: AppState): Option[ReducerResult] =
    Option.when(themeNames.nonEmpty) {
      val currentTheme = state.persisted.theme.name
      val pickerState  = ThemePickerState(themeNames, themeNames.indexOf(currentTheme).max(0), currentTheme)
      openFocused(SurfaceContent.ThemePicker(pickerState), state)
    }

  def openThemeCreator(state: AppState): ReducerResult =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val withoutCreator = stateWithId.runtime.uiSurfaces.filterNot {
      _.content match
        case SurfaceContent.ThemeCreator(_) => true
        case _                              => false
    }
    val creator = belowCursor(surfaceId, SurfaceContent.ThemeCreator(ThemeCreatorState.fromTheme(state.persisted.theme)), state)
    ReducerResult.noEffects(
      stateWithId
        .copy(runtime = stateWithId.runtime.copy(uiSurfaces = withoutCreator :+ creator))
        .pushFocus(Focus.Surface(surfaceId))
    )

  def openFileSearch(state: AppState): ReducerResult =
    openFocused(SurfaceContent.FileSearch(FileSearchState("", Nil, 0)), state)

  private def openFocused(content: SurfaceContent, state: AppState): ReducerResult =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    ReducerResult.noEffects(
      stateWithId.copy(
        persisted = stateWithId.persisted.copy(focus = Focus.Surface(surfaceId)),
        runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ belowCursor(surfaceId, content, state))
      )
    )

  private def belowCursor(surfaceId: SurfaceId, content: SurfaceContent, state: AppState): UiSurface =
    UiSurface(
      id = surfaceId,
      content = content,
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
