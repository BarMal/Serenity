package com.serenity.state.reducers

import com.serenity.command.ThemeCommands
import com.serenity.state.models.*
import com.serenity.ui.layout.ListPickerComposition
import com.serenity.ui.theme.config.ThemeCreatorState
import com.serenity.ui.widget.Loadable

/** Floating popups opened below the cursor by commands: the theme picker and the theme creator. */
object PopupSurfaceReducer:

  /** A filterable list of `themeNames` that previews each theme as it is highlighted, opened on the current theme and
    * restoring it on Escape. `None` when there are no names: the list is read from disk once at startup and only
    * re-read on demand, so a chooser opened empty would wait on a listing nobody asked for.
    */
  def openThemePicker(themeNames: List[String], state: AppState): Option[ReducerResult] =
    Option.when(themeNames.nonEmpty) {
      val current = state.persisted.theme.name
      val chooser = ListPicker.filterable(
        "Theme",
        themeNames.map(themeChoice(_, current)),
        onDismiss = Some(ThemeCommands.applyTheme(current))
      )
      ModalStateReducer.show(Modal.ListPicker(highlighting(chooser, themeNames.indexOf(current))), state)
    }

  private def themeChoice(name: String, current: String): ListChoice =
    val apply = ThemeCommands.applyTheme(name)
    ListChoice(name, Option.when(name == current)("current"), apply, preview = Some(apply))

  private def highlighting(picker: ListPicker, index: Int): ListPicker =
    picker.items match
      case Loadable.Ready(choices) if index >= 0 =>
        picker.copy(items = Loadable.Ready(choices.select(index, ListPickerComposition.VisibleRows)))
      case _ => picker

  def openThemeCreator(state: AppState): ReducerResult =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val withoutCreator = stateWithId.runtime.uiSurfaces.filterNot {
      _.content match
        case SurfaceContent.ThemeCreator(_) => true
        case _                              => false
    }
    val creatorContent = SurfaceContent.ThemeCreator(ThemeCreatorState.fromTheme(state.persisted.theme))
    val creator        = belowCursor(surfaceId, creatorContent, state)
    ReducerResult.noEffects(
      stateWithId
        .copy(runtime = stateWithId.runtime.copy(uiSurfaces = withoutCreator :+ creator))
        .pushFocus(Focus.Surface(surfaceId))
    )

  private def openFocused(content: SurfaceContent, state: AppState): ReducerResult =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val popup                    = belowCursor(surfaceId, content, state)
    ReducerResult.noEffects(
      stateWithId.copy(
        persisted = stateWithId.persisted.copy(focus = Focus.Surface(surfaceId)),
        runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ popup)
      )
    )

  private def belowCursor(surfaceId: SurfaceId, content: SurfaceContent, state: AppState): UiSurface =
    UiSurface(
      id = surfaceId,
      content = content,
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
