package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ReducerResult, ThemeEffect}

class ThemePickerComponent extends TypedFocusedComponent[ModalInputEvent]:

  protected def decodeEvent(event: Event): Option[ModalInputEvent] =
    ModalInputEvent.fromEvent(event)

  protected def processTypedEvent(event: ModalInputEvent, currentState: AppState): ComponentResult =
    currentState.themePickerSurface match
      case None => ComponentResult.dismiss
      case Some(surface) =>
        surface.content match
          case SurfaceContent.ThemePicker(pickerState) =>
            event match
              case ModalNavigate(Direction.Up)   => navigate(currentState, surface, pickerState, -1)
              case ModalNavigate(Direction.Down) => navigate(currentState, surface, pickerState, 1)
              case ModalSubmit => ComponentResult.updateState(_ => dismissToRunner(currentState, surface))
              case ModalDismiss =>
                val dismissed = dismissToRunner(currentState, surface)
                ComponentResult.reducerResult(
                  ReducerResult(dismissed, List(AppEffect.Theme(ThemeEffect.SwitchTheme(pickerState.originalTheme))))
                )
              case _ => ComponentResult.noChange
          case _ => ComponentResult.noChange

  private def navigate(
    state: AppState,
    surface: UiSurface,
    pickerState: ThemePickerState,
    delta: Int
  ): ComponentResult =
    val newPicker  = pickerState.moveSelection(delta)
    val newSurface = surface.copy(content = SurfaceContent.ThemePicker(newPicker))
    val newState = state.copy(runtime =
      state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id) :+ newSurface)
    )
    newPicker.selectedTheme match
      case Some(name) =>
        ComponentResult.reducerResult(ReducerResult(newState, List(AppEffect.Theme(ThemeEffect.SwitchTheme(name)))))
      case None =>
        ComponentResult.updateState(_ => newState)

  private def dismissToRunner(state: AppState, surface: UiSurface): AppState =
    val withoutPicker =
      state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id)))
    withoutPicker.commandRunnerSurface match
      case Some(runnerSurface) =>
        withoutPicker.copy(persisted = withoutPicker.persisted.copy(focus = Focus.Surface(runnerSurface.id)))
      case None =>
        withoutPicker.persisted.layout.activeEditorPaneId match
          case Some(paneId) =>
            withoutPicker.copy(persisted = withoutPicker.persisted.copy(focus = Focus.EditorPane(paneId)))
          case None => withoutPicker
