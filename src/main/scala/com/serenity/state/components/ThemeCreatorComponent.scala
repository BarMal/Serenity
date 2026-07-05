package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ReducerResult}

class ThemeCreatorComponent extends TypedFocusedComponent[ModalInputEvent]:

  protected def decodeEvent(event: Event): Option[ModalInputEvent] =
    ModalInputEvent.fromEvent(event)

  protected def processTypedEvent(event: ModalInputEvent, state: AppState): ComponentResult =
    state.themeCreatorSurface match
      case None => ComponentResult.dismiss
      case Some(surface) =>
        surface.content match
          case SurfaceContent.ThemeCreator(creatorState) =>
            event match
              case ModalInsertChar(char) =>
                ComponentResult.updateState(_ => replaceCreator(state, surface, creatorState.insertChar(char)))
              case ModalDeleteBackward =>
                ComponentResult.updateState(_ => replaceCreator(state, surface, creatorState.deleteBackward))
              case ModalNavigate(Direction.Up) =>
                ComponentResult.updateState(_ => replaceSurface(state, surface, creatorState.moveSelection(-1)))
              case ModalNavigate(Direction.Down) =>
                ComponentResult.updateState(_ => replaceSurface(state, surface, creatorState.moveSelection(1)))
              case ModalSubmit =>
                submit(state, surface, creatorState)
              case ModalDismiss =>
                ComponentResult.updateState(_ => dismissAndRestore(state, surface, creatorState))
              case _ =>
                ComponentResult.noChange
          case _ =>
            ComponentResult.noChange

  private def submit(
    state: AppState,
    surface: UiSurface,
    creatorState: com.serenity.ui.theme.config.ThemeCreatorState
  ): ComponentResult =
    creatorState.validConfig match
      case Right(config) =>
        val savedState = dismiss(state.copy(theme = creatorState.previewTheme.toOption.getOrElse(state.theme)), surface)
        ComponentResult.reducerResult(ReducerResult(savedState, List(AppEffect.SaveThemeConfig(config))))
      case Left(error) =>
        ComponentResult.updateState(_ => replaceSurface(state, surface, creatorState.withStatus(error)))

  private def replaceCreator(
    state: AppState,
    surface: UiSurface,
    creatorState: com.serenity.ui.theme.config.ThemeCreatorState
  ): AppState =
    val withSurface = replaceSurface(state, surface, creatorState)
    creatorState.previewTheme match
      case Right(theme) => withSurface.copy(theme = theme)
      case Left(_)      => withSurface

  private def replaceSurface(
    state: AppState,
    surface: UiSurface,
    creatorState: com.serenity.ui.theme.config.ThemeCreatorState
  ): AppState =
    state.copy(uiSurfaces = state.uiSurfaces.map {
      case current if current.id == surface.id =>
        current.copy(content = SurfaceContent.ThemeCreator(creatorState))
      case current =>
        current
    })

  private def dismissAndRestore(
    state: AppState,
    surface: UiSurface,
    creatorState: com.serenity.ui.theme.config.ThemeCreatorState
  ): AppState =
    dismiss(state.copy(theme = creatorState.originalTheme), surface)

  private def dismiss(state: AppState, surface: UiSurface): AppState =
    state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id)).popFocus

end ThemeCreatorComponent
