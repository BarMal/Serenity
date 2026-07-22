package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, SurfaceContent}

class StartupPageComponent extends TypedFocusedComponent[StartupPageEvent]:

  protected def decodeEvent(event: Event): Option[StartupPageEvent] =
    StartupPageEvent.fromEvent(event)

  protected def processTypedEvent(event: StartupPageEvent, currentState: AppState): ComponentResult =
    currentState.startPageSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.StartPage(startPage) =>
            event match
              case StartupPageMoveUp =>
                val updatedPage = startPage.moveSelectionUp
                ComponentResult.updateState(updateStartPage(surface.id, updatedPage))
              case StartupPageMoveDown =>
                val updatedPage = startPage.moveSelectionDown
                ComponentResult.updateState(updateStartPage(surface.id, updatedPage))
              case StartupPageSubmit =>
                executeSelectedAction(startPage)
              case StartupPageSelect(index) =>
                startPage.launchActions
                  .lift(index)
                  .fold(ComponentResult.noChange)(action => ComponentResult.executeCommand(action.command))
              case StartupPageDismiss =>
                ComponentResult.dismiss
          case _ => ComponentResult.noChange
      case None => ComponentResult.noChange

  private def updateStartPage(
    surfaceId: com.serenity.state.models.SurfaceId,
    updatedPage: com.serenity.state.models.StartupPage
  )(state: AppState): AppState =
    val updatedSurfaces = state.uiSurfaces.map { surface =>
      if surface.id == surfaceId then surface.copy(content = SurfaceContent.StartPage(updatedPage))
      else surface
    }
    state.copy(uiSurfaces = updatedSurfaces)

  private def executeSelectedAction(startPage: com.serenity.state.models.StartupPage): ComponentResult =
    startPage.selectedAction.fold(ComponentResult.noChange)(action => ComponentResult.executeCommand(action.command))
