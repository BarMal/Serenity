package com.serenity.state.components

import com.serenity.command.{Command, CommandIntent}
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
                createCommandForSelection(startPage.selectedIndex)
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

  private def createCommandForSelection(selectedIndex: Int): ComponentResult =
    val intent = selectedIndex match
      case 0 => CommandIntent.StartupNewSession
      case 1 => CommandIntent.StartupRestoreSession
      case 2 => CommandIntent.StartupOpenFile
      case _ => CommandIntent.StartupNewSession // Default fallback

    val command = selectedIndex match
      case 0 => Command.typed("startup.new-session", "Start a new session", intent)
      case 1 => Command.typed("startup.restore-session", "Restore an existing session", intent)
      case 2 => Command.typed("startup.open-file", "Open an existing file or directory", intent)
      case _ => Command.typed("startup.new-session", "Start a new session", intent)

    ComponentResult.executeCommand(command)
