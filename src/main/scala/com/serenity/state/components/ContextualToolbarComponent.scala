package com.serenity.state.components

import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.*
import com.serenity.state.models.*

class ContextualToolbarComponent(registry: CommandRegistry) extends TypedFocusedComponent[ModalInputEvent]:

  protected def decodeEvent(event: Event): Option[ModalInputEvent] =
    ModalInputEvent.fromEvent(event)

  protected def processTypedEvent(event: ModalInputEvent, currentState: AppState): ComponentResult =
    currentState.contextualToolbarSurface match
      case None => ComponentResult.dismiss
      case Some(surface) =>
        surface.content match
          case SurfaceContent.ContextualToolbar(toolbarState) =>
            val items = ContextualToolbar.itemsFor(currentState)
            event match
              case ModalNavigate(Direction.Left) | ModalNavigate(Direction.Up) =>
                updateToolbarState(currentState, surface, toolbarState.moveFocus(-1, items))
              case ModalNavigate(Direction.Right) | ModalNavigate(Direction.Down) =>
                updateToolbarState(currentState, surface, toolbarState.moveFocus(1, items))
              case ModalSubmit =>
                ContextualToolbar
                  .focusedCommand(toolbarState, currentState, registry)
                  .map(ComponentResult.executeCommand)
                  .getOrElse(ComponentResult.noChange)
              case ModalDismiss =>
                ComponentResult.updateState(dismissToolbar)
              case _ =>
                ComponentResult.noChange
          case _ =>
            ComponentResult.noChange

  private def updateToolbarState(
    state: AppState,
    surface: UiSurface,
    toolbarState: ContextualToolbarState
  ): ComponentResult =
    ComponentResult.updateState { current =>
      val items          = ContextualToolbar.itemsFor(current)
      val normalized     = toolbarState.normalized(items)
      val updatedSurface = surface.copy(content = SurfaceContent.ContextualToolbar(normalized))
      current.copy(uiSurfaces = current.uiSurfaces.filterNot(_.id == surface.id) :+ updatedSurface)
    }

  private def dismissToolbar(state: AppState): AppState =
    state.contextualToolbarSurface match
      case Some(surface) =>
        state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id)).popFocus
      case None =>
        state
