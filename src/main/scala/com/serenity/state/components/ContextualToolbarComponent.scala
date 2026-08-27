package com.serenity.state.components

import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.{ContextualToolbarLayout, EditorLayoutContract, LayoutEngine, SurfaceFrameLayout}

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
              case ModalNavigate(Direction.Left) =>
                toolbarState.normalized(items).detailState match
                  case Some(_: ContextualToolbarDetailState.Dropdown) =>
                    updateToolbarState(surface, toolbarState.moveDetailSelection(-1, items))
                  case _ =>
                    updateToolbarState(surface, toolbarState.moveFocus(-1, items))
              case ModalNavigate(Direction.Right) =>
                toolbarState.normalized(items).detailState match
                  case Some(_: ContextualToolbarDetailState.Dropdown) =>
                    updateToolbarState(surface, toolbarState.moveDetailSelection(1, items))
                  case _ =>
                    updateToolbarState(surface, toolbarState.moveFocus(1, items))
              case ModalNavigate(Direction.Up) =>
                toolbarState.normalized(items).detailState match
                  case Some(_: ContextualToolbarDetailState.Dropdown) =>
                    updateToolbarState(
                      surface,
                      moveDropdownVertical(currentState, surface, toolbarState, items, deltaRows = -1)
                    )
                  case _ =>
                    updateToolbarState(surface, moveTopLevelVertical(currentState, surface, toolbarState, items, -1))
              case ModalNavigate(Direction.Down) =>
                toolbarState.normalized(items).detailState match
                  case Some(_: ContextualToolbarDetailState.Dropdown) =>
                    updateToolbarState(
                      surface,
                      moveDropdownVertical(currentState, surface, toolbarState, items, deltaRows = 1)
                    )
                  case _ =>
                    updateToolbarState(surface, moveTopLevelVertical(currentState, surface, toolbarState, items, 1))
              case ModalInsertChar(char) =>
                toolbarState.normalized(items).detailState match
                  case Some(_: ContextualToolbarDetailState.Input) =>
                    updateToolbarState(surface, toolbarState.insertDetailChar(char, items))
                  case _ =>
                    ComponentResult.noChange
              case ModalDeleteBackward =>
                toolbarState.normalized(items).detailState match
                  case Some(_: ContextualToolbarDetailState.Input) =>
                    updateToolbarState(surface, toolbarState.deleteDetailBackward(items))
                  case _ =>
                    ComponentResult.noChange
              case ModalSubmit =>
                submitFocusedControl(currentState, surface, toolbarState, items)
              case ModalDismiss =>
                toolbarState.normalized(items).detailState match
                  case Some(_) =>
                    updateToolbarState(surface, toolbarState.closeDetail)
                  case None =>
                    ComponentResult.updateState(dismissToolbar)
              case _ =>
                ComponentResult.noChange
          case _ =>
            ComponentResult.noChange

  private def submitFocusedControl(
    state: AppState,
    surface: UiSurface,
    toolbarState: ContextualToolbarState,
    items: List[ContextualToolbarItem]
  ): ComponentResult =
    toolbarState.normalized(items).detailState match
      case Some(_) =>
        ContextualToolbar.detailCommand(toolbarState, state) match
          case Some(command) =>
            ComponentResult.composite(
              updateToolbarState(surface, toolbarState.closeDetail),
              ComponentResult.executeCommand(command),
              ComponentResult.transferFocus(editorFocus(state))
            )
          case None =>
            ComponentResult.noChange
      case None =>
        toolbarState.normalized(items).focusedItem(items) match
          case Some(_: ContextualToolbarItem.Button) =>
            ContextualToolbar
              .focusedCommand(toolbarState, state, registry)
              .map(command =>
                ComponentResult.composite(
                  ComponentResult.executeCommand(command),
                  ComponentResult.transferFocus(editorFocus(state))
                )
              )
              .getOrElse(ComponentResult.noChange)
          case Some(_: ContextualToolbarItem.Dropdown) | Some(_: ContextualToolbarItem.Input) =>
            updateToolbarState(surface, toolbarState.openFocusedDetail(items))
          case None =>
            ComponentResult.noChange

  private def updateToolbarState(
    surface: UiSurface,
    toolbarState: ContextualToolbarState
  ): ComponentResult =
    ComponentResult.updateState { current =>
      val items          = ContextualToolbar.itemsFor(current)
      val normalized     = toolbarState.normalized(items)
      val updatedSurface = surface.copy(content = SurfaceContent.ContextualToolbar(normalized))
      current.copy(runtime =
        current.runtime.copy(uiSurfaces = current.runtime.uiSurfaces.filterNot(_.id == surface.id) :+ updatedSurface)
      )
    }

  private def moveTopLevelVertical(
    state: AppState,
    surface: UiSurface,
    toolbarState: ContextualToolbarState,
    items: List[ContextualToolbarItem],
    deltaRows: Int
  ): ContextualToolbarState =
    toolbarContentWidth(state, surface, toolbarState) match
      case Some(width) =>
        ContextualToolbarLayout.focusedIndexAfterVerticalMove(toolbarState, items, width, deltaRows) match
          case Some(index) => toolbarState.withFocusedIndexClearingDetail(index, items)
          case None        => toolbarState
      case None =>
        toolbarState.moveFocus(deltaRows, items)

  private def moveDropdownVertical(
    state: AppState,
    surface: UiSurface,
    toolbarState: ContextualToolbarState,
    items: List[ContextualToolbarItem],
    deltaRows: Int
  ): ContextualToolbarState =
    toolbarContentWidth(state, surface, toolbarState) match
      case Some(width) =>
        ContextualToolbarLayout.detailSelectionAfterVerticalMove(toolbarState, items, width, deltaRows) match
          case Some((itemId, index)) => toolbarState.withDetailSelectionIndex(itemId, index)
          case None                  => toolbarState
      case None =>
        toolbarState.moveDetailSelection(deltaRows, items)

  private def toolbarContentWidth(
    state: AppState,
    surface: UiSurface,
    toolbarState: ContextualToolbarState
  ): Option[Int] =
    for
      viewport <- state.runtime.viewportSize
      rect <-
        val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
        val contract = EditorLayoutContract.from(state, viewport, layout)
        contract.overlayRect(surface.id)
    yield SurfaceFrameLayout
      .forContent(rect, SurfaceContent.ContextualToolbar(toolbarState))
      .contentRect
      .width
      .max(1)

  private def dismissToolbar(state: AppState): AppState =
    state.contextualToolbarSurface match
      case Some(surface) =>
        state
          .copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id)))
          .popFocus
      case None =>
        state

  private def editorFocus(state: AppState): Focus =
    state.persisted.layout.activeEditorPaneId
      .map(Focus.EditorPane.apply)
      .getOrElse(Focus.EditorPane(PaneId(0)))
