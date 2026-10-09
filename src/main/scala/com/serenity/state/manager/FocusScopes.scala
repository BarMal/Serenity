package com.serenity.state.manager

import com.serenity.keystroke.events.*
import com.serenity.state.components.ComponentResult
import com.serenity.state.models.*

/** What a key does to the transient surfaces around the focused scope, and where it goes once that scope declines it
  * (#1940): a focused surface's unhandled key goes on to the editor pane, and a peek never holds focus at all.
  */
private[manager] object FocusScopes:

  enum PeekKeyOutcome:
    case Consumed(state: AppState)
    case PassedOn(state: AppState)

  /** Escape closes every peek and goes no further; any other key closes the peeks that last only until the user acts,
    * then still reaches the focused scope. A scope that keeps every key keeps Escape too, and a peek that somehow holds
    * focus is left to its own handler.
    */
  def peekKeyOutcome(event: Event, state: AppState): PeekKeyOutcome =
    def unfocusedPeek(surface: UiSurface): Boolean =
      surface.focusPolicy == SurfaceFocusPolicy.Peek && state.persisted.focus != Focus.Surface(surface.id)
    if keepsEveryKey(state) then PeekKeyOutcome.PassedOn(state)
    else if isEscape(event) && state.runtime.uiSurfaces.exists(unfocusedPeek) then
      PeekKeyOutcome.Consumed(withoutSurfaces(state, unfocusedPeek))
    else PeekKeyOutcome.PassedOn(withoutSurfaces(state, surface => unfocusedPeek(surface) && surface.dismissOnMove))

  /** The editor pane a key goes on to when `focus`'s handler returns a bubbling result. */
  def bubbleTarget(focus: Focus, result: ComponentResult, state: AppState): Option[PaneId] =
    focus match
      case Focus.Surface(surfaceId) if result.bubbles =>
        val keepsKeys = state.surfaceById(surfaceId).exists(_.focusPolicy == SurfaceFocusPolicy.Modal)
        if keepsKeys then None else state.persisted.layout.activeEditorPaneId
      case _ => None

  /** A focused surface's translator spells keys in its own vocabulary; this restates one as the editor reads it. */
  def asEditorEvent(event: Event): Option[EditorEvent] =
    event match
      case textEntry: TextEntryEvent          => Some(textEntry)
      case vertical: VerticalNavigationEvent  => Some(vertical)
      case modal: ModalInputEvent             => modalAsEditorEvent(modal)
      case PeekInputEvent.Navigate(direction) => Some(moveFor(direction))
      case PeekInputEvent.Accept              => Some(NewLine)
      case PeekInputEvent.Dismiss             => Some(Escape)
      case _                                  => None

  private def keepsEveryKey(state: AppState): Boolean =
    state.persisted.focus == Focus.Modal || state.activeSurface.exists(_.focusPolicy == SurfaceFocusPolicy.Modal)

  private def isEscape(event: Event): Boolean =
    event match
      case Escape | ModalDismiss | PeekInputEvent.Dismiss | PanelInputEvent.Dismiss => true
      case _                                                                        => false

  private def withoutSurfaces(state: AppState, remove: UiSurface => Boolean): AppState =
    if state.runtime.uiSurfaces.exists(remove) then
      state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(remove)))
    else state

  private def modalAsEditorEvent(event: ModalInputEvent): Option[EditorEvent] =
    event match
      case ModalInsertChar(char)    => Some(InsertChar(char))
      case ModalDeleteBackward      => Some(DeleteBackward)
      case ModalDeleteForward       => Some(DeleteForward)
      case ModalDeleteWordBackward  => Some(DeleteWordBackward)
      case ModalDeleteWordForward   => Some(DeleteWordForward)
      case ModalPaste               => Some(Paste)
      case ModalNavigate(direction) => Some(moveFor(direction))
      case ModalLineStart           => Some(MoveToStart)
      case ModalLineEnd             => Some(MoveToEnd)
      case ModalFirst               => Some(MoveToStartOfFile)
      case ModalLast                => Some(MoveToEndOfFile)
      case ModalPage(pages)         => Some(if pages < 0 then PageUp else PageDown)
      case ModalNextField           => Some(TabKey)
      case ModalPreviousField       => Some(ReverseTabKey)
      case ModalSubmit              => Some(NewLine)
      case ModalFindNext            => Some(FindNext)
      case ModalFindPrevious        => Some(FindPrevious)
      case ModalDismiss             => Some(Escape)
      case ModalMove(_) | ModalCreateDirectory | ModalOpenAsProjectRoot | ModalClick(_, _) | ModalActionClick(_) |
          ModalToggleFindOption(_) =>
        None

  private def moveFor(direction: Direction): EditorEvent =
    direction match
      case Direction.Up    => MoveUp
      case Direction.Down  => MoveDown
      case Direction.Left  => MoveLeft
      case Direction.Right => MoveRight
