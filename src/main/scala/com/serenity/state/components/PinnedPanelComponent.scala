package com.serenity.state.components

import com.serenity.command.{Command, CommandCategory, CommandIntent, ViewIntent}
import com.serenity.config.PanelEscapeTarget
import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, Focus, SurfaceContent, SurfacePresentation, UiSurface}
import com.serenity.state.reducers.PanelFocusHistory
import com.serenity.ui.layout.{LayoutEngine, PanelPosition, ViewportSize}

/** Keys for the panel focused at one edge: typing returns to the editor, Escape to wherever the app mode's setting
  * says, and Ctrl+Up/Down resize it, while moving within it depends on what it shows -- see [[ExplorerPanelKeys]],
  * [[ListPanelKeys]] and [[OutputPanelKeys]].
  */
class PinnedPanelComponent(
    position: PanelPosition
) extends TypedFocusedComponent[PanelInputEvent]:

  protected def decodeEvent(event: Event): Option[PanelInputEvent] =
    PanelInputEvent.fromEvent(event)

  protected def processTypedEvent(event: PanelInputEvent, currentState: AppState): ComponentResult =
    activeSurface(currentState) match
      case Some(surface) => processPanelEvent(event, surface, currentState)
      case None          => ComponentResult.noChange

  private def processPanelEvent(event: PanelInputEvent, surface: UiSurface, currentState: AppState): ComponentResult =
    event match
      case PanelInputEvent.NoOp =>
        ComponentResult.noChange
      case PanelInputEvent.ReturnFocus =>
        returnToEditor(currentState)
      case PanelInputEvent.Dismiss =>
        currentState.persisted.config.panelEscapeTarget match
          case PanelEscapeTarget.Editor   => returnToEditor(currentState)
          case PanelEscapeTarget.Previous => ComponentResult.updateState(PanelFocusHistory.returnToPrevious)
      case PanelInputEvent.Resize(delta) =>
        resized(surface, delta)
      case movement =>
        val rows = visibleRows(surface, currentState)
        val handled = surface.content match
          case SurfaceContent.DirectoryTree(tree, selectedPath) =>
            ExplorerPanelKeys.handle(movement, surface, tree, selectedPath, currentState, rows)
          case SurfaceContent.Outline(_, _) | SurfaceContent.Comments(_, _) | SurfaceContent.Diagnostics(_, _) =>
            ListPanelKeys.handle(movement, surface, currentState, rows)
          case SurfaceContent.Terminal(text, cursor) =>
            OutputPanelKeys.handle(movement, surface, text, cursor, rows)
          case _ => None
        handled.getOrElse(ComponentResult.noChange)

  private def returnToEditor(currentState: AppState): ComponentResult =
    currentState.persisted.layout.activeEditorPaneId match
      case Some(paneId) => ComponentResult.transferFocus(Focus.EditorPane(paneId))
      case None         => ComponentResult.noChange

  /** Resizes the panel (issue #1310) through the same generic `Command`/`CommandIntent` path the palette already uses
    * -- not a bespoke keyboard-only mechanism.
    */
  private def resized(surface: UiSurface, delta: Int): ComponentResult =
    ComponentResult.executeCommand(
      Command.typed(
        "resize-focused-panel",
        "Resize the focused panel.",
        CommandIntent.View(ViewIntent.SetPanelSize(surface.id, delta)),
        CommandCategory.View
      )
    )

  /** How many rows the panel shows -- what a page is. */
  private def visibleRows(surface: UiSurface, state: AppState): Int =
    val layout = LayoutEngine.calculateLayoutWithUI(state, state.runtime.viewportSize.getOrElse(ViewportSize(80, 24)))
    layout.pinnedSurfaceRects.get(surface.id).map(rect => math.max(1, rect.height - 2)).getOrElse(1)

  /** The focused panel at this edge, or failing that the one most recently used there. */
  private def activeSurface(currentState: AppState): Option[UiSurface] =
    focusedPinnedSurface(currentState).orElse(
      currentState.runtime.uiSurfaces.reverse.find(isPinnedAtPosition(_, currentState))
    )

  private def focusedPinnedSurface(currentState: AppState): Option[UiSurface] =
    currentState.persisted.focus match
      case Focus.Surface(surfaceId) =>
        currentState.surfaceById(surfaceId).filter(isPinnedAtPosition(_, currentState))
      case _ =>
        None

  private def isPinnedAtPosition(surface: UiSurface, currentState: AppState): Boolean =
    surface.presentation match
      case SurfacePresentation.Docked =>
        currentState.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id)).contains(position)
      case _ =>
        false
