package com.serenity.state.components

import com.serenity.keystroke.events.Event
import com.serenity.state.models.{AppState, ContextMenu, SurfaceContent, replacedWhere}
import com.serenity.ui.widget.{EndBehaviour, ListOutcome, SelectableList, WidgetInput}

/** Keyboard handling for the editor's context menu: the arrows, Page Up/Down and Home/End move the highlight, Enter
  * runs the highlighted command and Escape closes the menu. Any other key is ignored rather than closing it.
  */
class ContextMenuComponent extends TypedFocusedComponent[Event]:

  protected def decodeEvent(event: Event): Option[Event] = Some(event)

  protected def processTypedEvent(event: Event, currentState: AppState): ComponentResult =
    (for
      input   <- WidgetInput.fromEvent(event)
      surface <- currentState.contextMenuSurface
      menu <- surface.content match
        case SurfaceContent.ContextMenu(menu) => Some(menu)
        case _                                => None
    yield
      val list = SelectableList(menu.items.toVector, Some(menu.selectedIndex), endBehaviour = EndBehaviour.Wrap)
      val (moved, outcome) = list.update(input, menu.items.size)
      outcome match
        case Some(ListOutcome.Activated(_, item)) =>
          ComponentResult.composite(
            ComponentResult.updateState(state =>
              val closed = state.withoutContextMenu
              closed.copy(persisted = closed.persisted.copy(focus = menu.targetFocus))
            ),
            ComponentResult.executeCommand(item.command)
          )
        case Some(ListOutcome.Dismissed) => ComponentResult.updateState(_.withoutContextMenu)
        case None =>
          moved.selected
            .filterNot(_ == menu.selectedIndex)
            .fold(ComponentResult.noChange)(index => ComponentResult.updateState(highlighted(surface.id, menu, index)))
    ).getOrElse(ComponentResult.noChange)

  private def highlighted(surfaceId: com.serenity.state.models.SurfaceId, menu: ContextMenu, index: Int)(
    state: AppState
  ): AppState =
    state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.replacedWhere(_.id == surfaceId)(
          _.copy(content = SurfaceContent.ContextMenu(menu.withSelectedIndex(index)))
        )
      )
    )
