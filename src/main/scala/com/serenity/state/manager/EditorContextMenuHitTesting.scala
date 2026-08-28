package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** State the event pipeline exposes for opening, hovering, and selecting from the editor's right-click context menu.
  * `resolveMouseTarget` stays owned by the pipeline's core mouse-targeting module since it is shared with click/press/
  * drag handling, not exclusive to the context menu.
  */
private[manager] trait EditorContextMenuHitTestingPort:
  def stateRef: Ref[IO, AppState]
  def executeCommand(command: com.serenity.command.Command): IO[Unit]
  def resolveMouseTarget(click: MouseInputEvent, state: AppState): IO[Option[(PaneId, Buffer, CursorPosition)]]

/** Opens the editor context menu at a resolved click target, and hit-tests hover/click against its open items,
  * independent of every other mouse target.
  */
final private[manager] class EditorContextMenuHitTesting(port: EditorContextMenuHitTestingPort):
  import port.*

  private val ContextMenuSurfaceId = SurfaceId("context-menu")

  private val EditorContextMenuCommands =
    List(
      "copy",
      "cut",
      "paste",
      "select-all",
      "save",
      "save-as",
      "find",
      "replace",
      "bold",
      "italic",
      "underline",
      "heading-1",
      "heading-2",
      "heading-3",
      "paragraph-body",
      "align-left",
      "align-center",
      "align-right",
      "align-justify",
      "goto-line",
      "toggle-bookmark",
      "next-bookmark",
      "previous-bookmark",
      "add-document-comment",
      "delete-document-comment",
      "next-document-comment",
      "previous-document-comment",
      "navigate-back",
      "navigate-forward",
      "next-document-symbol",
      "previous-document-symbol",
      "markdown-preview",
      "pin-outline"
    )

  def openEditorContextMenu(click: MouseClick, state: AppState): IO[Unit] =
    resolveMouseTarget(click, state).flatMap {
      case Some((paneId, _, clickedCursor)) =>
        editorContextMenu(Focus.EditorPane(paneId)) match
          case Some(menu) =>
            stateRef.update { current =>
              val surface = UiSurface(
                id = ContextMenuSurfaceId,
                content = SurfaceContent.ContextMenu(menu),
                presentation = SurfacePresentation.Floating(Some(clickedCursor), SurfacePlacement.BelowCursor)
              )
              current
                .copy(runtime =
                  current.runtime
                    .copy(uiSurfaces = current.runtime.uiSurfaces.filterNot(isContextMenuSurface) :+ surface)
                )
                .pushFocus(Focus.Surface(ContextMenuSurfaceId))
            }
          case None =>
            IO.unit
      case None =>
        dismissContextMenuIfOpen(state)
    }

  def handleContextMenuMouseHover(event: MouseInputEvent, state: AppState): IO[Boolean] =
    contextMenuSelectionAt(event, state) match
      case Some((surface, menu, index)) =>
        stateRef
          .update { current =>
            current.copy(runtime = current.runtime.copy(uiSurfaces = current.runtime.uiSurfaces.map {
              case existing if existing.id == surface.id =>
                existing.copy(content = SurfaceContent.ContextMenu(menu.withSelectedIndex(index)))
              case existing => existing
            }))
          }
          .as(true)
      case None =>
        IO.pure(false)

  def handleContextMenuMouseClick(click: MouseClick, state: AppState): IO[Boolean] =
    contextMenuSelectionAt(click, state) match
      case Some((_, menu, index)) =>
        menu.items.lift(index) match
          case Some(item) =>
            stateRef.update { current =>
              val dismissed = dismissContextMenu(current)
              dismissed.copy(persisted = dismissed.persisted.copy(focus = menu.targetFocus))
            } >>
              executeCommand(item.command).as(true)
          case None =>
            IO.pure(false)
      case None if isContextMenuItemGap(click, state) =>
        IO.pure(true)
      case None if state.contextMenuSurface.isDefined =>
        stateRef.update(dismissContextMenu).as(true)
      case None =>
        IO.pure(false)

  def dismissContextMenuIfOpen(state: AppState): IO[Unit] =
    if state.contextMenuSurface.isDefined then stateRef.update(dismissContextMenu)
    else IO.unit

  def dismissContextMenu(state: AppState): AppState =
    state
      .copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(isContextMenuSurface)))
      .popFocus

  private def contextMenuSelectionAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, ContextMenu, Int)] =
    for
      viewportSize <- state.runtime.viewportSize
      surface      <- state.contextMenuSurface
      menu <- surface.content match
        case SurfaceContent.ContextMenu(menu) => Some(menu)
        case _                                => None
      scene    = AuthoritativeUiScene.forState(state, viewportSize)
      layout   = scene.calculatedLayout
      contract = scene.editorContract
      contentRect <- contract.overlayContentRect(surface.id)
      index <- MouseHitTestGeometry.overlayItemIndex(
        event,
        state,
        layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
        contentRect,
        contract.overlayRowSlots(surface.id),
        menu.items.length,
        menu.selectedIndex,
        hasHeader = true,
        hasFooter = menu.items.nonEmpty,
        itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
        itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
      )
    yield (surface, menu, index)

  private def isContextMenuItemGap(event: MouseInputEvent, state: AppState): Boolean =
    (for
      viewportSize <- state.runtime.viewportSize
      surface      <- state.contextMenuSurface
      scene    = AuthoritativeUiScene.forState(state, viewportSize)
      contract = scene.editorContract
      contentRect <- contract.overlayContentRect(surface.id)
    yield contentRect.contains(event.col, event.row) &&
      !contract.overlayRowSlots(surface.id).exists(_.y == event.row)).getOrElse(false)

  private def editorContextMenu(targetFocus: Focus): Option[ContextMenu] =
    val registry = CommandRegistry.withToggleUI
    val items = EditorContextMenuCommands.flatMap { name =>
      registry.findCommand(name).map(command => ContextMenuItem(command.name, command.label, command))
    }
    Option.when(items.nonEmpty)(ContextMenu("editor", targetFocus, items))

  private def isContextMenuSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.ContextMenu(_) => true
      case _                             => false
