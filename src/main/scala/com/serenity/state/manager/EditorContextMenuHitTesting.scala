package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** State the event pipeline exposes for opening, hovering, and selecting from the editor's right-click context menu.
  * `resolveMouseTarget` stays owned by the pipeline's core mouse-targeting module since it is shared with click/press/
  * drag handling, not exclusive to the context menu. As a capability record rather than a trait -- nothing here breaks
  * a construction-order cycle (#1389), so mockability is the only reason this needs an interface at all, and a record
  * fakes trivially without one (#1017).
  */
final private[manager] case class EditorContextMenuHitTestingPort(
    stateRef: Ref[IO, AppState],
    executeCommand: com.serenity.command.Command => IO[Unit],
    resolveMouseTarget: (MouseInputEvent, AppState) => IO[Option[(PaneId, Buffer, CursorPosition)]]
)

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
            current.copy(runtime =
              current.runtime.copy(uiSurfaces =
                current.runtime.uiSurfaces.replacedWhere(_.id == surface.id)(
                  _.copy(content = SurfaceContent.ContextMenu(menu.withSelectedIndex(index)))
                )
              )
            )
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

  /** Resolves hover/click against the context menu's own `ResolvedSurfaceComposition` (issue #819, slice 2) --
    * the same composition `OverlayViewModel` paints from, via `SurfaceHitRegion.hitAt`, rather than a parallel
    * `MouseHitTestGeometry.overlayItemIndex` row calculation. Mirrors `ModalMouseHitTesting.modalHitAt`.
    */
  private def contextMenuSelectionAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, ContextMenu, Int)] =
    for
      (surface, menu, hit) <- contextMenuHitAt(event, state)
      absoluteIndex        <- hit.focusId.value.stripPrefix("context-menu-item-").toIntOption
    yield (surface, menu, absoluteIndex)

  private def contextMenuHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, ContextMenu, SurfaceHitRegion)] =
    for
      viewportSize <- state.runtime.viewportSize
      surface      <- state.contextMenuSurface
      menu <- surface.content match
        case SurfaceContent.ContextMenu(menu) => Some(menu)
        case _                                => None
      node <- UiSceneSnapshot
        .from(state, viewportSize)
        .nodesInPaintOrder
        .find(_.id == SceneNodeId.Surface(surface.id))
      _ <- Option.when(node.frameRect.contains(event.col, event.row))(())
      hit <- ContextMenuSurfaceComposition
        .forMenu(
          menu,
          node.frameRect,
          state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
          SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
        )
        .hitAt(event.col.toDouble, event.row.toDouble)
    yield (surface, menu, hit)

  /** True for a click inside the menu's content rect that lands on no painted row at all -- neither an item nor the
    * title/footer chrome rows -- so it should be swallowed rather than falling through to the editor underneath.
    * Checked against every `paintBoxes` rect (not just `hitRegions`) because the title and footer rows are painted
    * but intentionally not selectable, and a click on them is not a "gap" either.
    */
  private def isContextMenuItemGap(event: MouseInputEvent, state: AppState): Boolean =
    (for
      viewportSize <- state.runtime.viewportSize
      surface      <- state.contextMenuSurface
      menu <- surface.content match
        case SurfaceContent.ContextMenu(menu) => Some(menu)
        case _                                => None
      node <- UiSceneSnapshot
        .from(state, viewportSize)
        .nodesInPaintOrder
        .find(_.id == SceneNodeId.Surface(surface.id))
      contentRect = SurfaceFrameLayout(node.frameRect).contentRect
      composition = ContextMenuSurfaceComposition.forMenu(
        menu,
        node.frameRect,
        state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
        SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity)
      )
    yield contentRect.contains(event.col, event.row) &&
      !composition.paintBoxes.exists(_.rect.contains(event.col.toDouble, event.row.toDouble)))
      .getOrElse(false)

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
