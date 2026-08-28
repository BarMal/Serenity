package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** State the event pipeline exposes for hovering and clicking the contextual toolbar. */
private[manager] trait ContextualToolbarHitTestingPort:
  def stateRef: Ref[IO, AppState]
  def executeCommand(command: com.serenity.command.Command): IO[Unit]

/** Hit-tests hover/click against the open contextual toolbar's top-level items and open detail (dropdown or input),
  * independent of every other mouse target.
  */
final private[manager] class ContextualToolbarHitTesting(port: ContextualToolbarHitTestingPort):
  import port.*

  def handleContextualToolbarMouseHover(event: MouseInputEvent, state: AppState): IO[Boolean] =
    IO.pure(contextualToolbarSelectionAt(event, state).isDefined)

  def handleContextualToolbarMouseClick(click: MouseClick, state: AppState): IO[Boolean] =
    contextualToolbarSelectionAt(click, state) match
      case Some((surface, toolbarState, ContextualToolbarHit.TopLevelItem(index))) =>
        val registry     = CommandRegistry.withToggleUI
        val items        = ContextualToolbar.itemsFor(state)
        val focusedState = toolbarState.withFocusedIndex(index, items)
        val focusedItem  = focusedState.normalized(items).focusedItem(items)
        stateRef.update { current =>
          val nextState =
            focusedItem match
              case Some(_: ContextualToolbarItem.Button)   => focusedState.closeDetail
              case Some(_: ContextualToolbarItem.Dropdown) => focusedState.openFocusedDetail(items)
              case Some(_: ContextualToolbarItem.Input)    => focusedState.openFocusedDetail(items)
              case None                                    => focusedState
          val updated = replaceContextualToolbar(current, surface, nextState)
          focusedItem match
            case Some(_: ContextualToolbarItem.Dropdown) | Some(_: ContextualToolbarItem.Input) =>
              updated.pushFocus(Focus.Surface(surface.id))
            case Some(_: ContextualToolbarItem.Button) =>
              updated.copy(persisted = updated.persisted.copy(focus = editorFocus(current)))
            case _ =>
              updated
        } >>
          stateRef.get.flatMap { current =>
            focusedItem match
              case Some(_: ContextualToolbarItem.Button) =>
                ContextualToolbar.focusedCommand(focusedState, current, registry) match
                  case Some(command) => executeCommand(command).as(true)
                  case None          => IO.pure(false)
              case Some(_: ContextualToolbarItem.Dropdown) | Some(_: ContextualToolbarItem.Input) =>
                IO.pure(true)
              case None =>
                IO.pure(false)
          }
      case Some((surface, toolbarState, ContextualToolbarHit.DropdownOption(itemId, optionIndex))) =>
        val detailState =
          toolbarState.copy(detailState = Some(ContextualToolbarDetailState.Dropdown(itemId, optionIndex)))
        stateRef.update { current =>
          val replaced = replaceContextualToolbar(current, surface, detailState.closeDetail)
          replaced.copy(persisted = replaced.persisted.copy(focus = editorFocus(current)))
        } >>
          stateRef.get.flatMap { current =>
            ContextualToolbar.detailCommand(detailState, current) match
              case Some(command) => executeCommand(command).as(true)
              case None          => IO.pure(false)
          }
      case Some((surface, toolbarState, ContextualToolbarHit.InputDetail(_))) =>
        stateRef
          .update(current =>
            replaceContextualToolbar(current, surface, toolbarState).pushFocus(Focus.Surface(surface.id))
          )
          .as(true)
      case None =>
        IO.pure(false)

  private def contextualToolbarSelectionAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, ContextualToolbarState, ContextualToolbarHit)] =
    for
      viewportSize <- state.runtime.viewportSize
      surface      <- state.contextualToolbarSurface
      toolbarState <- surface.content match
        case SurfaceContent.ContextualToolbar(toolbarState) => Some(toolbarState)
        case _                                              => None
      scene    = AuthoritativeUiScene.forState(state, viewportSize)
      layout   = scene.calculatedLayout
      contract = scene.editorContract
      contentRect <- contract.overlayContentRect(surface.id)
      hit <- contextualToolbarItemHit(
        event,
        contentRect,
        state,
        toolbarState,
        contract.overlayRowSlots(surface.id),
        layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0)
      )
    yield (surface, toolbarState, hit)

  private def contextualToolbarItemHit(
    event: MouseInputEvent,
    contentRect: LayoutRect,
    state: AppState,
    toolbarState: ContextualToolbarState,
    rowSlots: List[SurfaceContentRowSlot],
    floatingOffsetRows: Double
  ): Option[ContextualToolbarHit] =
    val rowIndex =
      if event.pixelX.isDefined && event.pixelY.isDefined then
        val metrics = MouseHitTestGeometry.floatingCellMetrics(state)
        val rowCount = rowSlots.count {
          case SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), _) => true
          case _                                                       => false
        }
        for
          pixelX <- event.pixelX
          pixelY <- event.pixelY
          geometry = FloatingSurfaceGeometry
            .fromCells(
              contentRect,
              metrics,
              borderCells = 0,
              itemCount = rowCount,
              hasHeader = false,
              hasFooter = false,
              itemGapRows = state.persisted.config.uiElementGap,
              itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(
                SurfaceContent.ContextualToolbar(toolbarState),
                state.persisted.config.interfaceDensity
              )
            )
            .translated(0.0, FloatingSurfaceGeometry.signedRowOffsetPixels(floatingOffsetRows, metrics))
          index <- geometry.itemIndexAt(pixelX, pixelY)
        yield index
      else
        MouseHitTestGeometry.overlayDisplayedRowIndexAt(
          event,
          contentRect,
          rowSlots,
          SurfaceFrameLayout.itemTargetRowsFor(
            SurfaceContent.ContextualToolbar(toolbarState),
            state.persisted.config.interfaceDensity
          )
        )
    rowIndex.flatMap { rowIndex =>
      ContextualToolbarLayout.hitAt(
        rowIndex = rowIndex,
        columnOffset = event.col - contentRect.x,
        contentWidth = contentRect.width.max(1),
        toolbarState = toolbarState,
        state = state
      )
    }

  private def replaceContextualToolbar(
    state: AppState,
    surface: UiSurface,
    toolbarState: ContextualToolbarState
  ): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.ContextualToolbar(toolbarState))
      case existing =>
        existing
    }))

  private def editorFocus(state: AppState): Focus =
    state.persisted.layout.activeEditorPaneId
      .map(Focus.EditorPane.apply)
      .getOrElse(Focus.EditorPane(PaneId(0)))
