package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ReducerResult, Transition}
import com.serenity.ui.layout.*

/** State the event pipeline exposes for hovering and clicking the contextual toolbar, as a capability record rather
  * than a trait -- nothing here breaks a construction-order cycle (#1389), so mockability is the only reason this needs
  * an interface at all, and a record fakes trivially without one (#1017).
  */
final private[manager] case class ContextualToolbarHitTestingPort(
    currentState: IO[AppState],
    applyReducerResult: (ReducerResult, AppState) => IO[Unit]
)

/** Hit-tests hover/click against the open contextual toolbar's top-level items and open detail (dropdown or input),
  * independent of every other mouse target.
  */
final private[manager] class ContextualToolbarHitTesting(port: ContextualToolbarHitTestingPort):

  def handleContextualToolbarMouseHover(event: MouseInputEvent, state: AppState): IO[Boolean] =
    IO.pure(ContextualToolbarHitTesting.claimsHover(event, state))

  def handleContextualToolbarMouseClick(click: MouseClick, state: AppState): IO[Boolean] =
    MouseTransition.commit(port.currentState, port.applyReducerResult)(ContextualToolbarHitTesting.click(click, state))

private[manager] object ContextualToolbarHitTesting:

  /** Hovering the toolbar changes nothing; it only keeps the pointer from reaching the targets underneath. */
  def claimsHover(event: MouseInputEvent, state: AppState): Boolean =
    contextualToolbarSelectionAt(event, state).isDefined

  def click(click: MouseClick, state: AppState): Transition[Boolean] =
    contextualToolbarSelectionAt(click, state) match
      case Some((surface, toolbarState, ContextualToolbarHit.TopLevelItem(index))) =>
        topLevelItemClick(surface, toolbarState, index, state)
      case Some((surface, toolbarState, ContextualToolbarHit.DropdownOption(itemId, optionIndex))) =>
        val detailState =
          toolbarState.copy(detailState = Some(ContextualToolbarDetailState.Dropdown(itemId, optionIndex)))
        Transition.modify { current =>
          val replaced = replaceContextualToolbar(current, surface, detailState.closeDetail)
          replaced.copy(persisted = replaced.persisted.copy(focus = editorFocus(current)))
        } *> executeIfDefined(ContextualToolbar.detailCommand(detailState, _))
      case Some((surface, toolbarState, ContextualToolbarHit.InputDetail(_))) =>
        Transition
          .modify(current =>
            replaceContextualToolbar(current, surface, toolbarState).pushFocus(Focus.Surface(surface.id))
          )
          .as(true)
      case None =>
        Transition.pure(false)

  private def topLevelItemClick(
    surface: UiSurface,
    toolbarState: ContextualToolbarState,
    index: Int,
    state: AppState
  ): Transition[Boolean] =
    val items        = ContextualToolbar.itemsFor(state)
    val focusedState = toolbarState.withFocusedIndex(index, items)
    val focusedItem  = focusedState.normalized(items).focusedItem(items)
    val focusItem = Transition.modify { current =>
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
    }
    val outcome: Transition[Boolean] = focusedItem match
      case Some(_: ContextualToolbarItem.Button) =>
        executeIfDefined(ContextualToolbar.focusedCommand(focusedState, _))
      case Some(_: ContextualToolbarItem.Dropdown) | Some(_: ContextualToolbarItem.Input) =>
        Transition.pure(true)
      case None =>
        Transition.pure(false)
    focusItem *> outcome

  /** Resolved from the state after the toolbar update -- the same state the emitted command will run against. */
  private def executeIfDefined(command: AppState => Option[com.serenity.command.Command]): Transition[Boolean] =
    Transition.inspect(command).flatMap {
      case Some(command) => Transition.emit(AppEffect.ExecuteCommand(command)).as(true)
      case None          => Transition.pure(false)
    }

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
      frameRect   <- contract.overlayRect(surface.id)
      contentRect <- contract.overlayContentRect(surface.id)
      hit <- contextualToolbarItemHit(
        event,
        contentRect,
        frameRect,
        state,
        toolbarState,
        contract.overlayRowSlots(surface.id),
        layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0)
      )
    yield (surface, toolbarState, hit)

  /** Resolves hover/click against the toolbar's own `ResolvedSurfaceComposition` (issue #819, slice 1) for the ordinary
    * cell-coordinate case -- the same composition `OverlayViewModel` paints from, via `SurfaceHitRegion`, rather than a
    * parallel `MouseHitTestGeometry.overlayDisplayedRowIndexAt` row lookup followed by `ContextualToolbarLayout.hitAt`.
    * Mirrors `CommandRunnerMouseHitTesting.commandRunnerSelectionForSurface`.
    *
    * The sub-cell fractional-pixel path (`event.pixelX`/`pixelY` defined -- `ContextualToolbarMouseSpec`'s "fractional
    * code-metric pixel offset" test) is a distinct, already-tested feature this migration does not touch, so it still
    * resolves a row via `FloatingSurfaceGeometry` and then `ContextualToolbarLayout.hitAt` unchanged.
    */
  private def contextualToolbarItemHit(
    event: MouseInputEvent,
    contentRect: LayoutRect,
    frameRect: LayoutRect,
    state: AppState,
    toolbarState: ContextualToolbarState,
    rowSlots: List[SurfaceContentRowSlot],
    floatingOffsetRows: Double
  ): Option[ContextualToolbarHit] =
    if event.pixelX.isDefined && event.pixelY.isDefined then
      val metrics = MouseHitTestGeometry.floatingCellMetrics(state)
      val rowCount = rowSlots.count {
        case SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), _) => true
        case _                                                       => false
      }
      val rowIndex =
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
              itemGapRows = state.effectiveUiElementGap,
              itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(
                SurfaceContent.ContextualToolbar(toolbarState),
                state.persisted.config.interfaceDensity
              )
            )
            .translated(0.0, FloatingSurfaceGeometry.signedRowOffsetPixels(floatingOffsetRows, metrics))
          index <- geometry.itemIndexAt(pixelX, pixelY)
        yield index
      rowIndex.flatMap { rowIndex =>
        ContextualToolbarLayout.hitAt(
          rowIndex = rowIndex,
          columnOffset = event.col - contentRect.x,
          contentWidth = contentRect.width.max(1),
          toolbarState = toolbarState,
          state = state
        )
      }
    else
      ContextualToolbarSurfaceComposition
        .forToolbar(toolbarState, state, frameRect)
        .hitAt(event.col.toDouble, event.row.toDouble)
        .flatMap(region => ContextualToolbarSurfaceComposition.hitFromFocusId(region.focusId))

  private def replaceContextualToolbar(
    state: AppState,
    surface: UiSurface,
    toolbarState: ContextualToolbarState
  ): AppState =
    state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.replacedWhere(_.id == surface.id)(
          _.copy(content = SurfaceContent.ContextualToolbar(toolbarState))
        )
      )
    )

  private def editorFocus(state: AppState): Focus =
    state.persisted.layout.activeEditorPaneId
      .map(Focus.EditorPane.apply)
      .getOrElse(Focus.EditorPane(PaneId(0)))
