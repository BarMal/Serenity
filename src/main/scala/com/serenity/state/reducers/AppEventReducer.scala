package com.serenity.state.reducers

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, ViewportSize}

object AppEventReducer:

  def reduce(
    event: GlobalAppEvent,
    state: AppState,
    registry: CommandRegistry
  )(using com.serenity.rope.Balance): ReducerResult =
    event match
      case Quit =>
        ReducerResult.withEffect(state, AppEffect.CompleteQuit)

      case ToggleCommandRunner =>
        if state.startPageSurface.isDefined then ReducerResult.noEffects(state)
        else ReducerResult.noEffects(toggleCommandRunner(state, registry))

      case NewTab =>
        val (withBuffer, newBufferId) = createNewEmptyBuffer(state)
        val updatedState = focusBuffer(
          rebalancePanes(insertBufferInOrder(withBuffer, newBufferId), Some(newBufferId)),
          newBufferId
        )
        ReducerResult.noEffects(updatedState)

      case CloseTab =>
        ReducerResult.noEffects(closeFocusedTab(state))

      case NextTab =>
        ReducerResult.noEffects(navigateBuffer(state, _.nextBufferInOrder))

      case PreviousTab =>
        ReducerResult.noEffects(navigateBuffer(state, _.previousBufferInOrder))

  def rebalancePanes(state: AppState, focusedBufferId: Option[BufferId] = None): AppState =
    assignBuffersToPanes(state, focusedBufferId)

  private def toggleCommandRunner(state: AppState, registry: CommandRegistry): AppState =
    state.commandRunnerSurface.flatMap(asCommandRunner) match
      case Some((surface, runner)) if runner.isActive =>
        val previousFocus = runner.previousFocus.getOrElse(Focus.EditorPane(PaneId(0)))
        state.copy(
          uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id),
          focus = previousFocus
        )
      case _ =>
        val activatedRunner = CommandRunner.empty
          .activate(registry, state.config)
          .withPreviousFocus(state.focus)
        val (stateWithId, surfaceId) =
          state.commandRunnerSurface.map(surface => (state, surface.id)).getOrElse(state.allocateSurfaceId)
        val surface = UiSurface(
          id = surfaceId,
          content = SurfaceContent.CommandPalette(activatedRunner),
          presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
        )
        stateWithId.copy(
          uiSurfaces = upsertSurface(stateWithId.uiSurfaces, surface),
          focus = Focus.Surface(surfaceId)
        )

  private def createNewEmptyBuffer(state: AppState)(using com.serenity.rope.Balance): (AppState, BufferId) =
    val bufferId = state.nextBufferId
    val buffer   = Buffer.newEmpty(bufferId)
    (
      state.copy(
        buffers = state.buffers + (bufferId -> buffer),
        nextBufferId = BufferId(bufferId.value + 1)
      ),
      bufferId
    )

  private def insertBufferInOrder(state: AppState, newBufferId: BufferId): AppState =
    state.focusedBufferId match
      case Some(currentBufferId) =>
        val currentIndex = state.bufferOrder.indexOf(currentBufferId)
        if currentIndex == -1 then
          state.copy(bufferOrder = state.bufferOrder :+ newBufferId)
        else
          val (before, after) = state.bufferOrder.splitAt(currentIndex + 1)
          state.copy(bufferOrder = before ++ List(newBufferId) ++ after)
      case None =>
        state.copy(bufferOrder = state.bufferOrder :+ newBufferId)

  private def assignBuffersToPanes(state: AppState, focusedBufferId: Option[BufferId]): AppState =
    val viewportSize        = state.viewportSize.getOrElse(ViewportSize(80, 24))
    val layout              = LayoutEngine.calculateLayout(state, viewportSize)
    val maxPossiblePanes    = math.max(1, layout.editorPanelRect.width / state.config.minimumPaneWidth)
    val targetFocusedBuffer = focusedBufferId.orElse(state.focusedBufferId)
    updatePaneAssignments(state, maxPossiblePanes, targetFocusedBuffer)

  private def updatePaneAssignments(
    state: AppState,
    maxVisiblePanes: Int,
    targetFocusedBuffer: Option[BufferId]
  ): AppState =
    targetFocusedBuffer match
      case Some(focusedBufferId) =>
        val focusedIndex = state.bufferOrder.indexOf(focusedBufferId)
        val startIndex =
          if focusedIndex == -1 then 0
          else math.max(0, focusedIndex - maxVisiblePanes / 2)
        val visibleBuffers = state.bufferOrder.slice(startIndex, startIndex + maxVisiblePanes)

        val neededPanes  = visibleBuffers.size
        val currentPanes = state.layout.editorPanes
        val paneIds      = currentPanes.keys.toList.sortBy(_.value)

        val updatedState =
          if paneIds.size < neededPanes then
            val additionalPanes = (paneIds.size until neededPanes).map { i =>
              val paneId = PaneId(state.nextPaneId.value + i - paneIds.size)
              paneId -> EditorPane.empty(paneId)
            }.toMap
            val newNextPaneId = PaneId(
              math.max(state.nextPaneId.value, state.nextPaneId.value + neededPanes - paneIds.size)
            )
            state.copy(
              layout = state.layout.copy(editorPanes = currentPanes ++ additionalPanes),
              nextPaneId = newNextPaneId
            )
          else state

        val finalPanes      = updatedState.layout.editorPanes.keys.toList.sortBy(_.value)
        val paneAssignments = finalPanes.take(visibleBuffers.size).zip(visibleBuffers).toMap

        val assignedPanes = finalPanes.map { paneId =>
          paneAssignments.get(paneId) match
            case Some(bufferId) => paneId -> EditorPane.withBuffer(paneId, bufferId)
            case None           => paneId -> EditorPane.empty(paneId)
        }.toMap

        val finalState = updatedState.copy(
          layout = updatedState.layout.copy(editorPanes = assignedPanes)
        )

        assignedPanes.find(_._2.bufferId.contains(focusedBufferId)) match
          case Some((paneId, _)) =>
            finalState.copy(
              layout = finalState.layout.copy(activeEditorPaneId = Some(paneId)),
              focus = Focus.EditorPane(paneId)
            )
          case None =>
            finalState

      case None =>
        state

  private def focusBuffer(state: AppState, bufferId: BufferId): AppState =
    state.layout.editorPanes.find(_._2.bufferId.contains(bufferId)) match
      case Some((paneId, _)) =>
        state.copy(
          focus = Focus.EditorPane(paneId),
          layout = state.layout.copy(activeEditorPaneId = Some(paneId))
        )
      case None =>
        state

  private def closeFocusedTab(state: AppState)(using com.serenity.rope.Balance): AppState =
    state.focus match
      case Focus.EditorPane(paneId) =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId match
              case Some(bufferId) =>
                closePaneState(rebalancePanes(closeBufferState(state, bufferId)), paneId)
              case None =>
                closePaneState(state, paneId)
          case None =>
            state
      case _ =>
        state

  private def closeBufferState(state: AppState, bufferId: BufferId): AppState =
    val updatedPanes = state.layout.editorPanes.view.mapValues { pane =>
      if pane.bufferId.contains(bufferId) then pane.copy(bufferId = None) else pane
    }.toMap

    state.copy(
      buffers = state.buffers - bufferId,
      layout = state.layout.copy(editorPanes = updatedPanes)
    )

  private def closePaneState(state: AppState, paneId: PaneId): AppState =
    val updatedPanes = state.layout.editorPanes - paneId
    val newActivePaneId =
      if state.layout.activeEditorPaneId.contains(paneId) then updatedPanes.keys.headOption
      else state.layout.activeEditorPaneId

    val baseState = state.copy(
      layout = state.layout.copy(
        editorPanes = updatedPanes,
        activeEditorPaneId = newActivePaneId
      )
    )
    newActivePaneId match
      case Some(id) => baseState.copy(focus = Focus.EditorPane(id))
      case None     => toggleCommandRunner(baseState.copy(focus = Focus.EditorPane(PaneId(0))), CommandRegistry.default)

  private def navigateBuffer(
    state: AppState,
    nextBuffer: AppState => BufferId => Option[BufferId]
  ): AppState =
    if state.bufferOrder.isEmpty then state
    else
      state.focusedBufferId match
        case Some(currentBufferId) =>
          nextBuffer(state)(currentBufferId) match
            case Some(bufferId) =>
              focusBuffer(rebalancePanes(state, Some(bufferId)), bufferId)
            case None =>
              state
        case None =>
          state.bufferOrder.headOption match
            case Some(firstBufferId) => focusBuffer(state, firstBufferId)
            case None                => state

  private def asCommandRunner(surface: UiSurface): Option[(UiSurface, CommandRunner)] =
    surface.content match
      case SurfaceContent.CommandPalette(runner) => Some((surface, runner))
      case _                                     => None

  private def upsertSurface(surfaces: List[UiSurface], surface: UiSurface): List[UiSurface] =
    surfaces.filterNot(_.id == surface.id) :+ surface
