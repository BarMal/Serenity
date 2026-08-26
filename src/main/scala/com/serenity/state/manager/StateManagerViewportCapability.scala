package com.serenity.state.manager

import cats.effect.IO
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEventReducer, SystemEventReducer}
import com.serenity.ui.layout.ViewportSize

final private[manager] class StateManagerViewportCapability(
    stateRef: cats.effect.Ref[IO, AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    deviceTextScaleProvider: IO[Double],
    dependencies: ViewportCapabilityPort
)(using balance: com.serenity.rope.Balance):

  import dependencies.*

  def ensureCursorVisible(paneId: PaneId): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.buffers.get) match
            case Some(buffer) =>
              val cursor        = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
              val updatedBuffer = buffer.copy(viewport = CursorViewport.adjustForCursor(buffer, state, cursor))
              state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
            case None => state
        case None => state
    }

  def smoothScrollTo(paneId: PaneId, targetLine: Int): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          val updatedPane = pane.copy(
            smoothScrolling = Some(SmoothScrollState(targetTopLine = targetLine, progress = 0.0))
          )
          state.copy(layout =
            state.layout.copy(
              editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
            )
          )
        case None => state
    }

  def progressSmoothScroll(paneId: PaneId, progress: Double): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.buffers.get) match
            case Some(buffer) =>
              pane.smoothScrolling match
                case Some(SmoothScrollState(targetTopLine, _)) =>
                  val currentTopLine = buffer.viewport.topLine
                  val (newTopLine, newSmoothing) =
                    if progress >= 1.0 then (targetTopLine, None)
                    else
                      val interpolated =
                        math.round(currentTopLine + progress * (targetTopLine - currentTopLine)).toInt
                      (interpolated, Some(SmoothScrollState(targetTopLine, progress)))

                  val updatedBuffer =
                    buffer.copy(viewport = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0))
                  val updatedPane = pane.copy(smoothScrolling = newSmoothing)

                  state.copy(
                    buffers = state.buffers + (buffer.id -> updatedBuffer),
                    layout = state.layout.copy(
                      editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                    )
                  )
                case None => state
            case None => state
        case None => state
    }

  def clickMinimap(paneId: PaneId, targetLine: Int): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.buffers.get) match
            case Some(buffer) =>
              val halfVisible = buffer.viewport.visibleLines / 2
              val newTopLine  = math.max(0, targetLine - halfVisible)
              val updatedBuffer = buffer.copy(
                editing = buffer.editing.copy(cursors = List(CursorPosition(targetLine, 0))),
                viewport = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
              )
              state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
            case None => state
        case None => state
    }

  def handleViewportResize(newSize: ViewportSize): IO[Unit] =
    for
      _            <- logger.debug(s"Handling viewport resize to ${newSize.width}x${newSize.height}")
      _            <- refreshAutoTextScale
      currentState <- stateRef.get
      resizedState = SystemEventReducer.reduce(com.serenity.keystroke.events.ResizeEvent(newSize), currentState).state
      rebalancedState = AppEventReducer.rebalancePanes(resizedState, resizedState.focusedBufferId)
      _ <- validateAndUpdateState(rebalancedState, currentState)
    yield ()

  private def refreshAutoTextScale: IO[Unit] =
    deviceTextScaleProvider.flatMap { deviceScale =>
      stateRef.get.flatMap { state =>
        val fontConfig = state.config.fontConfig
        if fontConfig.resolveAutoTextScale(deviceScale) == fontConfig then IO.unit
        else updateFontConfig(identity)
      }
    }
