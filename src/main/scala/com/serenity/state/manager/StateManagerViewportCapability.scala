package com.serenity.state.manager

import cats.effect.IO
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEventReducer, Focused, SystemEventReducer}
import com.serenity.ui.layout.ViewportSize

final private[manager] class StateManagerViewportCapability(
    stateRef: cats.effect.Ref[IO, AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    deviceTextScaleProvider: IO[Double],
    events: StateManagerEventPipeline,
    effects: StateManagerEffectHandlers
):

  private def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
    events.validateAndUpdateState(newState, fallbackState)

  private def updateFontConfig(
    update: com.serenity.ui.fonts.FontLoader.FontConfig => com.serenity.ui.fonts.FontLoader.FontConfig
  ): IO[Unit] =
    effects.updateFontConfig(update)

  val scrollManager: ScrollManager = ScrollManager(
    ensureCursorVisible = ensureCursorVisible,
    smoothScrollTo = smoothScrollTo,
    progressSmoothScroll = progressSmoothScroll,
    clickMinimap = clickMinimap
  )

  private def ensureCursorVisible(paneId: PaneId): IO[Unit] =
    stateRef.update { state =>
      Focused.bufferOf(state, paneId) match
        case Some(buffer) =>
          val cursor        = buffer.editing.cursors.primaryCursor
          val updatedBuffer = buffer.copy(viewport = CursorViewport.adjustForCursor(buffer, state, cursor))
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> updatedBuffer)))
        case None => state
    }

  private def smoothScrollTo(paneId: PaneId, targetLine: Int): IO[Unit] =
    stateRef.update { state =>
      state.persisted.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          val updatedPane = pane.copy(
            smoothScrolling = Some(SmoothScrollState(targetTopLine = targetLine, progress = 0.0))
          )
          state.copy(persisted =
            state.persisted.copy(layout =
              state.persisted.layout.copy(
                editorPanes = state.persisted.layout.editorPanes + (paneId -> updatedPane)
              )
            )
          )
        case None => state
    }

  private def progressSmoothScroll(paneId: PaneId, progress: Double): IO[Unit] =
    stateRef.update { state =>
      state.persisted.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.persisted.buffers.get) match
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
                    persisted = state.persisted.copy(
                      buffers = state.persisted.buffers + (buffer.id -> updatedBuffer),
                      layout = state.persisted.layout.copy(
                        editorPanes = state.persisted.layout.editorPanes + (paneId -> updatedPane)
                      )
                    )
                  )
                case None => state
            case None => state
        case None => state
    }

  private def clickMinimap(paneId: PaneId, targetLine: Int): IO[Unit] =
    stateRef.get.flatMap { state =>
      val nextState = state.persisted.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.persisted.buffers.get) match
            case Some(buffer) =>
              // `targetLine` is derived from a click row against the minimap's rendering of the buffer at resolve
              // time; if the document has since shrunk (a concurrent edit/undo racing the click), it can land past
              // the buffer's current line count, so it is clamped here rather than trusted as already in-bounds.
              val clampedLine  = math.max(0, math.min(targetLine, math.max(0, buffer.document.content.lineCount - 1)))
              val halfVisible  = buffer.viewport.visibleLines / 2
              val newTopLine   = math.max(0, clampedLine - halfVisible)
              val updatedBuffer = buffer.copy(
                editing = buffer.editing.copy(cursors = List(CursorPosition(clampedLine, 0))),
                viewport = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
              )
              state.copy(persisted =
                state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> updatedBuffer))
              )
            case None => state
        case None => state
      validateAndUpdateState(nextState, state)
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
        val fontConfig = state.persisted.config.editorConfig.fontConfig
        if fontConfig.resolveAutoTextScale(deviceScale) == fontConfig then IO.unit
        else updateFontConfig(identity)
      }
    }
