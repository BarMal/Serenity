package com.serenity.state.manager

import cats.effect.IO
import com.serenity.state.models.*
import com.serenity.state.reducers.ReducerResult
import com.serenity.ui.layout.ViewportSize

final private[manager] class StateManagerViewportCapability(
    modelCommit: ModelCommit,
    logger: org.typelevel.log4cats.Logger[IO],
    deviceTextScaleProvider: IO[Double],
    effects: StateManagerEffectHandlers
):

  private def updateFontConfig(
    update: com.serenity.ui.fonts.FontLoader.FontConfig => com.serenity.ui.fonts.FontLoader.FontConfig
  ): IO[Unit] =
    effects.updateFontConfig(update)

  val scrollManager: ScrollManager = ScrollManager(
    ensureCursorVisible = ensureCursorVisible,
    clickMinimap = clickMinimap
  )

  private def commit(reduce: AppState => ReducerResult): IO[Unit] =
    modelCommit.currentState.flatMap(state => modelCommit.commitState(reduce(state).state, state))

  private def ensureCursorVisible(paneId: PaneId): IO[Unit] =
    commit(ViewportStateReducer.ensureCursorVisible(paneId, _))

  private def clickMinimap(paneId: PaneId, targetLine: Int): IO[Unit] =
    commit(ViewportStateReducer.clickMinimap(paneId, targetLine, _))

  def handleViewportResize(newSize: ViewportSize): IO[Unit] =
    logger.debug(s"Handling viewport resize to ${newSize.width}x${newSize.height}") >>
      refreshAutoTextScale >>
      commit(ViewportStateReducer.resize(newSize, _))

  private def refreshAutoTextScale: IO[Unit] =
    deviceTextScaleProvider.flatMap { deviceScale =>
      modelCommit.currentState.flatMap { state =>
        val fontConfig = state.persisted.config.editorConfig.fontConfig
        if fontConfig.resolveAutoTextScale(deviceScale) == fontConfig then IO.unit
        else updateFontConfig(identity)
      }
    }
