package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.*
import cats.effect.std.Queue
import com.serenity.config.PreferredWindowSize
import com.serenity.io.{FileDialog, FileManager}
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence}
import com.serenity.state.models.AppState
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.typelevel.log4cats.Logger

private[manager] case class StateManagerRuntime(
    stateRef: Ref[IO, AppState],
    undoRef: Ref[IO, UndoState],
    themeNamesRef: Ref[IO, List[String]],
    quitSignal: Deferred[IO, Unit],
    logger: Logger[IO],
    policy: SessionManager.SessionPolicy,
    themeManager: AppThemeManager,
    lspQueue: Queue[IO, LspEffect],
    mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]],
    documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    onFontConfigChanged: FontConfig => IO[Unit],
    deviceTextScaleProvider: IO[Double],
    configPersistencePath: Option[Path],
    uiPresetStore: UiPresetStore,
    windowSizeProvider: IO[Option[PreferredWindowSize]],
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit],
    fileDialog: FileDialog,
    fileManager: FileManager,
    sessionManager: SessionManager,
    sessionPersistence: SessionPersistence
)

private[manager] object StateManagerRuntime:

  def create(
    stateRef: Ref[IO, AppState],
    undoRef: Ref[IO, UndoState],
    themeNamesRef: Ref[IO, List[String]],
    quitSignal: Deferred[IO, Unit],
    logger: Logger[IO],
    policy: SessionManager.SessionPolicy,
    sessionRootOverride: Option[Path],
    themeManager: AppThemeManager,
    lspQueue: Queue[IO, LspEffect],
    mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]],
    documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    onFontConfigChanged: FontConfig => IO[Unit],
    deviceTextScaleProvider: IO[Double],
    configPersistencePath: Option[Path],
    uiPresetStore: UiPresetStore,
    windowSizeProvider: IO[Option[PreferredWindowSize]],
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit],
    fileDialog: FileDialog
  )(using Balance): StateManagerRuntime =
    val sessionManager = sessionRootOverride
      .map(root => SessionManager.create(root, themeManager, logger, policy))
      .getOrElse(SessionManager.create(themeManager, logger, policy))
    StateManagerRuntime(
      stateRef = stateRef,
      undoRef = undoRef,
      themeNamesRef = themeNamesRef,
      quitSignal = quitSignal,
      logger = logger,
      policy = policy,
      themeManager = themeManager,
      lspQueue = lspQueue,
      mouseTargetCacheRef = mouseTargetCacheRef,
      documentAnalysisFiberRef = documentAnalysisFiberRef,
      onFontConfigChanged = onFontConfigChanged,
      deviceTextScaleProvider = deviceTextScaleProvider,
      configPersistencePath = configPersistencePath,
      uiPresetStore = uiPresetStore,
      windowSizeProvider = windowSizeProvider,
      onPreferredWindowSizeChanged = onPreferredWindowSizeChanged,
      fileDialog = fileDialog,
      fileManager = new FileManager(),
      sessionManager = sessionManager,
      sessionPersistence = new SessionPersistence(sessionManager, policy, logger)
    )
