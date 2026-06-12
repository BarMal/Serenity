package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import com.serenity.config.PreferredWindowSize
import com.serenity.io.FileManager
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
    onFontConfigChanged: FontConfig => IO[Unit],
    configPersistencePath: Option[Path],
    uiPresetStore: UiPresetStore,
    windowSizeProvider: IO[Option[PreferredWindowSize]],
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit],
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
    onFontConfigChanged: FontConfig => IO[Unit],
    configPersistencePath: Option[Path],
    uiPresetStore: UiPresetStore,
    windowSizeProvider: IO[Option[PreferredWindowSize]],
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit]
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
      onFontConfigChanged = onFontConfigChanged,
      configPersistencePath = configPersistencePath,
      uiPresetStore = uiPresetStore,
      windowSizeProvider = windowSizeProvider,
      onPreferredWindowSizeChanged = onPreferredWindowSizeChanged,
      fileManager = new FileManager(),
      sessionManager = sessionManager,
      sessionPersistence = new SessionPersistence(sessionManager, policy, logger)
    )
