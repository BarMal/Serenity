package com.serenity.app

import java.nio.file.{Files, Path}

import cats.effect.IO
import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.session.UnreadableSession
import com.serenity.state.manager.*
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager

object AppStartup:

  /** `recentFiles` must already be filtered to existing, readable files -- `createStartPage` does no filesystem access
    * of its own, so callers filter via `IO.blocking` before calling this. Delegates to [[StartupPageContent]] so the
    * same page can be rebuilt from the state layer (the "return to start page" command) without a package cycle.
    */
  def createStartPage(
    sessionExists: Boolean,
    recentFiles: List[Path] = Nil,
    configNotice: Option[String] = None,
    resumeIdentifier: Option[String] = None,
    fileOrFolderOpen: Boolean = false
  ): StartupPage =
    StartupPageContent.createStartPage(sessionExists, recentFiles, configNotice, resumeIdentifier, fileOrFolderOpen)

  def startPageState(
    sessionService: SessionService,
    sessionStartupInfo: SessionStartupInfo,
    theme: Theme,
    initialViewportSize: ViewportSize,
    appConfig: AppConfig = AppConfig.default,
    capabilities: FrontendCapabilities = FrontendCapabilities.gui,
    configNotice: Option[String] = None
  ): IO[AppState] =
    for
      sessionExists <- sessionStartupInfo.sessionExists
      loadedSession <- sessionService.loadSession
      recentFiles = loadedSession.fold(List.empty[Path])(_.persisted.recentFiles)
      readableRecentFiles <- IO.blocking(
        recentFiles.filter(path => Files.isRegularFile(path) && Files.isReadable(path))
      )
      resumeIdentifier = loadedSession.map(StartupPageContent.sessionResumeIdentifier)
      startPage = createStartPage(
        sessionExists,
        readableRecentFiles,
        configNotice,
        resumeIdentifier,
        capabilities.opensFileOrFolder
      )
    yield
      val startPageSurfaceId = SurfaceId("surface-0")
      val base               = AppState.empty(appConfig)
      base.copy(
        persisted = base.persisted.copy(
          focus = Focus.Surface(startPageSurfaceId),
          theme = theme
        ),
        runtime = base.runtime.copy(
          uiSurfaces = List(
            UiSurface(
              id = startPageSurfaceId,
              content = SurfaceContent.StartPage(startPage),
              presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
            )
          ),
          viewportSize = Some(initialViewportSize),
          nextSurfaceId = SurfaceIdSupply(1),
          capabilities = capabilities
        )
      )

  def startupTheme(
    sessionStartupInfo: SessionStartupInfo,
    themeManager: AppThemeManager,
    fallbackThemeName: String = "dark"
  ): IO[Theme] =
    for
      savedThemeName <- sessionStartupInfo.currentSessionThemeName
      theme          <- themeManager.initializeWithTheme(savedThemeName.getOrElse(fallbackThemeName))
    yield theme

  /** Sets an unreadable session aside before anything can save over it, whether or not a file was asked for, then tells
    * the user about it in a prompt. A notice also goes on the start page when there is one, and into a prompt when a
    * launch straight into a file means there is none.
    */
  def initializeState(
    stateManager: StateManager,
    sessionStartupInfo: SessionStartupInfo,
    theme: Theme,
    initialViewportSize: ViewportSize,
    appConfig: AppConfig = AppConfig.default,
    openPath: Option[Path] = None,
    capabilities: FrontendCapabilities = FrontendCapabilities.gui,
    configNotice: Option[String] = None,
    recovery: StartupRecovery.Plan = StartupRecovery.Plan.normal
  ): IO[AppState] =
    initialState(
      stateManager,
      sessionStartupInfo,
      theme,
      initialViewportSize,
      appConfig,
      openPath,
      capabilities,
      configNotice
    )
      .flatMap(state =>
        if recovery == StartupRecovery.Plan.normal then IO.pure(state)
        else stateManager.updateStateValidated(recovery.appliedTo) >> stateManager.getCurrentState
      )

  private def initialState(
    stateManager: StateManager,
    sessionStartupInfo: SessionStartupInfo,
    theme: Theme,
    initialViewportSize: ViewportSize,
    appConfig: AppConfig,
    openPath: Option[Path],
    capabilities: FrontendCapabilities,
    configNotice: Option[String]
  ): IO[AppState] =
    for
      unreadable <- sessionStartupInfo.setAsideUnreadableSession
      startPageNotice = Option((configNotice.toList ++ unreadable.map(_.summary)).mkString(" ")).filter(_.nonEmpty)
      _ <- openPath.fold(
        startPageState(
          stateManager.sessionService,
          sessionStartupInfo,
          theme,
          initialViewportSize,
          appConfig,
          capabilities,
          startPageNotice
        ).flatMap(startState => stateManager.updateStateValidated(_ => startState))
      )(openStraightInto(stateManager, _, theme, initialViewportSize, appConfig, capabilities))
      unseenNotice = configNotice.filter(_ => openPath.isDefined)
      _     <- stateManager.updateStateValidated(withStartupPrompts(_, unreadable, unseenNotice))
      state <- stateManager.getCurrentState
    yield state

  private def openStraightInto(
    stateManager: StateManager,
    path: Path,
    theme: Theme,
    initialViewportSize: ViewportSize,
    appConfig: AppConfig,
    capabilities: FrontendCapabilities
  ): IO[Unit] =
    stateManager.updateStateValidated { _ =>
      val base = AppState.empty(appConfig)
      base.copy(
        persisted = base.persisted.copy(theme = theme),
        runtime = base.runtime.copy(
          viewportSize = Some(initialViewportSize),
          capabilities = capabilities
        )
      )
    } >> stateManager.fileOpener.openFile(path)

  def withStartupPrompts(state: AppState, unreadable: Option[UnreadableSession], notice: Option[String]): AppState =
    val prompts =
      notice.map(ConfirmPrompt.startupNotice).toList ++
        unreadable.map(session => ConfirmPrompt.sessionNotRestored(session.describe, session.recoveredTexts))
    prompts.foldLeft(state)((current, prompt) => ModalStateReducer.show(Modal.Confirm(prompt), current).state)
