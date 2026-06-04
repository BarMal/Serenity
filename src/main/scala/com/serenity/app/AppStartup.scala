package com.serenity.app

import com.serenity.config.AppConfig
import cats.effect.IO
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme

object AppStartup:

  def createStartPage(sessionExists: Boolean): StartupPage =
    val statusMessage =
      if sessionExists then None
      else Some("No previous session found")

    StartupPage(
      title = "What would you like to do?",
      options = List(
        "1. Start a new session",
        "2. Restore an existing session",
        "3. Open an existing file or directory"
      ),
      statusMessage = statusMessage
    )

  def startPageState(
    stateManager: StateManager,
    theme: Theme,
    initialViewportSize: ViewportSize,
    appConfig: AppConfig = AppConfig.default
  ): IO[AppState] =
    for
      sessionExists <- stateManager.sessionExists
      startPage = createStartPage(sessionExists)
    yield
      val startPageSurfaceId = SurfaceId("surface-0")
      AppState.empty.copy(
        focus = Focus.Surface(startPageSurfaceId),
        config = appConfig,
        uiSurfaces = List(
          UiSurface(
            id = startPageSurfaceId,
            content = SurfaceContent.StartPage(startPage),
            presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        ),
        viewportSize = Some(initialViewportSize),
        theme = theme,
        nextSurfaceId = 1
      )

  /** Initialize the application state for first render using the active theme and current viewport size. */
  def initializeState(
    stateManager: StateManager,
    theme: Theme,
    initialViewportSize: ViewportSize,
    appConfig: AppConfig = AppConfig.default
  ): IO[AppState] =
    for
      startState <- startPageState(stateManager, theme, initialViewportSize, appConfig)
      _          <- stateManager.updateState(_ => startState)
      state      <- stateManager.getCurrentState
    yield state
