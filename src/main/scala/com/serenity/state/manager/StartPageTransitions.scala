package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.state.models.*

/** Replacing the editor with the start page once the session snapshot has been written off the dispatcher (#1911). */
private[manager] object StartPageTransitions:

  /** The start page in place of `live` -- which keeps the chrome it has now, not the snapshot's -- or `live` itself if
    * a buffer was opened, closed or edited since `saved` was taken: the page would offer to resume a session that is no
    * longer the one on screen.
    */
  def withStartPageShown(
    live: AppState,
    saved: AppState,
    readableRecentFiles: List[Path],
    readableRecentFolders: List[Path] = Nil
  ): AppState =
    if !sameBuffers(live, saved) then live
    else
      startPageStateFrom(
        live,
        StartupPageContent.createStartPage(
          sessionExists = true,
          recentFiles = readableRecentFiles,
          resumeIdentifier = Some(StartupPageContent.sessionResumeIdentifier(saved)),
          fileOrFolderOpen = live.runtime.capabilities.opensFileOrFolder,
          recentFolders = readableRecentFolders
        )
      )

  def sameBuffers(live: AppState, saved: AppState): Boolean =
    live.persisted.bufferOrder == saved.persisted.bufferOrder &&
      live.persisted.buffers.view.mapValues(_.document.contentVersion).toMap ==
      saved.persisted.buffers.view.mapValues(_.document.contentVersion).toMap

  private def startPageStateFrom(committed: AppState, page: StartupPage): AppState =
    val startPageSurfaceId = SurfaceId("surface-0")
    val base               = AppState.empty(committed.persisted.config)
    base.copy(
      persisted = base.persisted.copy(
        focus = Focus.Surface(startPageSurfaceId),
        theme = committed.persisted.theme
      ),
      runtime = base.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            id = startPageSurfaceId,
            content = SurfaceContent.StartPage(page),
            presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        ),
        viewportSize = committed.runtime.viewportSize,
        nextSurfaceId = SurfaceIdSupply(1),
        capabilities = committed.runtime.capabilities
      )
    )
