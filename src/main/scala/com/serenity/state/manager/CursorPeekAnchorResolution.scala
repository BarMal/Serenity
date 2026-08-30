package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.layout.{CursorLayout, LayoutEngine, ScreenPosition}

/** Resolves the experimental cursor-peek prototype's frozen `CursorPosition` (`AppState.runtime.cursorPeekAnchor`)
  * to an actual on-screen `ScreenPosition`, exactly once per peek session, and caches the result in
  * `AppState.runtime.cursorPeekResolvedAnchor`.
  *
  * Lives in `state.manager`, not a reducer: resolving a position requires `LayoutEngine`/`CursorLayout`, which
  * `ArchitectureChecks.ForbiddenImports` forbids `state.reducers` from touching (reducers must stay pure geometry-
  * free state transitions). This is the render-time counterpart `AppEventReducer`'s `PeekBegin` handling defers to.
  *
  * Deliberately idempotent past the first successful resolution: once `cursorPeekResolvedAnchor` is set, `resolve`
  * is a no-op even if the underlying buffer reflows underneath it (a reformat) -- that is the entire point of
  * caching the resolved `ScreenPosition` rather than re-deriving it from the frozen `CursorPosition` on every call,
  * which `LayoutEngine.calculateFrozenCursorPeekRect` then consumes verbatim instead of ever calling
  * `CursorLayout.calculateScreenPositionInContent` itself.
  */
private[manager] object CursorPeekAnchorResolution:

  def resolve(state: AppState): AppState =
    if state.runtime.cursorPeekResolvedAnchor.isDefined then state
    else
      resolvedPosition(state) match
        case Some(screenPosition) =>
          state.copy(runtime = state.runtime.copy(cursorPeekResolvedAnchor = Some(screenPosition)))
        case None => state

  private def resolvedPosition(state: AppState): Option[ScreenPosition] =
    for
      cursorPos    <- state.runtime.cursorPeekAnchor
      viewportSize <- state.runtime.viewportSize
      paneId       <- state.persisted.layout.activeEditorPaneId
      pane         <- state.persisted.layout.editorPanes.get(paneId)
      bufferId     <- pane.bufferId
      buffer       <- state.persisted.buffers.get(bufferId)
      layout        = LayoutEngine.calculateLayout(state, viewportSize)
      paneLayout   <- LayoutEngine.calculateEditorPaneLayouts(state, layout).get(paneId)
      screenPosition <- CursorLayout.calculateScreenPositionInContent(
        cursorPos,
        buffer.document.content,
        paneLayout.contentRect,
        buffer.viewport,
        state.persisted.config.surfaceConfig.wordWrapEnabled
      )
    yield screenPosition
