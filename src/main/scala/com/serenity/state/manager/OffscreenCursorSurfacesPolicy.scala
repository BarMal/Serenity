package com.serenity.state.manager

import com.serenity.config.OffscreenCursorSurfaces
import com.serenity.state.models.*
import com.serenity.ui.layout.{LaidOutPanes, LayoutEngine}

/** A surface anchored to the caret that opens with the caret scrolled out of its pane brings the two back together, as
  * `editor.cursor.offscreen_surfaces` says (#2570): the view to the caret, or the caret to the middle of the view.
  * Applied once, when the surface opens; a surface already open stays clamped inside the pane while the wheel moves the
  * view, since snapping back on every notch would leave the wheel nothing to do.
  */
private[manager] object OffscreenCursorSurfacesPolicy:

  def settled(previous: AppState, next: AppState): AppState =
    next.persisted.config.surfaceConfig.offscreenCursorSurfaces match
      case OffscreenCursorSurfaces.Clamp => next
      case choice if opensCursorSurface(previous, next) =>
        (for
          size   <- next.runtime.viewportSize
          paneId <- next.persisted.layout.activeEditorPaneId
          buffer <- next.persisted.layout.editorPanes
            .get(paneId)
            .flatMap(_.bufferId)
            .flatMap(next.persisted.buffers.get)
          cursor <- buffer.editing.cursorPositions.headOption
          panes = LayoutEngine.laidOutPanes(next, LayoutEngine.calculateLayoutWithUI(next, size))
          if panes.screenPosition(paneId, cursor).isEmpty
        yield choice match
          case OffscreenCursorSurfaces.SummonCursor => summoned(next, paneId, buffer, panes)
          case _                                    => revealed(next, buffer, cursor)
        ).getOrElse(next)
      case _ => next

  private def opensCursorSurface(previous: AppState, next: AppState): Boolean =
    val before = previous.runtime.uiSurfaces.map(_.id).toSet
    next.runtime.uiSurfaces.exists(surface => anchoredToCaret(surface) && !before.contains(surface.id))

  private def anchoredToCaret(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor | SurfacePlacement.AboveCursor) => true
      case _                                                                                            => false

  private def revealed(state: AppState, buffer: Buffer, cursor: CursorPosition): AppState =
    val surfaceConfig = state.persisted.config.surfaceConfig
    val viewport =
      if surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled then
        CursorViewport.adjustForCursorColumnMode(buffer, state, cursor)
      else CursorViewport.adjustForCursor(buffer, state, cursor)
    withBuffer(state, buffer.copy(viewport = viewport))

  private def summoned(state: AppState, paneId: PaneId, buffer: Buffer, panes: LaidOutPanes): AppState =
    val middleRow = panes.layouts.get(paneId).map(layout => layout.contentRect.y + layout.contentRect.height / 2)
    middleRow.flatMap(panes.cursorOnRow(paneId, _)).fold(state) { cursor =>
      withBuffer(state, buffer.copy(editing = EditingState(List(cursor))))
    }

  private def withBuffer(state: AppState, buffer: Buffer): AppState =
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(buffer.id, buffer)))
