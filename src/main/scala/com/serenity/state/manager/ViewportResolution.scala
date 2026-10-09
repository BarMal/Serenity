package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.layout.WrappedLineCache

/** Splits `CursorViewport.ensureVisibleCursors` into the cheap half that records a caret move and the measuring half
  * that places the viewport against it, so a run of moves can be recorded one at a time and placed once.
  *
  * `resolve` relies on `CursorViewport.adjustForCursor` placing a viewport from the caret, the content, the viewport
  * size and the config alone, so placing later gives the same answer as placing straight after the move.
  */
object ViewportResolution:

  /** Marks every buffer whose primary cursor moved from `before` to `after` as `FollowCaret`, by the same test
    * `CursorViewport.ensureVisibleCursors` uses. A buffer already following the caret stays so.
    */
  def markFollow(before: AppState, after: AppState): AppState =
    replacingBuffers(after) { (bufferId, buffer) =>
      val headMoved =
        before.persisted.buffers
          .get(bufferId)
          .exists(_.editing.cursorPositions.headOption != buffer.editing.cursorPositions.headOption)
      Option.when(headMoved && buffer.viewport.placement != ViewportPlacement.FollowCaret)(
        buffer.copy(viewport = buffer.viewport.copy(placement = ViewportPlacement.FollowCaret))
      )
    }

  /** Places every buffer marked `FollowCaret` against its primary cursor and marks it `Placed`; the very same state if
    * none is.
    */
  def resolve(state: AppState, wrapCache: WrappedLineCache = WrappedLineCache.Uncached): AppState =
    replacingBuffers(state) { (_, buffer) =>
      Option.when(buffer.viewport.placement == ViewportPlacement.FollowCaret)(placed(state, buffer, wrapCache))
    }

  private def placed(state: AppState, buffer: Buffer, wrapCache: WrappedLineCache): Buffer =
    val surfaceConfig    = state.persisted.config.surfaceConfig
    val columnModeActive = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
    val viewport = buffer.editing.cursorPositions.headOption.fold(buffer.viewport) { cursor =>
      if columnModeActive then CursorViewport.adjustForCursorColumnMode(buffer, state, cursor, wrapCache)
      else CursorViewport.adjustForCursor(buffer, state, cursor, wrapCache)
    }
    buffer.copy(viewport = viewport.copy(placement = ViewportPlacement.Placed))

  private def replacingBuffers(state: AppState)(replacement: (BufferId, Buffer) => Option[Buffer]): AppState =
    val replaced =
      state.persisted.buffers.flatMap((bufferId, buffer) => replacement(bufferId, buffer).map(bufferId -> _))
    if replaced.isEmpty then state
    else state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers ++ replaced))
