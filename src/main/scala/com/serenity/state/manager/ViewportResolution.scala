package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.layout.WrappedLineCache

/** Splits `CursorViewport.ensureVisibleCursors` into the cheap half that records a caret move and the measuring half
  * that places the viewport against it, so a run of moves can be recorded one at a time and placed once.
  *
  * A run placed once at its end rests where placing after each move would only where placement reads the caret, the
  * content, the viewport size and the config alone -- typewriter centring and column paging. Without typewriter
  * scrolling the view keeps where it was until the caret nears an edge, so a run can rest a little differently from the
  * same keys placed one at a time; placing each key would re-wrap the edited paragraph once per key (#1879).
  */
object ViewportResolution:

  /** Marks every buffer whose primary cursor moved from `before` to `after` as `FollowCaret`, by the same test
    * `CursorViewport.ensureVisibleCursors` uses. A buffer already following the caret stays so.
    */
  def markFollow(before: AppState, after: AppState, keyedIn: Option[BufferId] = None): AppState =
    replacingBuffers(after) { (bufferId, buffer) =>
      Option.when(
        needsPlacing(before, bufferId, buffer, keyedIn) && buffer.viewport.placement != ViewportPlacement.FollowCaret
      )(buffer.copy(viewport = buffer.viewport.copy(placement = ViewportPlacement.FollowCaret)))
    }

  /** Whether `buffer` has to be placed against its caret: its primary cursor moved from `before`, or a key went to it
    * (`keyedIn`) while the wheel had its view scrolled away from the caret.
    */
  def needsPlacing(before: AppState, bufferId: BufferId, buffer: Buffer, keyedIn: Option[BufferId]): Boolean =
    val headMoved =
      before.persisted.buffers
        .get(bufferId)
        .exists(_.editing.cursorPositions.headOption != buffer.editing.cursorPositions.headOption)
    headMoved || (keyedIn.contains(bufferId) && buffer.viewport.placement == ViewportPlacement.Scrolled)

  /** The buffer the focused editor pane shows, which a caret or edit key goes to. */
  def focusedEditorBuffer(state: AppState): Option[BufferId] =
    state.persisted.focus match
      case Focus.EditorPane(paneId) => state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
      case _                        => None

  /** Places every buffer marked `FollowCaret` against its primary cursor and marks it `Placed`; the very same state if
    * none is.
    */
  def resolve(state: AppState, wrapCache: WrappedLineCache = WrappedLineCache.Uncached): AppState =
    replacingBuffers(state) { (_, buffer) =>
      Option.when(buffer.viewport.placement == ViewportPlacement.FollowCaret)(placed(state, buffer, wrapCache))
    }

  private def placed(state: AppState, buffer: Buffer, wrapCache: WrappedLineCache): Buffer =
    val viewport = buffer.editing.cursorPositions.headOption.fold(buffer.viewport) { cursor =>
      CursorViewport.placementFor(buffer, state, cursor, wrapCache)
    }
    buffer.copy(viewport = viewport.copy(placement = ViewportPlacement.Placed))

  private def replacingBuffers(state: AppState)(replacement: (BufferId, Buffer) => Option[Buffer]): AppState =
    val replaced =
      state.persisted.buffers.flatMap((bufferId, buffer) => replacement(bufferId, buffer).map(bufferId -> _))
    if replaced.isEmpty then state
    else state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers ++ replaced))
