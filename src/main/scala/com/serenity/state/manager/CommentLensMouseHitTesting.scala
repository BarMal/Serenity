package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** State the event pipeline exposes for clicking inside the floating comment lens's body. */
private[manager] trait CommentLensMouseHitTestingPort:
  def stateRef: Ref[IO, AppState]

/** Routes a primary click that lands inside a *read-only* floating comment lens's body to the existing editable state
  * (#1222) -- the read-only display only reachable by clicking a highlighted comment range in floating display mode
  * (see `MouseHitTesting`'s editor-click fallback). A lens already in the editable state was fully interactive before
  * this feature and needs no new click handling here; it's left to the generic
  * `MouseHitTestGeometry.isInsideFloatingSurface` swallow later in the dispatch chain.
  */
final private[manager] class CommentLensMouseHitTesting(port: CommentLensMouseHitTestingPort):
  import port.*

  def handleCommentLensMouseClick(click: MouseClick, state: AppState): IO[Boolean] =
    readOnlyLensClickedInBody(click, state) match
      case Some((surface, lens)) =>
        stateRef.update(s => replaceLensMode(s, surface, lens.copy(mode = CommentLensMode.Editable))).as(true)
      case None =>
        IO.pure(false)

  private def readOnlyLensClickedInBody(
    click: MouseClick,
    state: AppState
  ): Option[(UiSurface, CommentLensState)] =
    for
      surface <- state.commentLensSurface
      lens <- surface.content match
        case SurfaceContent.CommentLens(lens) if lens.mode == CommentLensMode.ReadOnly => Some(lens)
        case _                                                                         => None
      viewportSize <- state.runtime.viewportSize
      if MouseHitTestGeometry.insideFloatingSurface(click, state, viewportSize, surface)
    yield (surface, lens)

  private def replaceLensMode(state: AppState, surface: UiSurface, lens: CommentLensState): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case current if current.id == surface.id => current.copy(content = SurfaceContent.CommentLens(lens))
      case current                             => current
    }))
