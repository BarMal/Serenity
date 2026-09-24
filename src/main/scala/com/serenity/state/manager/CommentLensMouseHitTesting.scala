package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{ReducerResult, Transition}

/** State the event pipeline exposes for clicking inside the floating comment lens's body, as a capability record rather
  * than a trait -- nothing here breaks a construction-order cycle (#1389), so mockability is the only reason this needs
  * an interface at all, and a record fakes trivially without one (#1017).
  */
final private[manager] case class CommentLensMouseHitTestingPort(
    stateRef: Ref[IO, AppState],
    applyReducerResult: (ReducerResult, AppState) => IO[Unit]
)

/** Routes a primary click that lands inside a *read-only* floating comment lens's body to the existing editable state
  * (#1222) -- the read-only display only reachable by clicking a highlighted comment range in floating display mode
  * (see `MouseHitTesting`'s editor-click fallback). A lens already in the editable state was fully interactive before
  * this feature and needs no new click handling here; it's left to the generic
  * `MouseHitTestGeometry.isInsideFloatingSurface` swallow later in the dispatch chain.
  *
  * Left as a single whole-body containment check rather than migrated onto `CommentLensSurfaceComposition`'s paint
  * boxes (issue #819, slice 3): the lens is one contiguous draft text region, not a list of discrete rows a user picks
  * between, and its UX contract (`CommentClickSpec`) is "click anywhere in the body to start editing", not "click a
  * specific row" -- so there is no per-row target for a composition `hitAt` lookup to distinguish. Building one would
  * be a hit-testing surface with nothing for it to resolve differently than this existing check already does.
  */
final private[manager] class CommentLensMouseHitTesting(port: CommentLensMouseHitTestingPort):

  def handleCommentLensMouseClick(click: MouseClick, state: AppState): IO[Boolean] =
    MouseTransition.commit(port.stateRef, port.applyReducerResult)(CommentLensMouseHitTesting.click(click, state))

private[manager] object CommentLensMouseHitTesting:

  def click(click: MouseClick, state: AppState): Transition[Boolean] =
    readOnlyLensClickedInBody(click, state) match
      case Some((surface, lens)) =>
        Transition.modify(replaceLensMode(_, surface, lens.copy(mode = CommentLensMode.Editable))).as(true)
      case None =>
        Transition.pure(false)

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
    state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.replacedWhere(_.id == surface.id)(_.copy(content = SurfaceContent.CommentLens(lens)))
      )
    )
