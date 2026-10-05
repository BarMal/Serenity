package com.serenity.state.reducers

import com.serenity.config.CornerPosition
import com.serenity.state.models.*

/** Corner notices (#1717): floating surfaces that never take focus. `FocusScopes` already closes an unfocused peek on
  * Escape and lets every other key through, so a notice only needs the peek focus policy -- and, unlike a peek, it
  * outlives the next key press.
  */
object NoticeReducer:

  val Corner: CornerPosition = CornerPosition.BottomRight

  /** More than this and the oldest goes: a corner stacked to the top of the window hides the text being worked on. */
  val MaxShown: Int = 3

  /** `notice` in the corner, replacing an older notice on its topic; one that dismisses itself leaves once [[expired]]
    * is given a time `autoDismissAfter` past `nowNanos`.
    */
  def shown(state: AppState, notice: Notice, nowNanos: Long): AppState =
    val (withId, surfaceId) = state.allocateSurfaceId
    val expiresAt           = notice.level.autoDismissAfter.map(after => nowNanos + after.toNanos)
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.Notice(notice, expiresAt),
      presentation = SurfacePresentation.Floating(None, SurfacePlacement.Corner(Corner))
    )
    val kept =
      withId.runtime.uiSurfaces.filterNot(existing => notice.topic.exists(topic => topicOf(existing).contains(topic)))
    val overflow = kept.filter(isNotice).dropRight(MaxShown - 1).map(_.id).toSet
    withSurfaces(withId, kept.filterNot(existing => overflow.contains(existing.id)) :+ surface)

  /** Removes the notices whose time is up by `nowNanos`; the same instance when there are none. */
  def expired(state: AppState, nowNanos: Long): AppState =
    def timeIsUp(surface: UiSurface): Boolean =
      surface.content match
        case SurfaceContent.Notice(_, Some(expiresAt)) => expiresAt <= nowNanos
        case _                                         => false
    without(state, timeIsUp)

  def withoutTopic(state: AppState, topic: NoticeTopic): AppState =
    without(state, topicOf(_).contains(topic))

  /** The notices on screen, oldest first. */
  def visible(state: AppState): List[Notice] =
    state.runtime.uiSurfaces.collect { case UiSurface(_, SurfaceContent.Notice(notice, _), _, _) => notice }

  private def without(state: AppState, remove: UiSurface => Boolean): AppState =
    if state.runtime.uiSurfaces.exists(remove) then withSurfaces(state, state.runtime.uiSurfaces.filterNot(remove))
    else state

  private def withSurfaces(state: AppState, surfaces: List[UiSurface]): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = surfaces))

  private def topicOf(surface: UiSurface): Option[NoticeTopic] =
    surface.content match
      case SurfaceContent.Notice(notice, _) => notice.topic
      case _                                => None

  private def isNotice(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.Notice(_, _) => true
      case _                           => false
