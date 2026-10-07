package com.serenity.state.reducers

import com.serenity.config.CornerPosition
import com.serenity.lsp.LspEffect
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
    * is given a time `autoDismissAfter` past `nowNanos`. A question takes focus, over any earlier one still waiting,
    * and is never pushed out by the notices that follow it: its asker waits for an answer.
    */
  def shown(state: AppState, notice: Notice, nowNanos: Long): AppState =
    val (withId, surfaceId) = state.allocateSurfaceId
    val expiresAt           = notice.autoDismissAfter.map(after => nowNanos + after.toNanos)
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.Notice(notice, expiresAt),
      presentation = SurfacePresentation.Floating(None, SurfacePlacement.Corner(Corner))
    )
    val kept =
      withId.runtime.uiSurfaces.filterNot(existing => notice.topic.exists(topic => topicOf(existing).contains(topic)))
    val excess   = (kept.count(isNotice) - (MaxShown - 1)).max(0)
    val overflow = kept.filter(isNotice).filterNot(isPrompt).take(excess).map(_.id).toSet
    withSurfaces(withId, kept.filterNot(existing => overflow.contains(existing.id)) :+ surface)
      .pushFocusUnlessPeek(surface)

  /** Removes the notices whose time is up by `nowNanos`; the same instance when there are none. */
  def expired(state: AppState, nowNanos: Long): AppState =
    def timeIsUp(surface: UiSurface): Boolean =
      surface.content match
        case SurfaceContent.Notice(_, Some(expiresAt)) => expiresAt <= nowNanos
        case _                                         => false
    without(state, timeIsUp)

  def withoutTopic(state: AppState, topic: NoticeTopic): AppState =
    without(state, topicOf(_).contains(topic))

  /** Moves the highlight of question `id` by `step` actions, wrapping; from nothing highlighted, forwards lands on the
    * first action and backwards on the last.
    */
  def highlighted(state: AppState, id: NoticePromptId, step: Int): AppState =
    promptSurface(state, id).filter((_, _, prompt) => prompt.actions.nonEmpty).fold(state) {
      (surface, notice, prompt) =>
        val count = prompt.actions.size
        val target =
          prompt.highlighted.fold(if step >= 0 then 0 else count - 1)(current => Math.floorMod(current + step, count))
        val content =
          SurfaceContent.Notice(notice.copy(prompt = Some(prompt.copy(highlighted = Some(target)))), expiresAt(surface))
        withSurfaces(
          state,
          state.runtime.uiSurfaces
            .map(existing => if existing.id == surface.id then existing.copy(content = content) else existing)
        )
    }

  /** Closes question `id` with the user's `choice` -- an action's index, or `None` for a dismissal -- and tells the
    * language server side, which holds the request waiting. A question already withdrawn changes nothing.
    */
  def answered(state: AppState, id: NoticePromptId, choice: Option[Int]): ReducerResult =
    promptSurface(state, id).fold(ReducerResult.noEffects(state)) { (_, _, prompt) =>
      val valid = choice.filter(prompt.actions.indices.contains)
      ReducerResult(
        withoutPrompt(state, id),
        List(AppEffect.LspQueue(LspQueueEffect.Enqueue(LspEffect.MessageRequestAnswered(id, valid))))
      )
    }

  /** Closes question `id` because its asker stopped waiting, so there is nobody left to tell. */
  def withdrawn(state: AppState, id: NoticePromptId): AppState =
    if promptSurface(state, id).isDefined then withoutPrompt(state, id) else state

  /** The notices on screen, oldest first. */
  def visible(state: AppState): List[Notice] =
    state.runtime.uiSurfaces.collect { case UiSurface(_, SurfaceContent.Notice(notice, _), _, _) => notice }

  private def promptSurface(state: AppState, id: NoticePromptId): Option[(UiSurface, Notice, NoticePrompt)] =
    state.runtime.uiSurfaces.view.flatMap { surface =>
      surface.content match
        case SurfaceContent.Notice(notice, _) =>
          notice.prompt.filter(_.id == id).map(prompt => (surface, notice, prompt))
        case _ => None
    }.headOption

  private def withoutPrompt(state: AppState, id: NoticePromptId): AppState =
    val closing = promptSurface(state, id).map(_._1.id)
    val closed  = without(state, surface => closing.contains(surface.id))
    if closing.exists(surfaceId => state.persisted.focus == Focus.Surface(surfaceId)) then closed.popFocus else closed

  private def expiresAt(surface: UiSurface): Option[Long] =
    surface.content match
      case SurfaceContent.Notice(_, expiresAt) => expiresAt
      case _                                   => None

  private def without(state: AppState, remove: UiSurface => Boolean): AppState =
    if state.runtime.uiSurfaces.exists(remove) then withSurfaces(state, state.runtime.uiSurfaces.filterNot(remove))
    else state

  private def withSurfaces(state: AppState, surfaces: List[UiSurface]): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = surfaces))

  private def topicOf(surface: UiSurface): Option[NoticeTopic] =
    surface.content match
      case SurfaceContent.Notice(notice, _) => notice.topic
      case _                                => None

  private def isPrompt(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.Notice(notice, _) => notice.prompt.isDefined
      case _                                => false

  private def isNotice(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.Notice(_, _) => true
      case _                           => false
