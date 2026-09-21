package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.{MouseButton, MouseDrag, MousePress}
import com.serenity.state.core.EditorState
import com.serenity.state.models.{AppState, BufferId, SurfaceContent, TabDragSession}
import com.serenity.ui.layout.LayoutEngine

/** State the event pipeline exposes for dragging a tab to reorder it, as a capability record rather than a trait --
  * nothing here breaks a construction-order cycle (#1389), so mockability is the only reason this needs an interface at
  * all, and a record fakes trivially without one (#1017).
  */
final private[manager] case class TabBarDragHitTestingPort(
    stateRef: Ref[IO, AppState],
    validateAndUpdateState: (AppState, AppState) => IO[Unit]
)

/** Drag-to-reorder for the always-visible tab strip (issue #1079). A primary press on a tab starts a [[TabDragSession]]
  * (`Runtime.tabDragSession`); each subsequent `MouseDrag` tick that lands on another tab reorders `bufferOrder` live
  * via `EditorState.reorderBuffer`, the same live-apply-per-tick pattern
  * `PinnedPanelMouseHitTesting.handlePinnedPanelResizeDrag`/`handleTextAreaResizeDrag` already use for resize, in place
  * of a deferred "apply on release" this app's input model has no event for (`MouseEvent.scala` has no mouse-release
  * event -- see [[TabDragSession]]'s own doc comment). A drag that never lands back on a different tab before the
  * gesture ends therefore leaves `bufferOrder` exactly as the last successful tick left it, which is already unchanged
  * if it never moved off its starting tab.
  */
final private[manager] class TabBarDragHitTesting(port: TabBarDragHitTestingPort):
  import port.*

  /** Starts (or clears) the drag session for a fresh primary-button press -- the only reliable "the previous drag
    * gesture ended" signal available, so this always resets `tabDragSession` first, whether or not the press itself
    * landed on a tab.
    */
  def handleTabBarPress(press: MousePress, state: AppState): IO[Boolean] =
    if press.button != MouseButton.Primary then IO.pure(false)
    else
      tabAt(state, press.col.toDouble, press.row.toDouble) match
        case Some(bufferId) =>
          stateRef.update(setSession(_, Some(TabDragSession(bufferId)))).as(true)
        case None =>
          stateRef.update(setSession(_, None)).as(false)

  def handleTabBarDrag(drag: MouseDrag, state: AppState): IO[Boolean] =
    if drag.button != MouseButton.Primary then IO.pure(false)
    else
      state.runtime.tabDragSession match
        case None => IO.pure(false)
        case Some(session) =>
          tabAt(state, drag.col.toDouble, drag.row.toDouble) match
            case Some(targetBufferId) if targetBufferId != session.bufferId =>
              stateRef.get
                .flatMap { current =>
                  validateAndUpdateState(EditorState.reorderBuffer(current, session.bufferId, targetBufferId), current)
                }
                .as(true)
            case _ =>
              // Off a tab, or back over the tab already being dragged -- the gesture continues, nothing to reorder
              // yet this tick.
              IO.pure(true)

  private def setSession(state: AppState, session: Option[TabDragSession]): AppState =
    state.copy(runtime = state.runtime.copy(tabDragSession = session))

  private def tabAt(state: AppState, col: Double, row: Double): Option[BufferId] =
    for
      viewportSize <- state.runtime.viewportSize
      rect         <- LayoutEngine.calculateLayoutWithUI(state, viewportSize).tabBarRect
      surface      <- state.tabBarSurface
      (entries, activeBufferId) <- surface.content match
        case SurfaceContent.TabBar(entries, activeBufferId) => Some((entries, activeBufferId))
        case _                                               => None
      hit                       <- TabBarMouseHitTesting.hitAt(entries, activeBufferId, rect, col, row)
    yield hit
