package com.serenity.state.manager

import cats.syntax.all.*
import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.MouseClick
import com.serenity.state.core.EditorState
import com.serenity.state.models.{AppState, BufferId, SurfaceContent, TabListEntry}
import com.serenity.state.reducers.{AppEffect, Transition}
import com.serenity.ui.layout.{LayoutEngine, LayoutRect, TabBarSurfaceComposition}

/** Per-tab hit regions for the always-visible tab strip (issue #1075: Foundation; close affordance, #1078), and
  * resolving a primary click against them into a buffer switch (issue #1077) or, via the trailing new-tab (+)
  * affordance, a new tab (issue #1080). Resolves a click's cell coordinate to the `BufferId` of the tab (`hitAt`) or
  * its close affordance (`closeHitAt`) it landed on, using the exact same
  * `ResolvedSurfaceComposition`/`closeAffordances`/`newTabAffordance` geometry `TextOverlayRenderer` paints from -- the
  * same "painted and hit-tested from one plan" guarantee `ModalMouseHitTesting`/`CommandRunnerMouseHitTesting` already
  * give their own surfaces.
  */
private[manager] object TabBarMouseHitTesting:

  def hitAt(
    entries: List[TabListEntry],
    activeBufferId: Option[BufferId],
    rect: LayoutRect,
    col: Double,
    row: Double
  ): Option[BufferId] =
    TabBarSurfaceComposition
      .forTabBar(entries, activeBufferId, rect)
      .hitAt(col, row)
      .flatMap(hit => TabBarSurfaceComposition.bufferIdOf(hit.focusId))

  /** Resolves a primary click's cell coordinate against `state`'s own tab strip -- `AppState.tabBarSurface` for its
    * entries/active buffer, `LayoutEngine.calculateLayoutWithUI`'s `tabBarRect` for the same on-screen frame the strip
    * is painted at (mirroring `EditorContextMenuHitTesting.contextMenuHitAt`'s use of a resolved surface frame).
    *
    * `None` means the click missed the strip entirely -- there is no tab bar this frame, or `(col, row)` falls outside
    * its rect -- so the caller should keep resolving other mouse targets. `Some` means the strip claims the click, and
    * carries the buffer to switch the active pane to: `Some(None)` for a click that lands inside the strip but off any
    * tab (an inter-tab gap) or squarely on the tab that is already active (issue #1077: "clicking the active tab is a
    * no-op") -- either way the click must still be swallowed, not fall through to the editor beneath.
    */
  def clickTarget(state: AppState, col: Int, row: Int): Option[Option[BufferId]] =
    for
      surface      <- state.tabBarSurface
      viewportSize <- state.runtime.viewportSize
      rect         <- LayoutEngine.calculateLayoutWithUI(state, viewportSize).tabBarRect
      _            <- Option.when(rect.contains(col, row))(())
    yield
      val (entries, activeBufferId) = surface.content match
        case SurfaceContent.TabBar(entries, activeBufferId) => (entries, activeBufferId)
        // Unreachable: AppState.tabBarSurface only ever builds SurfaceContent.TabBar.
        case _ => (Nil, None)
      hitAt(entries, activeBufferId, rect, col.toDouble, row.toDouble).filterNot(activeBufferId.contains)

  /** Applies [[clickTarget]]'s resolution to `state`, switching the active pane to the clicked buffer via
    * `EditorState.switchToBuffer` -- the same helper keyboard `NextTab`/`PreviousTab` navigation uses -- when a
    * different tab was clicked, or leaving `state` untouched for a swallowed click that hit no tab (a gap) or the
    * already-active one. `None` (click missed the strip) is left to the caller, which should keep resolving other mouse
    * targets rather than treating the click as handled.
    */
  def handleClick(state: AppState, col: Int, row: Int): Option[AppState] =
    clickTarget(state, col, row).map(_.fold(state)(EditorState.switchToBuffer(state, _)))

  /** Resolves a click's cell coordinate to the `BufferId` of the tab whose close (x) affordance it landed on (issue
    * #1078) -- the close-specific counterpart to `hitAt` above, resolved from
    * `TabBarSurfaceComposition.closeAffordances` rather than `forTabBar`'s own hit regions, so a close click and a
    * switch click (#1077, via `hitAt`) never contend for the same region. `activeBufferId` must match the value
    * `hitAt`/`forTabBar` were resolved with for this same click, so both agree on which tabs are visible under overflow
    * (issue #1081).
    */
  def closeHitAt(
    entries: List[TabListEntry],
    activeBufferId: Option[BufferId],
    rect: LayoutRect,
    col: Double,
    row: Double
  ): Option[BufferId] =
    TabBarSurfaceComposition
      .closeAffordances(entries, activeBufferId, rect)
      .reverse
      .find(_.rect.contains(col, row))
      .flatMap(hit => TabBarSurfaceComposition.closeBufferIdOf(hit.focusId))

  /** Resolves a primary click's cell coordinate against `state`'s own tab strip's trailing new-tab (+) affordance
    * (issue #1080) -- the same `AppState.tabBarSurface`/`LayoutEngine.calculateLayoutWithUI` resolution `clickTarget`
    * uses, but checked against `TabBarSurfaceComposition.newTabAffordance` instead of `hitAt`, so a new-tab click and a
    * switch click (`clickTarget`, #1077) always resolve from disjoint region sets. `false` covers both "the click
    * missed the strip" and "the click landed in the strip but not on the affordance" -- either way the caller should
    * keep resolving other mouse targets/tab-strip outcomes.
    */
  def newTabClickTarget(state: AppState, col: Int, row: Int): Boolean =
    (for
      surface      <- state.tabBarSurface
      viewportSize <- state.runtime.viewportSize
      rect         <- LayoutEngine.calculateLayoutWithUI(state, viewportSize).tabBarRect
      _            <- Option.when(rect.contains(col, row))(())
    yield
      val entries = surface.content match
        case SurfaceContent.TabBar(entries, _) => entries
        // Unreachable: AppState.tabBarSurface only ever builds SurfaceContent.TabBar.
        case _ => Nil
      TabBarSurfaceComposition.newTabAffordance(entries, rect).exists(_.rect.contains(col.toDouble, row.toDouble))
    ).getOrElse(false)

  /** Applies [[newTabClickTarget]]'s resolution to `state`, opening a new tab via `EditorState.openNewTab` -- the same
    * helper keyboard `NewTab` (Ctrl+T) uses -- when the click landed on the affordance, or `None` otherwise so the
    * caller keeps resolving other mouse targets.
    */
  def handleNewTabClick(state: AppState, col: Int, row: Int)(using com.serenity.rope.Balance): Option[AppState] =
    Option.when(newTabClickTarget(state, col, row))(EditorState.openNewTab(state))

  /** Resolves a primary click against the always-visible tab strip: the trailing new-tab affordance (issue #1080), a
    * tab's close affordance (#1078, #1673), or a tab itself (issue #1077). `false` means the click missed the strip and
    * the caller should keep resolving other mouse targets; a click inside the strip that hits nothing is still claimed,
    * so it never reaches the editor beneath.
    */
  def click(click: MouseClick, state: AppState)(using com.serenity.rope.Balance): Transition[Boolean] =
    if newTabClickTarget(state, click.col, click.row) then Transition.modify(EditorState.openNewTab).as(true)
    else
      closeClickTarget(state, click.col, click.row).flatMap(closeTab) match
        case Some(close) => close.as(true)
        case None =>
          clickTarget(state, click.col, click.row) match
            case Some(switchTo) =>
              Transition.modify(current => switchTo.fold(current)(EditorState.switchToBuffer(current, _))).as(true)
            case None =>
              Transition.pure(false)

  /** [[closeHitAt]] resolved against `state`'s own tab strip, the same surface and frame [[clickTarget]] uses. */
  def closeClickTarget(state: AppState, col: Int, row: Int): Option[BufferId] =
    for
      surface      <- state.tabBarSurface
      viewportSize <- state.runtime.viewportSize
      rect         <- LayoutEngine.calculateLayoutWithUI(state, viewportSize).tabBarRect
      (entries, activeBufferId) <- surface.content match
        case SurfaceContent.TabBar(entries, activeBufferId) => Some((entries, activeBufferId))
        case _                                              => None
      bufferId <- closeHitAt(entries, activeBufferId, rect, col.toDouble, row.toDouble)
    yield bufferId

  /** #1673: a close click must prompt for unsaved changes exactly as keyboard `CloseTab` does, so it runs the same
    * `close` command rather than dropping the buffer here. That workflow only ever closes the active editor's buffer,
    * hence the switch to the clicked tab first -- which also puts the buffer a save prompt asks about on screen.
    */
  private def closeTab(bufferId: BufferId): Option[Transition[Unit]] =
    CommandRegistry.withToggleUI.findCommand("close").map { close =>
      Transition.modify(EditorState.switchToBuffer(_, bufferId)) *> Transition.emit(AppEffect.ExecuteCommand(close))
    }
