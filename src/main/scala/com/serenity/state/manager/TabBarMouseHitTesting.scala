package com.serenity.state.manager

import com.serenity.state.core.EditorState
import com.serenity.state.models.{AppState, BufferId, SurfaceContent, TabListEntry}
import com.serenity.ui.layout.{LayoutEngine, LayoutRect, TabBarSurfaceComposition}

/** Per-tab hit regions for the always-visible tab strip (issue #1075: Foundation; close affordance, #1078), and
  * resolving a primary click against them into a buffer switch (issue #1077). Resolves a click's cell coordinate to the
  * `BufferId` of the tab (`hitAt`) or its close affordance (`closeHitAt`) it landed on, using the exact same
  * `ResolvedSurfaceComposition`/`closeAffordances` geometry `TextOverlayRenderer` paints from -- the same "painted and
  * hit-tested from one plan" guarantee `ModalMouseHitTesting`/`CommandRunnerMouseHitTesting` already give their own
  * surfaces.
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
    * switch click (#1077, via `hitAt`) never contend for the same region.
    */
  def closeHitAt(
    entries: List[TabListEntry],
    rect: LayoutRect,
    col: Double,
    row: Double
  ): Option[BufferId] =
    TabBarSurfaceComposition
      .closeAffordances(entries, rect)
      .reverse
      .find(_.rect.contains(col, row))
      .flatMap(hit => TabBarSurfaceComposition.closeBufferIdOf(hit.focusId))
