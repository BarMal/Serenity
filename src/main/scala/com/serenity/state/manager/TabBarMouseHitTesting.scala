package com.serenity.state.manager

import com.serenity.state.models.{BufferId, TabListEntry}
import com.serenity.ui.layout.{LayoutRect, TabBarSurfaceComposition}

/** Per-tab hit regions for the always-visible tab strip (issue #1075: Foundation; close affordance, #1078). Resolves a
  * click's cell coordinate to the `BufferId` of the tab (`hitAt`) or its close affordance (`closeHitAt`) it landed on,
  * using the exact same `ResolvedSurfaceComposition`/`closeAffordances` geometry `TextOverlayRenderer` paints from --
  * the same "painted and hit-tested from one plan" guarantee `ModalMouseHitTesting`/`CommandRunnerMouseHitTesting`
  * already give their own surfaces.
  *
  * Scope note: this object only defines *where* a hit lands, not what to do about it. Dispatching a resolved hit as a
  * live mouse event -- switching to it (#1077) or closing it (`GlobalAppEvent.CloseTabById`, #1078) -- and resolving
  * `rect`/`entries` from live `AppState` the way every other `*MouseHitTesting` sibling resolves its surface's
  * on-screen frame from a placed `UiSurface`, is `TabBarComponent` (#1077), deliberately out of this slice.
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
