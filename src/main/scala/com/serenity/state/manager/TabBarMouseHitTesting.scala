package com.serenity.state.manager

import com.serenity.state.models.{BufferId, TabListEntry}
import com.serenity.ui.layout.{LayoutRect, TabBarSurfaceComposition}

/** Per-tab hit regions for the always-visible tab strip (issue #1075: Foundation). Resolves a click's cell coordinate
  * to the `BufferId` of the tab it landed on, using the exact same `ResolvedSurfaceComposition` `TextOverlayRenderer`
  * paints from -- the same "painted and hit-tested from one plan" guarantee `ModalMouseHitTesting`/
  * `CommandRunnerMouseHitTesting` already give their own surfaces.
  *
  * Scope note: this object only defines *where* each tab's hit region is. Turning a hit into a switch/close/reorder
  * action -- and resolving `rect`/`entries` from live `AppState` the way every other `*MouseHitTesting` sibling
  * resolves its surface's on-screen frame from a placed `UiSurface` -- is issue #1077 onward, deliberately out of
  * this slice.
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
