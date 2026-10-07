package com.serenity.ui.accessibility

import com.serenity.state.models.*
import com.serenity.ui.layout.*

private[accessibility] object AccessibilityTabBar:

  /** Per-tab, close-affordance and new-tab-affordance nodes for the always-visible tab strip (issue #1611), plus its
    * own container node -- built straight from `AppState.tabBarSurface`/`CalculatedLayout.tabBarRect` rather than from
    * a `SceneNode`, since the strip never becomes one (see `from`'s own comment). `None` (one or no open buffers, or no
    * `tabBarRect` reserved this frame) yields no nodes at all, matching the strip itself not being painted.
    */
  private[accessibility] def tabBarNodes(state: AppState, calculatedLayout: CalculatedLayout): List[AccessibleNode] =
    (for
      surface <- state.tabBarSurface
      rect    <- calculatedLayout.tabBarRect
    yield surface.content match
      case content @ SurfaceContent.TabBar(entries, activeBufferId) =>
        val containerNode = AccessibleNode(
          s"surface:${surface.id.value}",
          AccessibilitySnapshot.surfaceRole(content),
          AccessibilitySnapshot.surfaceName(content),
          AccessibleValue.plain(AccessibilitySnapshot.surfaceValue(content)),
          selected = false,
          focused = state.persisted.focus == Focus.Surface(surface.id),
          rect
        )
        containerNode :: tabBarControls(surface.id, entries, activeBufferId, rect, state)
      // Unreachable: AppState.tabBarSurface only ever builds SurfaceContent.TabBar.
      case _ => Nil
    ).getOrElse(Nil)

  private def tabBarControls(
    surfaceId: SurfaceId,
    entries: List[TabListEntry],
    activeBufferId: Option[BufferId],
    rect: LayoutRect,
    state: AppState
  ): List[AccessibleNode] =
    val composition = TabBarSurfaceComposition.forTabBar(entries, activeBufferId, rect)
    val tabNodes = composition.hitRegions.flatMap { hit =>
      TabBarSurfaceComposition.bufferIdOf(hit.focusId).map { bufferId =>
        val selected = activeBufferId.contains(bufferId)
        AccessibleNode(
          s"surface:${surfaceId.value}/tab:${bufferId.value}",
          AccessibilityRole.Button,
          hit.semanticLabel,
          None,
          selected,
          state.persisted.focus == Focus.Surface(surfaceId) && selected,
          LayoutRect(hit.rect.x.toInt, hit.rect.y.toInt, hit.rect.width.toInt, hit.rect.height.toInt)
        )
      }
    }
    val closeNodes = TabBarSurfaceComposition.closeAffordances(entries, activeBufferId, rect).flatMap { hit =>
      TabBarSurfaceComposition.closeBufferIdOf(hit.focusId).map { bufferId =>
        AccessibleNode(
          s"surface:${surfaceId.value}/close:${bufferId.value}",
          AccessibilityRole.Button,
          hit.semanticLabel,
          None,
          selected = false,
          focused = false,
          LayoutRect(hit.rect.x.toInt, hit.rect.y.toInt, hit.rect.width.toInt, hit.rect.height.toInt)
        )
      }
    }
    val newTabNode = TabBarSurfaceComposition.newTabAffordance(entries, rect).toList.map { hit =>
      AccessibleNode(
        s"surface:${surfaceId.value}/new-tab",
        AccessibilityRole.Button,
        hit.semanticLabel,
        None,
        selected = false,
        focused = false,
        LayoutRect(hit.rect.x.toInt, hit.rect.y.toInt, hit.rect.width.toInt, hit.rect.height.toInt)
      )
    }
    tabNodes ++ closeNodes ++ newTabNode

  /** The tab strip's own accessible value: the active tab's title plus its position among the open tabs, e.g.
    * `"main.scala (2 of 4)"` -- there is otherwise no way for assistive tech to learn which tab is active or how many
    * are open without visiting every per-tab child node (issue #1611).
    */
  private[accessibility] def tabBarValue(
    entries: List[TabListEntry],
    activeBufferId: Option[BufferId]
  ): Option[String] =
    activeBufferId
      .flatMap(id => entries.zipWithIndex.find { case (entry, _) => entry.bufferId == id })
      .map { case (entry, index) => s"${entry.title} (${index + 1} of ${entries.size})" }
