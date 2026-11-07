package com.serenity.ui.renderer

import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.PixelRect

/** Where a frame goes: the screen it will be shown on, and the region sink that screen's repaint should honour --
  * `None` for the whole canvas, otherwise the disjoint rects outside which the frame matches what is on screen.
  * `lastPlan` holds the plan the latest frame drew by, for a test to report when that frame turns out wrong.
  */
final case class FrameOutput(
    screenToken: ScreenIdentity,
    repaintRegion: AtomicReference[Option[List[PixelRect]]],
    private[serenity] val lastPlan: AtomicReference[Option[FramePlan]] = new AtomicReference(None)
):

  /** Publishes what the frame decided: the region its screen may keep, and the plan it drew by. */
  private[renderer] def commit(plan: Option[FramePlan]): Unit =
    repaintRegion.set(plan.flatMap(_.repaintRegion))
    lastPlan.set(plan)

  /** A frame that repainted everything: no region to keep and no plan. */
  private[renderer] def reset(): Unit = commit(None)
