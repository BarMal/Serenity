package com.serenity.ui.renderer

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Each pane's [[TextLayoutSnapshot]] as of the frame a screen last showed. A surface's own previous snapshots are the
  * frame it was last painted from, which in a pool of two images is the frame before the one on screen, so a repaint
  * region measured against them misses the rows an edit that goes A, B, A changed.
  */
final private[renderer] class ScreenSnapshots(capacity: AtomicInteger):
  private val shown = new BoundedRefCache[ScreenIdentity, Map[PaneId, TextLayoutSnapshot]](capacity)

  def shownOn(output: Option[FrameOutput]): Map[PaneId, TextLayoutSnapshot] =
    output.flatMap(value => shown.get(value.screenToken)).getOrElse(Map.empty)

  def remember(output: Option[FrameOutput], snapshots: Map[PaneId, TextLayoutSnapshot]): Unit =
    output.foreach(value => shown.put(value.screenToken, snapshots))

  def forget(screenToken: ScreenIdentity): Unit = shown.remove(screenToken)
