package com.serenity.ui.benchmark

import java.awt.GraphicsEnvironment

import scala.util.Try

/** How often the display refreshes, which is the unit a frame's latency is counted in. `assumed` is set where the
  * display reported no rate, as virtual and headless screens do, and 60 Hz stands in.
  */
final case class RefreshRate(hz: Double, assumed: Boolean):

  def nanos: Long = math.round(1_000_000_000.0 / hz)

object RefreshRate:

  val Assumed60: RefreshRate = RefreshRate(60.0, assumed = true)

  def of(reportedHz: Int): RefreshRate =
    if reportedHz > 0 then RefreshRate(reportedHz.toDouble, assumed = false) else Assumed60

  def current: RefreshRate =
    if GraphicsEnvironment.isHeadless then Assumed60
    else
      Try(GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDisplayMode.getRefreshRate)
        .fold(_ => Assumed60, of)
