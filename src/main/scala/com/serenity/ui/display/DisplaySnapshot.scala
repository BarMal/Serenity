package com.serenity.ui.display

import com.serenity.ui.display.DisplayScale.DeviceScale

/** What the window and the toolkit say about their size once the window is shown, in one log line, so a window that is
  * the wrong size under one toolkit can be compared with the same window under another (#2479).
  */
final case class DisplaySnapshot(
    toolkitClass: String,
    scale: DeviceScale,
    frameWidthPx: Int,
    frameHeightPx: Int,
    canvasWidthPx: Int,
    canvasHeightPx: Int,
    preferredWidthPx: Option[Int],
    preferredHeightPx: Option[Int]
):

  def logLine: String = line("")

  /** The same line, said once the window's size had stopped changing (or had not, within the wait). */
  def settledLogLine(settled: Boolean): String = line(s"settled=$settled ")

  def sameWindowSizeAs(other: DisplaySnapshot): Boolean =
    frameWidthPx == other.frameWidthPx && frameHeightPx == other.frameHeightPx &&
      canvasWidthPx == other.canvasWidthPx && canvasHeightPx == other.canvasHeightPx

  private def line(prefix: String): String =
    val preferred = (preferredWidthPx, preferredHeightPx) match
      case (Some(width), Some(height)) => s"${width}x$height"
      case _                           => "none"
    s"[DISPLAY] ${prefix}toolkit=$toolkitClass scale=${scale.x}x${scale.y} frame=${frameWidthPx}x$frameHeightPx " +
      s"canvas=${canvasWidthPx}x$canvasHeightPx preferred=$preferred"
