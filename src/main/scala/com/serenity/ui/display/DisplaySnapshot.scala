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

  def logLine: String =
    val preferred = (preferredWidthPx, preferredHeightPx) match
      case (Some(width), Some(height)) => s"${width}x$height"
      case _                           => "none"
    s"[DISPLAY] toolkit=$toolkitClass scale=${scale.x}x${scale.y} frame=${frameWidthPx}x$frameHeightPx " +
      s"canvas=${canvasWidthPx}x$canvasHeightPx preferred=$preferred"
