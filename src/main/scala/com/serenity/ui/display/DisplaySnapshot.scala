package com.serenity.ui.display

import java.awt.{Component, Toolkit}
import javax.swing.SwingUtilities

import com.serenity.config.PreferredWindowSize
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

object DisplaySnapshot:

  /** The frame is the canvas's window, so a canvas with no window reads as a 0x0 frame. */
  def of(canvas: Component, preferred: Option[PreferredWindowSize]): DisplaySnapshot =
    val frame = Option(SwingUtilities.getWindowAncestor(canvas))
    DisplaySnapshot(
      toolkitClass = Toolkit.getDefaultToolkit.getClass.getName,
      scale = DisplayScale.forComponent(canvas),
      frameWidthPx = frame.fold(0)(_.getWidth),
      frameHeightPx = frame.fold(0)(_.getHeight),
      canvasWidthPx = canvas.getWidth,
      canvasHeightPx = canvas.getHeight,
      preferredWidthPx = preferred.map(_.width),
      preferredHeightPx = preferred.map(_.height)
    )
