package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Color, Rectangle}

import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.Java2DRenderSurface.DeviceScale

/** A caret to fill over a presented frame. */
final case class CaretPaint(rect: Rectangle, color: Color)

/** What a target tells the renderer about the surface it paints: its size in logical pixels, the device scale between
  * those and the pixels it shows, and the identity that frames of one surface share their cached layers under.
  */
final case class SurfaceGeometry(
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    deviceScale: DeviceScale,
    identity: ScreenIdentity
):
  def deviceWidthPx: Int  = Java2DRenderSurface.deviceImageDimension(logicalWidthPx, deviceScale.x)
  def deviceHeightPx: Int = Java2DRenderSurface.deviceImageDimension(logicalHeightPx, deviceScale.y)

/** Where the renderer paints a window's frames. A window system implements it; the renderer asks it for the surface to
  * draw into and hands the finished frame back, with no window-system type crossing the boundary.
  */
trait RenderTarget:
  def metrics: CellMetrics
  def viewportSize: ViewportSize
  def geometry: SurfaceGeometry

  def acquireBaseImage(width: Int, height: Int, imageType: Int): BufferedImage

  /** Publish a finished base frame, repainting only `dirtyRects` when the rest is known to match what is shown. */
  def onImageReady(image: BufferedImage, dirtyRects: Option[List[Rectangle]]): Unit

  /** Publish a base frame whose repaint is left to the carets that follow it. */
  def onBaseImageReady(image: BufferedImage): Unit

  /** Publish the carets to fill over the current base frame; false when there is no base frame to fill them over. */
  def onCursorOverlayReady(baseDirtyRects: Option[List[Rectangle]])(paintCarets: => List[CaretPaint]): Boolean

object RenderTarget:

  /** A base frame is repainted before the carets only when no caret is drawn over it. */
  def shouldRepaintBaseFrameBeforeCursorOverlay(cursorVisible: Boolean): Boolean =
    !cursorVisible
