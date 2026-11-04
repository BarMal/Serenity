package com.serenity.ui.renderer

import java.awt.Graphics2D
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.PixelRect

/** Keeps a Java2D surface's draws off the pixels a frame holds correct (#2481). A clip on the graphics context itself
  * would make every draw take Java2D's slow path for a non-rectangular clip, about doubling a scroll frame, so only the
  * draws that can reach a kept rect are clipped, and the rest keep the rectangular one.
  */
private[renderer] trait Java2DKeptOff extends PanelOutlineDrawing:

  protected def clipGraphics: Graphics2D

  private val keptRects = AtomicReference[List[PixelRect]](Nil)

  override def keepsOffPixels: Boolean = true

  override def withoutPixels(kept: List[PixelRect])(render: => Unit): Unit =
    val before = keptRects.getAndSet(kept)
    try render
    finally keptRects.set(before)

  /** Runs `draw`, whose ink stays within a little more than the box `left, top, right, bottom`, off the kept rects.
    * What is left of that reach is a few rectangles, each a plain rectangular clip for `draw`: a clip of any other
    * shape sends Java2D down its slow path for every draw it touches.
    */
  protected def awayFromKept(left: Int, top: Int, right: Int, bottom: Int)(draw: => Unit): Unit =
    val margin = (bottom - top).max(1) / 2
    val near   = PreservedPixels.near(keptRects.get, left, top, right, bottom, margin)
    if near.isEmpty then draw
    else
      val reach = PixelRect(left - margin, top - margin, right - left + 2 * margin, bottom - top + 2 * margin)
      val saved = clipGraphics.getClip
      try
        PixelRect.uncoveredWithin(reach, near).foreach { piece =>
          clipGraphics.clipRect(piece.xPx, piece.yPx, piece.widthPx, piece.heightPx)
          draw
          clipGraphics.setClip(saved)
        }
      finally clipGraphics.setClip(saved)

  /** The sprite is blitted through a graphics context of its own, which the surface's clip does not reach: the clip
    * that keeps a draw off kept pixels is a rectangle, and it is handed on in device pixels.
    */
  protected def clippedLikeSurface(device: Graphics2D): Unit =
    Option(clipGraphics.getClipBounds).foreach { bounds =>
      val transform = clipGraphics.getTransform
      val left      = math.floor(bounds.x * transform.getScaleX + transform.getTranslateX).toInt
      val top       = math.floor(bounds.y * transform.getScaleY + transform.getTranslateY).toInt
      val right     = math.ceil((bounds.x + bounds.width) * transform.getScaleX + transform.getTranslateX).toInt
      val bottom    = math.ceil((bounds.y + bounds.height) * transform.getScaleY + transform.getTranslateY).toInt
      device.clipRect(left, top, right - left, bottom - top)
    }
