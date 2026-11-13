package com.serenity.ui.skia

import java.awt.Cursor
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.terminal.SwingWindow
import org.jetbrains.skia.{Bitmap, Image, Picture}

/** The GPU layer's image frames without a GL layer: [[LayerSurfaceFactory]] paints each frame on the persistent surface
  * and hands the snapshot to a stub that keeps only the latest, so a benchmark times what the window's frame costs (the
  * painting and the snapshot) without a display or a readback into an AWT image.
  */
private[serenity] object DiscardingImageLayer:

  private val latest = new AtomicReference(Option.empty[Image])

  final private class Host extends LayerHost:
    def baseAvailable: Boolean = latest.get().isDefined

    def publish(picture: Picture): Unit = picture.close()

    def publishImage(image: Image): Unit = latest.getAndSet(Some(image)).foreach(_.close())

    def showCarets(carets: List[SwingWindow.CaretPaint]): Unit = ()
    def setCursor(cursor: Cursor): Unit                        = ()
    def dispose(): Unit                                        = latest.getAndSet(None).foreach(_.close())
    def onRendered(callback: () => Unit): Unit                 = ()
    def renderApi: String                                      = "DISCARDING"

  val factory: FrameSurfaceFactory =
    new LayerSurfaceFactory(_ => new Host, SkiaRasterSurfaceFactory, LayerFrames.Image)

  /** Whether the latest frame holds anything but its first pixel's colour along its middle row. */
  def latestHasInk: Boolean =
    latest.get().exists { image =>
      val bitmap = Bitmap.Companion.makeFromImage(image)
      val row    = image.getHeight / 2
      (0 until image.getWidth).exists(x => bitmap.getColor(x, row) != bitmap.getColor(0, row))
    }
