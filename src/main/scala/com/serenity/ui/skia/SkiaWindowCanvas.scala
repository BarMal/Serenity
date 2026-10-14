package com.serenity.ui.skia

import java.awt.Component
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javax.swing.SwingUtilities

import scala.util.Try

import com.serenity.ui.renderer.{HostedCanvas, HostedFrame, HostedOutcome, RenderSurface}
import org.jetbrains.skia.{Canvas, PixelGeometry}
import org.jetbrains.skiko.{SkiaLayer, SkiaLayerAnalytics, SkiaLayerProperties}

/** A window's [[SkiaLayer]], drawn only when the frame scheduler hands it a frame.
  *
  * The layer's render callback runs on the event-dispatch thread, and the scheduler's frame loop is another thread, so
  * [[drawFrame]] parks the frame for the callback, asks the layer for one render and waits for it. Nothing else asks
  * for renders. The layer still renders by itself after a resize or an exposure, which no frame answers; those redraw
  * the last frame, because a window's buffers are swapped and would otherwise show whatever the driver left in the back
  * one.
  *
  * Every frame builds its [[SkiaRenderSurface]] over the callback's canvas with the same [[HostedFrame.screen]], and
  * flushes it, which puts the canvas back as the layer handed it over.
  */
final class SkiaWindowCanvas private (layer: SkiaLayer) extends HostedCanvas:

  final private class Job(val frame: HostedFrame, val paint: RenderSurface => Unit):
    val finished = new CountDownLatch(1)
    val failure  = new AtomicReference[Option[Throwable]](None)

  private val parked    = new AtomicReference[Option[Job]](None)
  private val lastDrawn = new AtomicReference[Option[Job]](None)

  layer.setRenderDelegate((canvas, _, _, _) => render(canvas))

  def component: Component = layer

  def renderApi: String = layer.getRenderApi.toString

  def drawFrame(frame: HostedFrame)(paint: RenderSurface => Unit): HostedOutcome =
    if !layer.isShowing || layer.getWidth <= 0 || layer.getHeight <= 0 then
      HostedOutcome.Skipped(
        s"the canvas is not showing (showing=${layer.isShowing}, ${layer.getWidth}x${layer.getHeight})"
      )
    else
      val job    = new Job(frame, paint)
      val handle = Some(job)
      parked.set(handle)
      if SwingUtilities.isEventDispatchThread then layer.renderImmediately()
      else SwingUtilities.invokeLater(() => layer.needRender(false))
      await(job, handle)

  private def await(job: Job, handle: Option[Job]): HostedOutcome =
    val rendered = job.finished.await(SkiaWindowCanvas.RenderTimeoutMillis, TimeUnit.MILLISECONDS)
    if !rendered && parked.compareAndSet(handle, None) then
      HostedOutcome.Skipped(s"the canvas did not render within ${SkiaWindowCanvas.RenderTimeoutMillis} ms")
    else
      job.finished.await()
      job.failure.get().fold[HostedOutcome](HostedOutcome.Drawn)(HostedOutcome.Failed(_))

  private def render(canvas: Canvas): Unit =
    parked.getAndSet(None) match
      case Some(job) =>
        try Try(draw(canvas, job)).fold(error => job.failure.set(Some(error)), _ => lastDrawn.set(Some(job)))
        finally job.finished.countDown()
      case None =>
        lastDrawn.get() match
          case Some(job) => val _ = Try(draw(canvas, job))
          case None      => val _ = canvas.clear(SkiaWindowCanvas.OpaqueBlack)

  private def draw(canvas: Canvas, job: Job): Unit =
    val scale  = layer.getContentScale.toDouble
    val before = canvas.getSaveCount
    val surface = SkiaRenderSurface.onCanvas(
      canvas,
      job.frame.metrics,
      job.frame.font,
      layer.getWidth,
      layer.getHeight,
      scale,
      layerCacheOwner = Some(job.frame.screen)
    )
    try
      job.paint(surface)
      surface.flush()
    finally
      val _ = canvas.restoreToCount(before)

object SkiaWindowCanvas:

  private val RenderTimeoutMillis = 2000L
  private val OpaqueBlack         = 0xff000000

  /** The layer is built here and added to the window on the event-dispatch thread, which Skiko requires of it. */
  def create(): SkiaWindowCanvas = new SkiaWindowCanvas(newLayer())

  // The first argument is an optional accessibility factory that Skiko's Kotlin signature takes as a nullable function.
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def newLayer(): SkiaLayer =
    new SkiaLayer(
      null,
      new SkiaLayerProperties(),
      SkiaLayerAnalytics.Companion.getEmpty,
      PixelGeometry.UNKNOWN,
      false
    )
