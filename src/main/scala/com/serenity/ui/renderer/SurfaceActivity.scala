package com.serenity.ui.renderer

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.annotation.tailrec

import com.serenity.diagnostics.{FrameCounter, FrameCounters}

/** What a [[RenderSurface]] did while a frame drew into it, for [[FrameCounters]] to fold into the frame's figures.
  *
  * Counted by the surface itself because only it sees every run, composite and byte, whichever renderer asked for them.
  * A layer surface shares its parent's activity, so what a modal or panel painted into its own buffer still counts.
  */
final class SurfaceActivity:
  private val runs        = new AtomicInteger(0)
  private val shapings    = new AtomicLong(0)
  private val promotions  = new AtomicLong(0)
  private val composites  = new AtomicInteger(0)
  private val flushBytes  = new AtomicLong(0)
  private val layerPixels = new AtomicLong(0)

  def runDrawn(): Unit =
    val _ = runs.incrementAndGet()

  def shaped(count: Long): Unit =
    val _ = shapings.addAndGet(count)

  def promoted(count: Long): Unit =
    val _ = promotions.addAndGet(count)

  /** Runs `body`, which looks text up in `traffic`'s cache, and records the shapings and promotions it caused. */
  def counting[A](traffic: ShapeTraffic)(body: => A): A =
    val shapedBefore   = traffic.shapings
    val promotedBefore = traffic.promotions
    val answer         = body
    shaped(traffic.shapings - shapedBefore)
    promoted(traffic.promotions - promotedBefore)
    answer

  def layerComposited(): Unit =
    val _ = composites.incrementAndGet()

  def layerAllocated(devicePixels: Long): Unit =
    val _ = layerPixels.addAndGet(devicePixels)

  /** Bytes a flush wrote. They join the frame that closes next, so the caret flush a terminal makes after its frame's
    * content flush is counted with the frame after it, and a window's total is exact.
    */
  def flushed(bytes: Int): Unit =
    val _ = flushBytes.addAndGet(bytes.toLong)

  /** Adds what has happened since the previous call to `counters`. */
  def drainInto(counters: FrameCounters): Unit =
    counters.add(FrameCounter.RunsDrawn, runs.getAndSet(0).toLong)
    counters.add(FrameCounter.ShapingCalls, shapings.getAndSet(0))
    counters.add(FrameCounter.ShapePromotions, promotions.getAndSet(0))
    counters.add(FrameCounter.LayerComposites, composites.getAndSet(0).toLong)
    counters.add(FrameCounter.LayerBufferPx, layerPixels.getAndSet(0))
    counters.add(FrameCounter.TuiBytes, flushBytes.getAndSet(0))

object SurfaceActivity:

  def apply(): SurfaceActivity = new SurfaceActivity

  /** The UTF-8 size of `text`, counted without encoding it. */
  def utf8Length(text: String): Int = utf8Length(text, 0, 0)

  @tailrec
  private def utf8Length(text: String, index: Int, bytes: Int): Int =
    if index >= text.length then bytes
    else
      val char = text.charAt(index)
      if char < 0x80 then utf8Length(text, index + 1, bytes + 1)
      else if char < 0x800 then utf8Length(text, index + 1, bytes + 2)
      else if Character.isHighSurrogate(char) && pairedAt(text, index + 1) then utf8Length(text, index + 2, bytes + 4)
      else utf8Length(text, index + 1, bytes + 3)

  private def pairedAt(text: String, index: Int): Boolean =
    index < text.length && Character.isLowSurrogate(text.charAt(index))
