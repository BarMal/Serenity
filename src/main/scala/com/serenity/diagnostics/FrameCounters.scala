package com.serenity.diagnostics

import java.util.Locale
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLongArray}
import java.util.function.LongBinaryOperator

/** What a frame drew, counted rather than timed: the figures that tell an over-repainting frame from a frame that is
  * merely slow.
  */
enum FrameCounter(val label: String):
  case RowsDrawn       extends FrameCounter("rows-drawn")
  case RowsPreserved   extends FrameCounter("rows-preserved")
  case RunsDrawn       extends FrameCounter("runs")
  case ShapingCalls    extends FrameCounter("shapings")
  case LayerComposites extends FrameCounter("layer-composites")

  /** Device pixels of layer image the frame had to allocate; a layer buffer recycled from an earlier frame adds none.
    */
  case LayerBufferPx extends FrameCounter("layer-px")

  /** Times the frame worked out its overlay view model, which one frame should do once. */
  case OverlayModelBuilds extends FrameCounter("overlay-models")

  /** 1 on a frame that repainted any chrome, so its mean over a window is the share of frames that did. */
  case ChromePainted extends FrameCounter("chrome-painted")

  /** Logical pixels the frame published to the screen; the cell grid's area when the whole frame went. */
  case PresentAreaPx extends FrameCounter("present-px")

  /** Bytes the terminal frontend wrote, UTF-8, escape sequences included. */
  case TuiBytes extends FrameCounter("tui-bytes")

/** The counters of one finished frame. */
final case class FrameCounts(values: Map[FrameCounter, Long]):
  def apply(counter: FrameCounter): Long = values.getOrElse(counter, 0L)

object FrameCounts:
  val empty: FrameCounts = FrameCounts(Map.empty)

/** The counters of every frame finished in a window: how many frames, and each counter's total and largest frame. */
final case class FrameCounterStats(frames: Int, totals: Map[FrameCounter, Long], maxima: Map[FrameCounter, Long]):

  def mean(counter: FrameCounter): Double =
    if frames > 0 then totals.getOrElse(counter, 0L).toDouble / frames else 0.0

  def max(counter: FrameCounter): Long = maxima.getOrElse(counter, 0L)

  /** One `label avg=.. max=..` part per counter that was ever non-zero, or nothing for a window with no frames. */
  def logParts: List[String] =
    if frames == 0 then Nil
    else
      FrameCounter.values.toList.filter(counter => max(counter) > 0).map { counter =>
        String.format(Locale.ROOT, "%s avg=%.1f max=%d", counter.label, mean(counter), max(counter))
      }

object FrameCounterStats:
  val empty: FrameCounterStats = FrameCounterStats(0, Map.empty, Map.empty)

/** Per-frame counters, added to from the render thread as a frame draws and folded into the window when it finishes.
  *
  * Nothing is allocated per count: the figures sit in preallocated atomic arrays, and with counting off every hook is a
  * flag read. It is switched together with [[FrameTimings]] (`ui.render.frame_timing`) so a `[FRAME]` line carries
  * both.
  */
final class FrameCounters:
  import FrameCounters.*

  private val enabled = new AtomicBoolean(false)
  private val current = new AtomicLongArray(Size)
  private val latest  = new AtomicLongArray(Size)
  private val totals  = new AtomicLongArray(Size)
  private val maxima  = new AtomicLongArray(Size)
  private val frames  = new AtomicInteger(0)

  def isEnabled: Boolean = enabled.get()

  /** Starts from an empty window either way, so nothing counted before a toggle is reported after it. */
  def setEnabled(on: Boolean): Unit =
    discard()
    enabled.set(on)

  def add(counter: FrameCounter, amount: Long): Unit =
    if enabled.get() then
      val _ = current.addAndGet(counter.ordinal, amount)

  /** A frame that drew `drawn` of `total` rows kept the others from an earlier frame. */
  def rows(total: Int, drawn: Int): Unit =
    if enabled.get() then
      val redrawn = drawn.max(0).min(total)
      add(FrameCounter.RowsDrawn, redrawn.toLong)
      add(FrameCounter.RowsPreserved, (total - redrawn).max(0).toLong)

  /** Closes the frame being counted: its figures become [[lastFrame]] and join the window. */
  def frameFinished(): Unit =
    if enabled.get() then
      Ordinals.foreach { index =>
        val value = current.getAndSet(index, 0L)
        latest.set(index, value)
        val _ = totals.addAndGet(index, value)
        val _ = maxima.accumulateAndGet(index, value, Larger)
      }
      val _ = frames.incrementAndGet()

  def lastFrame: FrameCounts =
    FrameCounts(FrameCounter.values.map(counter => counter -> latest.get(counter.ordinal)).toMap)

  /** What the frames since the previous drain counted, starting a new window. */
  def drain(): FrameCounterStats =
    val frameCount = frames.getAndSet(0)
    val windowTotals =
      FrameCounter.values.map(counter => counter -> totals.getAndSet(counter.ordinal, 0L)).toMap
    val windowMaxima =
      FrameCounter.values.map(counter => counter -> maxima.getAndSet(counter.ordinal, 0L)).toMap
    FrameCounterStats(frameCount, windowTotals, windowMaxima)

  private def discard(): Unit =
    Ordinals.foreach { index =>
      current.set(index, 0L)
      latest.set(index, 0L)
    }
    val _ = drain()

object FrameCounters:
  private val Size                       = FrameCounter.values.length
  private val Ordinals                   = FrameCounter.values.indices
  private val Larger: LongBinaryOperator = (a, b) => math.max(a, b)
