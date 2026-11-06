package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicLong
import javax.swing.SwingUtilities

import org.jetbrains.skiko.SkiaLayer

/** How a hosted layer is asked to draw: after a frame was published to it, and after the carets over it changed. */
private[skia] trait LayerRedraw:

  def afterPublish(): Unit

  def afterCaretsChanged(): Unit

  /** The layer has begun a draw, which answers everything asked for until now. */
  def drawStarted(): Unit

private[skia] object LayerRedraw:

  /** A request that no draw has answered for this long is taken as lost (a window that was hidden when it was made). */
  val LostAfterNanos: Long = 250_000_000L

  private val NothingAsked = Long.MinValue

  /** A published frame is asked for through `repaint`, which also has the layer check its content scale. A change of
    * carets is asked for through `render`, which Skiko's frame driver takes only on the event thread, so it is handed
    * to `onEventThread`; that skips AWT's repaint queue and the extra paint pass `repaint` goes through.
    *
    * A draw reads the latest picture and the latest carets as it starts, so a draw already asked for answers every
    * change made before it begins, whichever route asked: only the first change after a draw asks, and a published
    * frame and its carets cost one draw instead of two. Each of them is on the event thread, and each draw there
    * flushes the whole picture (#2489).
    */
  def over(
    repaint: () => Unit,
    render: () => Unit,
    onEventThread: (() => Unit) => Unit,
    nanoTime: () => Long = () => System.nanoTime()
  ): LayerRedraw = new LayerRedraw:
    private val askedAt = new AtomicLong(NothingAsked)

    private def firstSinceDraw(): Boolean =
      val now  = nanoTime()
      val last = askedAt.get()
      (last == NothingAsked || now - last > LostAfterNanos) && askedAt.compareAndSet(last, now)

    def afterPublish(): Unit = if firstSinceDraw() then repaint()

    def afterCaretsChanged(): Unit =
      if firstSinceDraw() then onEventThread(() => render())

    def drawStarted(): Unit = askedAt.set(NothingAsked)

  def forLayer(layer: SkiaLayer): LayerRedraw =
    over(() => layer.repaint(), () => layer.needRender(true), task => SwingUtilities.invokeLater(() => task()))
