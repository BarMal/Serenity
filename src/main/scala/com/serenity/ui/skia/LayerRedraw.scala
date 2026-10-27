package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.SwingUtilities

import org.jetbrains.skiko.SkiaLayer

/** How a hosted layer is asked to draw: after a frame was published to it, and after the carets over it changed. */
private[skia] trait LayerRedraw:

  def afterPublish(): Unit

  def afterCaretsChanged(): Unit

private[skia] object LayerRedraw:

  /** A published frame is asked for through `repaint`, which also has the layer check its content scale. A change of
    * carets is asked for through `render`, which Skiko's frame driver takes only on the event thread, so it is handed
    * to `onEventThread`; that skips AWT's repaint queue and the extra paint pass `repaint` goes through. Changes that
    * come before a queued render has run share it.
    */
  def over(repaint: () => Unit, render: () => Unit, onEventThread: (() => Unit) => Unit): LayerRedraw = new LayerRedraw:
    private val queued = new AtomicBoolean(false)

    def afterPublish(): Unit = repaint()

    def afterCaretsChanged(): Unit =
      if queued.compareAndSet(false, true) then
        onEventThread { () =>
          queued.set(false)
          render()
        }

  def forLayer(layer: SkiaLayer): LayerRedraw =
    over(() => layer.repaint(), () => layer.needRender(true), task => SwingUtilities.invokeLater(() => task()))
