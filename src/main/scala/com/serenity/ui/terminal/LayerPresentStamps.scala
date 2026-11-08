package com.serenity.ui.terminal

import java.util.concurrent.atomic.AtomicLong

import com.serenity.diagnostics.FrameTimings

/** Gives the Skia layer's draws the marks a canvas paint leaves in [[FrameTimings]], so `[FRAME] input-to-paint` and
  * `[LATENCY]` complete for keystrokes the layer shows. A frame is published when the layer is handed its picture; a
  * draw starts in Skiko's render callback and ends with Skiko's draw cycle, after the replay, the flush and the swap
  * (see `PresentPoint`).
  *
  * Draws run one at a time on the event thread, so one start mark is enough.
  */
final class LayerPresentStamps(timings: FrameTimings):

  private val startedAt = new AtomicLong(0L)

  def published(): Unit = timings.framePublished()

  def drawStarted(): Unit = startedAt.set(timings.paintStarted())

  def drawFinished(): Unit = timings.paintFinished(startedAt.get())
