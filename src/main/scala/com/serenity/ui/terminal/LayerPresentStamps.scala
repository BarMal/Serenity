package com.serenity.ui.terminal

import java.util.concurrent.ConcurrentLinkedQueue

import com.serenity.diagnostics.{FrameTimings, PaintSource}

/** Gives the Skia layer's draws the marks a canvas paint leaves in [[FrameTimings]], so `[FRAME] input-to-paint` and
  * `[LATENCY]` complete for keystrokes the layer shows. A frame is published when the layer is handed its picture; a
  * draw starts in Skiko's render callback and ends with Skiko's draw cycle, after the replay, the flush and the swap
  * (see `PresentPoint`).
  *
  * A draw's end is posted to the event thread, so the next draw can start before it arrives. Each end is therefore
  * paired with the oldest start still waiting, never with the latest one, which would time it from the wrong draw. An
  * end with no start of ours (a cycle already under way when the picture was published) is left out.
  */
final class LayerPresentStamps(timings: FrameTimings):
  import LayerPresentStamps.MaxWaitingStarts

  private val waiting = new ConcurrentLinkedQueue[java.lang.Long]()

  def published(): Unit = timings.framePublished()

  /** A start whose end never comes would otherwise leave every later end paired with a draw that long gone. */
  def drawStarted(): Unit =
    val _ = waiting.add(timings.paintStarted(PaintSource.Layer))
    if waiting.size > MaxWaitingStarts then waiting.poll(): Unit

  def drawFinished(): Unit =
    Option(waiting.poll()).foreach(started => timings.paintFinished(started, PaintSource.Layer))

object LayerPresentStamps:
  private val MaxWaitingStarts = 16
