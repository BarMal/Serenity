package com.serenity.ui.terminal

import java.awt.Cursor

/** A GPU layer standing in for the canvas's own painting: the base frame lives in the layer, which has no
  * `BufferedImage` for the window to fill carets over, so the window hands the carets and the pointer cursor to it.
  */
trait LayerPresenter:

  def baseAvailable: Boolean

  def showCarets(carets: scala.List[SwingWindow.CaretPaint]): Unit

  def setCursor(cursor: Cursor): Unit

  /** `callback` is called, on the event thread, each time the layer has drawn a frame. */
  def onRendered(callback: () => Unit): Unit

  /** How many frames have been published to the layer, and how many of them its latest draw showed. */
  def publishedFrames: Int = 0

  def shownFrames: Int = 0

  /** The render API the layer ended up with, which is a software one where the GPU's could not start. */
  def renderApi: String

  /** What the layer's draws have cost so far; a presenter that does not measure reports [[LayerDrawStats.None]]. */
  def drawStats: LayerDrawStats = LayerDrawStats.None

  def dispose(): Unit

/** How a layer's draws went: how many, against how many published frames, and the mean milliseconds in each part of a
  * draw. `publishToRender` is the wait from a frame being published to the layer first drawing it, which is where time
  * spent behind other work on the event thread shows.
  */
final case class LayerDrawStats(
    renders: Int,
    publishes: Int,
    publishToRenderMeanMs: Double,
    publishToRenderMaxMs: Double,
    replayMeanMs: Double,
    caretsMeanMs: Double,
    syncMeanMs: Double,
    replays: Int = 0,
    cachedDraws: Int = 0,
    syncedDraws: Int = 0,
    gpuContext: Boolean = false
):

  def replayCacheEngaged: Boolean = cachedDraws > 0

  def replayCacheNote: String =
    if replayCacheEngaged then s"on ($cachedDraws of $renders draws show the cached picture)"
    else if gpuContext then "off (not selected)"
    else "off (no GPU context on the canvas Skiko draws the layer with, which records into a picture)"

  def gpuSyncNote: String =
    if syncedDraws > 0 then s"$syncedDraws of $renders draws"
    else "not done (no GPU context to wait on)"

object LayerDrawStats:

  val None: LayerDrawStats = LayerDrawStats(0, 0, 0.0, 0.0, 0.0, 0.0, 0.0)
