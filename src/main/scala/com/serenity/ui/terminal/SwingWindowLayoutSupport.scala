package com.serenity.ui.terminal

import java.awt.Dimension

import com.serenity.config.WindowChromeMode
import com.serenity.ui.layout.{CellMetrics, ViewportSize}

/** Pure canvas/viewport sizing math for [[SwingWindow]] -- how a pixel resize (window or font-metrics driven) maps to a
  * cell viewport, and when that change is worth publishing. Mixed into that class's companion object so callers keep
  * seeing `SwingWindow.CanvasResizeSnapshot` etc.; split into its own file to keep `SwingWindow.scala` within the
  * architecture ratchet's line target.
  */
private[terminal] trait SwingWindowLayoutSupport:

  final case class CanvasResizeSnapshot(pixelSize: Dimension, viewportSize: ViewportSize)

  private[serenity] def shouldPublishCanvasResize(
    previous: CanvasResizeSnapshot,
    current: CanvasResizeSnapshot
  ): Boolean =
    previous.viewportSize != current.viewportSize

  def canvasResizeSnapshot(
    metrics: CellMetrics,
    canvasSize: Dimension,
    fallbackSize: Dimension
  ): CanvasResizeSnapshot =
    val size =
      if canvasSize.width > 0 && canvasSize.height > 0 then new Dimension(canvasSize)
      else new Dimension(fallbackSize)
    CanvasResizeSnapshot(size, metrics.viewportSize(size.width, size.height))

  /** Recalculate the cell viewport after a font change using the currently laid-out canvas when available. */
  def fontMetricsUpdateSnapshot(
    metrics: CellMetrics,
    canvasSize: Dimension,
    previousCanvasSize: Dimension
  ): CanvasResizeSnapshot =
    canvasResizeSnapshot(metrics, canvasSize, previousCanvasSize)

  def canvasFallbackSize(
    windowSize: Dimension,
    chromeMode: WindowChromeMode,
    chromeMetrics: SwingWindow.ChromeMetrics
  ): Dimension =
    val chromeHeight =
      chromeMode match
        case WindowChromeMode.Custom => chromeMetrics.titleBarHeight
        case WindowChromeMode.Auto | WindowChromeMode.Native | WindowChromeMode.NativeThemed => 0
    new Dimension(windowSize.width.max(1), (windowSize.height - chromeHeight).max(1))

  def fallbackCanvasResizeSnapshot(
    metrics: CellMetrics,
    windowSize: Dimension,
    chromeMode: WindowChromeMode,
    chromeMetrics: SwingWindow.ChromeMetrics
  ): CanvasResizeSnapshot =
    canvasResizeSnapshot(
      metrics,
      new Dimension(0, 0),
      canvasFallbackSize(windowSize, chromeMode, chromeMetrics)
    )
