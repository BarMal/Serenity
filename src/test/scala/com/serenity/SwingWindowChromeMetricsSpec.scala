package com.serenity

import java.awt.Dimension

import com.serenity.config.WindowChromeMode
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SwingWindowChromeMetricsSpec extends AnyFlatSpec with Matchers:

  "SwingWindow.ChromeMetrics" should "scale title chrome from the current UI metrics" in {
    val base   = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13))
    val scaled = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 16, lineHeight = 32, ascent = 26))

    scaled.titleBarHeight shouldBe base.titleBarHeight * 2
    scaled.buttonWidth shouldBe base.buttonWidth * 2
    scaled.margin shouldBe base.margin * 2
    scaled.cornerArc shouldBe base.cornerArc * 2
    scaled.titleFontSize shouldBe base.titleFontSize * 2
  }

  it should "derive viewport size from the live canvas size when available" in {
    val metrics        = CellMetrics(charWidth = 10, lineHeight = 20, ascent = 15)
    val canvasSize     = new Dimension(640, 480)
    val requestedFrame = new Dimension(1200, 900)

    val snapshot = SwingWindow.canvasResizeSnapshot(metrics, canvasSize, requestedFrame)

    snapshot.pixelSize shouldBe new Dimension(640, 480)
    snapshot.viewportSize.width shouldBe 64
    snapshot.viewportSize.height shouldBe 24
  }

  it should "fall back to the requested window size before the canvas has been laid out" in {
    val metrics        = CellMetrics(charWidth = 10, lineHeight = 20, ascent = 15)
    val canvasSize     = new Dimension(0, 0)
    val requestedFrame = new Dimension(1200, 900)

    val snapshot = SwingWindow.canvasResizeSnapshot(metrics, canvasSize, requestedFrame)

    snapshot.pixelSize shouldBe new Dimension(1200, 900)
    snapshot.viewportSize.width shouldBe 120
    snapshot.viewportSize.height shouldBe 45
  }

  it should "subtract custom title chrome from fallback canvas height" in {
    val requestedWindow = new Dimension(1200, 900)
    val chrome = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 10, lineHeight = 20, ascent = 15))

    val fallback = SwingWindow.canvasFallbackSize(requestedWindow, WindowChromeMode.Custom, chrome)

    fallback.width shouldBe requestedWindow.width
    fallback.height shouldBe requestedWindow.height - chrome.titleBarHeight
  }

  it should "use the full fallback canvas size for native chrome" in {
    val requestedWindow = new Dimension(1200, 900)
    val chrome = SwingWindow.ChromeMetrics.fromCellMetrics(CellMetrics(charWidth = 10, lineHeight = 20, ascent = 15))

    val fallback = SwingWindow.canvasFallbackSize(requestedWindow, WindowChromeMode.Native, chrome)

    fallback shouldBe requestedWindow
  }

  "SwingWindow.ChromePalette" should "derive custom chrome colours from the active theme" in {
    val palette = SwingWindow.ChromePalette.fromTheme(Theme.light)

    palette.titleBackground shouldBe Theme.light.panel.background
    palette.titleForeground shouldBe Theme.light.panel.foreground
    palette.border shouldBe Theme.light.border
    palette.closeHoverBackground shouldBe Theme.light.error.foreground
  }

  it should "derive distinct custom chrome colours for dark and light themes" in {
    val darkPalette  = SwingWindow.ChromePalette.fromTheme(Theme.dark)
    val lightPalette = SwingWindow.ChromePalette.fromTheme(Theme.light)

    darkPalette.titleBackground should not be lightPalette.titleBackground
    darkPalette.titleForeground should not be lightPalette.titleForeground
    darkPalette.buttonHoverBackground should not be lightPalette.buttonHoverBackground
  }

end SwingWindowChromeMetricsSpec
