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

  it should "derive pressed button colours from the active theme" in {
    val palette = SwingWindow.ChromePalette.fromTheme(Theme.light)

    palette.buttonPressedBackground should not be palette.titleBackground
    palette.buttonPressedBackground should not be palette.buttonHoverBackground
    palette.closePressedBackground should not be palette.closeHoverBackground
  }

  it should "derive focused button affordance colours from the active theme" in {
    val palette = SwingWindow.ChromePalette.fromTheme(Theme.light)

    palette.focusBorder shouldBe Theme.light.highlighted.foreground
    palette.focusBorder should not be palette.border
  }

  "SwingWindow.ChromeControlPaint" should "resolve button colours by state and control kind" in {
    val palette = SwingWindow.ChromePalette.fromTheme(Theme.light)

    SwingWindow.ChromeControlPaint.background(
      SwingWindow.ChromeControlKind.Minimize,
      palette,
      SwingWindow.ChromeControlState()
    ) shouldBe palette.titleBackground
    SwingWindow.ChromeControlPaint.background(
      SwingWindow.ChromeControlKind.Minimize,
      palette,
      SwingWindow.ChromeControlState(hovered = true)
    ) shouldBe palette.buttonHoverBackground
    SwingWindow.ChromeControlPaint.background(
      SwingWindow.ChromeControlKind.Minimize,
      palette,
      SwingWindow.ChromeControlState(pressed = true)
    ) shouldBe palette.buttonPressedBackground
    SwingWindow.ChromeControlPaint.background(
      SwingWindow.ChromeControlKind.Close,
      palette,
      SwingWindow.ChromeControlState(hovered = true)
    ) shouldBe palette.closeHoverBackground
    SwingWindow.ChromeControlPaint.focusBorder(palette, SwingWindow.ChromeControlState()) shouldBe None
    SwingWindow.ChromeControlPaint.focusBorder(palette, SwingWindow.ChromeControlState(focused = true)) shouldBe
      Some(palette.focusBorder)
  }

  "SwingWindow.ChromeIconGeometry" should "draw every control icon inside the same centered box" in {
    val icons = List(
      SwingWindow.ChromeControlKind.Minimize,
      SwingWindow.ChromeControlKind.Maximize,
      SwingWindow.ChromeControlKind.Restore,
      SwingWindow.ChromeControlKind.Close
    )

    icons.foreach { kind =>
      val lines = SwingWindow.ChromeIconGeometry.lines(kind, width = 46, height = 32)

      lines should not be empty
      lines.foreach { line =>
        line.x1 should be >= 14
        line.x1 should be <= 32
        line.x2 should be >= 14
        line.x2 should be <= 32
        line.y1 should be >= 7
        line.y1 should be <= 25
        line.y2 should be >= 7
        line.y2 should be <= 25
      }
    }
  }

end SwingWindowChromeMetricsSpec
