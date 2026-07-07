package com.serenity

import java.awt.{Color, Font}

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.ui.layout.{CellMetrics, LayoutRect}
import com.serenity.ui.renderer.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextOverlayRendererSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "TextOverlayRenderer" should "scroll a long editable split row horizontally to keep the caret visible" in {
    val surface = new MockRenderSurface(20, 8)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val rowText = "Editor Font Size: 1234567890"
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 12, 5),
      rows = List(
        OverlayRow(
          plainText = rowText,
          selected = true,
          cursorColumn = Some(rowText.length),
          segments = List(
            OverlaySegment("Editor Font Size"),
            OverlaySegment("1234567890", selected = true)
          ),
          layout = OverlayRowLayout.Split
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    surface.getRow(1).slice(1, 11) shouldBe "1234567890"
    surface.fillPixelRectCalls should not be empty
    surface.fillPixelRectCalls.last.xPx should be >= metrics.toPixelX(1)
  }

  it should "place an editable split row caret after the inline value segment" in {
    val surface = new MockRenderSurface(80, 8)
    val font    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val query   = "iiiiWWWW"
    val rowText = s"Find $query"
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 40, 5),
      rows = List(
        OverlayRow(
          plainText = rowText,
          selected = true,
          cursorColumn = Some(rowText.length),
          segments = List(
            OverlaySegment("Find"),
            OverlaySegment(query, selected = true)
          ),
          layout = OverlayRowLayout.Split
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    surface.getRow(1).slice(1, 1 + rowText.length) shouldBe rowText

    val valueX  = 1 + "Find ".length
    val caretXs = com.serenity.ui.layout.TextLayoutSnapshot.caretXsForText(query, font, surface.fontRenderContext.get)
    val expectedCursorX = metrics.toPixelX(valueX) + math.round(caretXs.last)

    surface.fillPixelRectCalls.last.xPx shouldBe expectedCursorX
  }

  it should "paint a plain editable row caret using the cursor colour" in {
    val surface = new MockRenderSurface(80, 8)
    val font    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val rowText = "search: iiiiWWWW"
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 40, 5),
      rows = List(
        OverlayRow(
          plainText = rowText,
          cursorColumn = Some(rowText.length)
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    surface.fillPixelRectCalls.last.color shouldBe Theme.light.cursor
    surface.fillPixelRectCalls.last.yPx shouldBe metrics.toPixelY(1)
  }

  it should "highlight selected segments in plain breadcrumb rows" in {
    val surface = new MockRenderSurface(80, 8)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 50, 5),
      header = Some(
        OverlayRow(
          plainText = "UI Presets > Edit Preset: Writing > Fonts",
          segments = List(
            OverlaySegment("UI Presets >", selected = true),
            OverlaySegment("Edit Preset: Writing >", selected = true),
            OverlaySegment("Fonts")
          )
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    surface.getRow(1) should include("UI Presets > Edit Preset: Writing > Fonts")
    surface.getBg(1, 1) shouldBe Theme.light.highlighted.background
    surface.getBg(14, 1) shouldBe Theme.light.highlighted.background
    surface.getBg(38, 1) shouldBe Theme.light.panel.background
  }

  it should "place a plain editable row caret at the measured text end for proportional fonts" in {
    val surface = new MockRenderSurface(80, 8)
    val font    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val rowText = "search: iiiiWWWW"
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 40, 5),
      rows = List(
        OverlayRow(
          plainText = rowText,
          cursorColumn = Some(rowText.length)
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    val caretXs = com.serenity.ui.layout.TextLayoutSnapshot.caretXsForText(rowText, font, surface.fontRenderContext.get)
    val expectedCursorX = metrics.toPixelX(1) + math.round(caretXs.last)

    surface.fillPixelRectCalls.last.xPx shouldBe expectedCursorX
  }

  it should "render editable plain row text and caret with the same measured baseline" in {
    val surface = new MockRenderSurface(80, 8)
    val font    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val rowText = "search: iiiiWWWW"
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 40, 5),
      rows = List(
        OverlayRow(
          plainText = rowText,
          cursorColumn = Some(rowText.length)
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    val textRun = surface.drawRunPxCalls.find(_.s == rowText).getOrElse(fail("expected measured row text"))
    textRun.yPx shouldBe metrics.toPixelY(1)
    textRun.lineHeightPx shouldBe metrics.lineHeight
    textRun.ascentPx shouldBe metrics.ascent
    surface.fillPixelRectCalls.last.yPx shouldBe textRun.yPx
    surface.fillPixelRectCalls.last.heightPx shouldBe textRun.lineHeightPx
  }

  it should "keep measured row text and caret pixel writes inside the content rect" in {
    val surface = new MockRenderSurface(30, 8)
    val font    = Font(Font.SANS_SERIF, Font.PLAIN, 18)
    val metrics = CellMetrics.fromFont(font)
    val rowText = "WWWWWWWWWWWW"
    val overlay = TextOverlayView(
      rect = LayoutRect(3, 1, 8, 5),
      rows = List(
        OverlayRow(
          plainText = rowText,
          cursorColumn = Some(rowText.length)
        )
      )
    )
    val contentRect = com.serenity.ui.layout.SurfaceFrameLayout(overlay.rect).contentRect
    val leftPx      = metrics.toPixelX(contentRect.x)
    val rightPx     = metrics.toPixelX(contentRect.right)

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    val textRun = surface.drawRunPxCalls.find(_.s.nonEmpty).getOrElse(fail("expected measured row text"))
    textRun.xPx should be >= leftPx.toFloat
    textRun.xPx + textRun.bgWidthPx should be <= rightPx.toFloat

    val cursor = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursor).last
    cursor.xPx should be >= leftPx
    cursor.xPx + cursor.widthPx should be <= rightPx
    cursor.yPx should be >= metrics.toPixelY(contentRect.y)
    cursor.yPx + cursor.heightPx should be <= metrics.toPixelY(contentRect.bottom)
  }

  it should "render command runner settings rows with stable label, hint, and value columns" in {
    val surface = new MockRenderSurface(80, 8)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 70, 6),
      rows = List(
        OverlayRow(
          plainText = "Code Font: Used in code buffers Monaspace Neon",
          selected = true,
          segments = List(
            OverlaySegment("Code Font"),
            OverlaySegment("Used in code buffers"),
            OverlaySegment("Monaspace Neon", selected = true)
          ),
          layout = OverlayRowLayout.Columns
        ),
        OverlayRow(
          plainText = "UI Font: Used in the app interface SansSerif",
          segments = List(
            OverlaySegment("UI Font"),
            OverlaySegment("Used in the app interface"),
            OverlaySegment("SansSerif", selected = true)
          ),
          layout = OverlayRowLayout.Columns
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    val firstRow  = surface.getRow(1)
    val secondRow = surface.getRow(2)
    firstRow.indexOf("Used") shouldBe secondRow.indexOf("Used")
    firstRow.indexOf("Monaspace") + "Monaspace Neon".length shouldBe
      secondRow.indexOf("SansSerif") + "SansSerif".length
  }

  it should "right-align selected command option values in the value column" in {
    val surface = new MockRenderSurface(80, 8)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 70, 6),
      rows = List(
        OverlayRow(
          plainText = "Animation Style: None, subtle, or full Full",
          selected = true,
          segments = List(
            OverlaySegment("Animation Style"),
            OverlaySegment("None, subtle, or full"),
            OverlaySegment("Full", selected = true)
          ),
          layout = OverlayRowLayout.Columns
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    val contentWidth = overlay.rect.width - 2
    val valueWidth   = math.min(18, math.max(8, contentWidth / 4))
    val valueEnd     = overlay.rect.x + 1 + contentWidth
    val valueStart   = valueEnd - valueWidth
    val expectedX    = valueStart + valueWidth - "Full".length

    surface.getRow(1).slice(expectedX, expectedX + "Full".length) shouldBe "Full"
  }

  it should "render close workflow action choices when the frame reserves enough content rows" in {
    val surface = new MockRenderSurface(80, 8)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 50, 5),
      header = Some(OverlayRow("unsaved changes")),
      rows = List(
        OverlayRow("Buffer 0 - unsaved"),
        OverlayRow(
          plainText = "Save Close Anyway Cancel",
          segments = List(
            OverlaySegment("Save", selected = true),
            OverlaySegment("Close Anyway"),
            OverlaySegment("Cancel")
          ),
          layout = OverlayRowLayout.Distributed
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    surface.getRow(1) should include("unsaved changes")
    surface.getRow(2) should include("Buffer 0 - unsaved")
    surface.getRow(3) should include("Close Anyway")
  }

  it should "render overlay footers in the footer slot from the shared frame contract" in {
    val surface = new MockRenderSurface(80, 8)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 30, 7),
      header = Some(OverlayRow("header")),
      rows = List(OverlayRow("one item")),
      footer = Some(OverlayRow("1/1"))
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    surface.getRow(1) should include("header")
    surface.getRow(2) should include("one item")
    surface.getRow(5) should include("1/1")
    surface.getRow(3) should not include "1/1"
  }

  it should "truncate long selected column text from the end instead of dropping the leading characters" in {
    val surface = new MockRenderSurface(40, 6)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 24, 5),
      rows = List(
        OverlayRow(
          plainText = "Extremely Long Setting Name: Helpful hint Value",
          selected = true,
          segments = List(
            OverlaySegment("Extremely Long Setting Name"),
            OverlaySegment("Helpful hint"),
            OverlaySegment("Value", selected = true)
          ),
          layout = OverlayRowLayout.Columns
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    val renderedRow = surface.getRow(1)
    renderedRow should include("Extre")
    renderedRow should not include "etting"
  }

  it should "keep editable command setting columns anchored when the value overflows" in {
    val surface = new MockRenderSurface(40, 6)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val rowText = "Motion Speed Scale: Scale (0.0-4.0) 12345678901234567890"
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 24, 5),
      rows = List(
        OverlayRow(
          plainText = rowText,
          selected = true,
          cursorColumn = Some(rowText.length),
          segments = List(
            OverlaySegment("Motion Speed Scale"),
            OverlaySegment("Scale (0.0-4.0)"),
            OverlaySegment("12345678901234567890", selected = true)
          ),
          layout = OverlayRowLayout.Columns
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    val renderedRow = surface.getRow(1)
    renderedRow should include("Motio")
    renderedRow should not include "78901234567890"
  }

  it should "place an editable command setting caret on the rendered value cell" in {
    val surface  = new MockRenderSurface(80, 8)
    val gridFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val font     = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val metrics  = CellMetrics.fromFont(gridFont)
    val value    = "250"
    val rowText  = s"Animation Duration: Duration in ms $value"
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 70, 6),
      rows = List(
        OverlayRow(
          plainText = rowText,
          selected = true,
          cursorColumn = Some(rowText.length),
          segments = List(
            OverlaySegment("Animation Duration"),
            OverlaySegment("Duration in ms"),
            OverlaySegment(value, selected = true)
          ),
          layout = OverlayRowLayout.Columns
        )
      )
    )

    surface.setFont(font)
    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    val contentWidth = overlay.rect.width - 2
    val labelWidth   = math.min(22, math.max(8, contentWidth / 3))
    val valueWidth   = math.min(18, math.max(8, contentWidth / 4))
    val hintWidth    = math.max(0, contentWidth - labelWidth - valueWidth - 2)
    val valueCellX   = overlay.rect.x + 1 + labelWidth + hintWidth + 2
    val valueX       = valueCellX + valueWidth - value.length

    surface.getRow(1).slice(valueX, valueX + value.length) shouldBe value
    val caretXs = com.serenity.ui.layout.TextLayoutSnapshot.caretXsForText(value, font, surface.fontRenderContext.get)
    val rawCaretXPx     = metrics.toPixelX(valueX) + math.round(caretXs.last)
    val contentRightXPx = metrics.toPixelX(com.serenity.ui.layout.SurfaceFrameLayout(overlay.rect).contentRect.right)
    val caret           = surface.fillPixelRectCalls.last
    caret.xPx shouldBe math.min(rawCaretXPx, contentRightXPx - caret.widthPx)
    caret.xPx + caret.widthPx should be <= contentRightXPx
  }

  it should "render font preview segments with the segment font family" in {
    val surface = new MockRenderSurface(60, 6)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 50, 5),
      rows = List(
        OverlayRow(
          plainText = "Serif - Used in prose buffers",
          segments = List(
            OverlaySegment("Serif", fontFamily = Some(Font.SERIF)),
            OverlaySegment("Used in prose buffers")
          ),
          layout = OverlayRowLayout.Columns
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = true, font, metrics)

    surface.setFontCalls.map(_.getFamily) should contain(Font.SERIF)
    surface.setFontCalls.last.getFamily shouldBe font.getFamily
  }

  it should "use theme panel colours for rows without animation overrides" in {
    val surface = new MockRenderSurface(24, 6)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val theme = Theme.light.copy(
      panel = Theme.light.panel.copy(
        foreground = new Color(0x12, 0x34, 0x56),
        background = new Color(0xab, 0xcd, 0xef)
      )
    )
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 18, 4),
      rows = List(OverlayRow(plainText = "plain row"))
    )

    TextOverlayRenderer.render(surface, overlay, theme, AppConfig.default, cursorVisible = false, font, metrics)

    surface.getFg(1, 1) shouldBe theme.panel.foreground
    surface.getBg(1, 1) shouldBe theme.panel.background
  }
