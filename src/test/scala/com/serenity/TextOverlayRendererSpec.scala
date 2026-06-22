package com.serenity

import java.awt.{Color, Font}

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.ui.layout.{CellMetrics, LayoutRect, TextLayoutSnapshot}
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

  it should "place an editable split row caret after the rendered value segment" in {
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

    val valueX          = 1 + math.max(0, 38 - query.length)
    val caretXs         = TextLayoutSnapshot.caretXsForText(query, font, surface.fontRenderContext.get)
    val expectedCursorX = metrics.toPixelX(valueX) + math.round(caretXs.last)

    surface.fillPixelRectCalls.last.xPx shouldBe expectedCursorX
  }

  it should "place a plain editable row caret using measured text advances" in {
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

    val caretXs         = TextLayoutSnapshot.caretXsForText(rowText, font, surface.fontRenderContext.get)
    val expectedCursorX = metrics.toPixelX(1) + math.round(caretXs.last)

    surface.fillPixelRectCalls.last.xPx shouldBe expectedCursorX
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
    firstRow.indexOf("Monaspace") shouldBe secondRow.indexOf("SansSerif")
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
