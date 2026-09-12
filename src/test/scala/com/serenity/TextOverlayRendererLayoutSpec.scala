package com.serenity

import java.awt.{Color, Font}

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, LayoutRect, OverlayRow, OverlayRowLayout, OverlaySegment, SurfaceContentRowKind}
import com.serenity.ui.renderer.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextOverlayRendererLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "TextOverlayRenderer" should "highlight selected segments in plain breadcrumb rows" in {
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

  it should "render content from an explicit overlay content rect when one is provided" in {
    val surface = new MockRenderSurface(20, 8)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 10, 6),
      contentRect = Some(LayoutRect(3, 2, 4, 2)),
      rows = List(OverlayRow(plainText = "ABCD"))
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    surface.getRow(2).slice(3, 7) shouldBe "ABCD"
    surface.getRow(1) should not include "ABCD"
    surface.getRow(2).slice(1, 3) shouldBe "  "
  }

  it should "derive row slots from an explicit overlay content rect" in {
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 10, 6),
      contentRect = Some(LayoutRect(3, 2, 4, 4)),
      header = Some(OverlayRow("head")),
      rows = List(OverlayRow("A"), OverlayRow("B"), OverlayRow("C")),
      footer = Some(OverlayRow("foot"))
    )

    overlay.contentRowSlots
      .map(slot => slot.kind -> slot.y)
      .shouldBe(
        List(
          SurfaceContentRowKind.Header  -> 2,
          SurfaceContentRowKind.Item(0) -> 3,
          SurfaceContentRowKind.Item(1) -> 4,
          SurfaceContentRowKind.Footer  -> 5
        )
      )
  }

  it should "keep narrow column rows inside their assigned content rect" in {
    val surface = new MockRenderSurface(20, 8)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 12, 5),
      contentRect = Some(LayoutRect(3, 2, 4, 1)),
      rows = List(
        OverlayRow(
          plainText = "Label Hint Value",
          segments = List(OverlaySegment("Label"), OverlaySegment("Hint"), OverlaySegment("Value")),
          layout = OverlayRowLayout.Columns
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    surface.getRow(2).slice(3, 7) shouldBe "L  V"
    surface.getRow(2).charAt(7) shouldBe ' '
  }

  it should "keep narrow two-column rows inside their assigned content rect" in {
    val surface = new MockRenderSurface(20, 8)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 12, 5),
      contentRect = Some(LayoutRect(3, 2, 3, 1)),
      rows = List(
        OverlayRow(
          plainText = "Label Hint",
          segments = List(OverlaySegment("Label"), OverlaySegment("Hint")),
          layout = OverlayRowLayout.Columns
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    surface.getRow(2).slice(3, 6) shouldBe "L H"
    surface.getRow(2).charAt(6) shouldBe ' '
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

  it should "render a persistent key-hint row above the footer, distinct from both it and the item rows (issue #931, Stage 3)" in {
    val surface = new MockRenderSurface(80, 9)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 30, 8),
      header = Some(OverlayRow("header")),
      rows = List(OverlayRow("one item")),
      keyHintRow = Some(OverlayRow("Esc dismiss")),
      footer = Some(OverlayRow("1/1"))
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    surface.getRow(1) should include("header")
    surface.getRow(2) should include("one item")
    surface.getRow(5) should include("Esc dismiss")
    surface.getRow(6) should include("1/1")
    surface.getRow(5) should not include "1/1"
    surface.getRow(6) should not include "Esc dismiss"
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

  it should "render an inline icon run with its icon font before the label" in {
    val surface        = new MockRenderSurface(40, 6)
    val font           = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics        = CellMetrics.fromFont(font)
    val iconFontFamily = FontLoader.toolbarIconFontFamily.getOrElse(fail("Expected bundled toolbar icon font"))
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 30, 5),
      rows = List(
        OverlayRow(
          plainText = "Bold",
          segments = List(
            OverlaySegment(
              "Bold",
              inlineIcon = Some("\ue238"),
              inlineIconFontFamily = Some(iconFontFamily)
            )
          ),
          layout = OverlayRowLayout.Distributed
        )
      )
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    surface.putStringCalls.map(_.s) should contain("\ue238")
    surface.putStringCalls.map(_.s) should contain("Bold")
    surface.setFontCalls.map(_.getFamily) should contain(iconFontFamily)
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

  it should "override the cursor info bar's background alpha when configured" in {
    val surface = new MockRenderSurface(24, 6)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val config  = AppConfig.default.withCursorInfoBarBackgroundAlpha(Some(0.5))
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 18, 4),
      rows = List(OverlayRow(plainText = "12:1")),
      surfaceId = Some(com.serenity.state.models.UiSurface.CursorInfoBarSurfaceId)
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, config, cursorVisible = false, font, metrics)

    val bg = surface.getBg(1, 1)
    bg.getRed shouldBe Theme.light.panel.background.getRed
    bg.getGreen shouldBe Theme.light.panel.background.getGreen
    bg.getBlue shouldBe Theme.light.panel.background.getBlue
    bg.getAlpha shouldBe 128
  }

  it should "keep the theme's own panel background alpha for the cursor info bar when no override is configured" in {
    val surface = new MockRenderSurface(24, 6)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 18, 4),
      rows = List(OverlayRow(plainText = "12:1")),
      surfaceId = Some(com.serenity.state.models.UiSurface.CursorInfoBarSurfaceId)
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    surface.getBg(1, 1) shouldBe Theme.light.panel.background
  }

  it should "leave every other panel's background alpha unaffected by a cursor-info-bar alpha override" in {
    val surface = new MockRenderSurface(24, 6)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val config  = AppConfig.default.withCursorInfoBarBackgroundAlpha(Some(0.5))
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 18, 4),
      rows = List(OverlayRow(plainText = "some panel")),
      surfaceId = Some(com.serenity.state.models.SurfaceId("some-other-surface"))
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, config, cursorVisible = false, font, metrics)

    surface.getBg(1, 1) shouldBe Theme.light.panel.background
  }

  // #1295: the cursor info bar had no way to override its own theme colours -- only its background alpha (above).
  it should "override the cursor info bar's foreground and background colours when configured" in {
    val surface    = new MockRenderSurface(24, 6)
    val font       = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics    = CellMetrics.fromFont(font)
    val foreground = new Color(0x11, 0x22, 0x33)
    val background = new Color(0x44, 0x55, 0x66)
    val config = AppConfig.default.withCursorInfoBarColors(
      com.serenity.config.CursorInfoBarColorConfig(Some(foreground), Some(background))
    )
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 18, 4),
      rows = List(OverlayRow(plainText = "12:1")),
      surfaceId = Some(com.serenity.state.models.UiSurface.CursorInfoBarSurfaceId)
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, config, cursorVisible = false, font, metrics)

    surface.getFg(1, 1) shouldBe foreground
    surface.getBg(1, 1) shouldBe background
  }

  it should "keep the theme's own panel colours for the cursor info bar when no colour override is configured" in {
    val surface = new MockRenderSurface(24, 6)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 18, 4),
      rows = List(OverlayRow(plainText = "12:1")),
      surfaceId = Some(com.serenity.state.models.UiSurface.CursorInfoBarSurfaceId)
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    surface.getFg(1, 1) shouldBe Theme.light.panel.foreground
    surface.getBg(1, 1) shouldBe Theme.light.panel.background
  }

  it should "leave every other panel's colours unaffected by a cursor-info-bar colour override" in {
    val surface = new MockRenderSurface(24, 6)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)
    val config = AppConfig.default.withCursorInfoBarColors(
      com.serenity.config.CursorInfoBarColorConfig(Some(new Color(0x11, 0x22, 0x33)), Some(new Color(0x44, 0x55, 0x66)))
    )
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 18, 4),
      rows = List(OverlayRow(plainText = "some panel")),
      surfaceId = Some(com.serenity.state.models.SurfaceId("some-other-surface"))
    )

    TextOverlayRenderer.render(surface, overlay, Theme.light, config, cursorVisible = false, font, metrics)

    surface.getFg(1, 1) shouldBe Theme.light.panel.foreground
    surface.getBg(1, 1) shouldBe Theme.light.panel.background
  }
