package com.serenity

import java.awt.Font

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
