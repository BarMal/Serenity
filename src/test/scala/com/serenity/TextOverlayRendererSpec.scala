package com.serenity

import java.awt.Font

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.ui.layout.{CellMetrics, LayoutRect}
import com.serenity.ui.renderer.{OverlayRow, OverlayRowLayout, OverlaySegment, TextOverlayRenderer, TextOverlayView}
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
