package com.serenity

import java.awt.Font

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.ui.layout.{CellMetrics, LayoutRect}
import com.serenity.ui.renderer.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1105: `TextOverlayRenderer`'s measured row-text and cursor paths must degrade to the existing cell fallback (plain
  * `putString`/inverse-video character) on a surface reporting no `FontRenderContext`, rather than falling through to
  * `drawRunPx`'s no-op.
  */
class TextOverlayRendererCellFallbackSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "TextOverlayRenderer" should "render a proportional-font overlay row via putString when the surface has no FontRenderContext" in {
    val surface = new MockRenderSurface(80, 8, fontRenderContextOverride = None)
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

    surface.drawRunPxCalls shouldBe empty
    surface.getRow(1).slice(1, 1 + rowText.length) shouldBe rowText
  }

  it should "render the overlay row cursor as an inverse-video cell character when the surface has no FontRenderContext" in {
    val surface = new MockRenderSurface(80, 8, fontRenderContextOverride = None)
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

    surface.fillPixelRectCalls shouldBe empty
    val cursorX = 1 + rowText.length
    surface.getBg(cursorX, 1) shouldBe Theme.light.cursor
  }

  it should "still use drawRunPx for a proportional-font overlay row when the surface does report a FontRenderContext (GUI mode unchanged)" in {
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

    surface.drawRunPxCalls.exists(_.s == rowText) shouldBe true
    surface.fillPixelRectCalls should not be empty
  }
