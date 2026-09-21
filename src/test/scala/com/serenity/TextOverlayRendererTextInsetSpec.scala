package com.serenity

import java.awt.Font

import com.serenity.config.{AppConfig, InterfaceDensity}
import com.serenity.ui.layout.{CellMetrics, LayoutRect, OverlayRow}
import com.serenity.ui.renderer.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Glyphs inside a floating surface sit one whole character cell from its border, because a cell is the only inset the
  * frame can express. That reads as cramped against the rounded corner the border is drawn with. These cover the
  * sub-cell text inset that fixes it, and the things it must leave alone: the row's own background, and the terminal.
  */
class TextOverlayRendererTextInsetSpec extends AnyFlatSpec with Matchers:

  private val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val metrics = CellMetrics(charWidth = 8, lineHeight = 20, ascent = 15)

  private def expectedInsetPx(config: AppConfig): Int =
    SurfaceTextInset.px(config).toInt

  private def renderRowOverlay(config: AppConfig, surface: MockRenderSurface): Unit =
    val overlay = TextOverlayView(rect = LayoutRect(0, 0, 12, 3), rows = List(OverlayRow("row")))
    TextOverlayRenderer.render(surface, overlay, Theme.light, config, cursorVisible = false, font, metrics)

  "An overlay row" should "inset its glyphs from the surface border by a sub-cell amount" in {
    val surface = new MockRenderSurface(40, 8)
    val config  = AppConfig.default
    renderRowOverlay(config, surface)

    val inset = expectedInsetPx(config)
    inset should be > 0
    surface.pixelTranslationCalls.map(_.xPx) should contain(inset.toDouble)
  }

  it should "inset by less than one cell, so the text stays inside the frame it was laid out for" in {
    val surface = new MockRenderSurface(40, 8)
    val config  = AppConfig.default
    renderRowOverlay(config, surface)

    expectedInsetPx(config) should be < metrics.charWidth
  }

  it should "give a roomier interface a larger inset than a denser one" in {
    val compact  = AppConfig.default.withInterfaceDensity(InterfaceDensity.Compact)
    val spacious = AppConfig.default.withInterfaceDensity(InterfaceDensity.Spacious)

    expectedInsetPx(compact) should be < expectedInsetPx(spacious)
  }

  it should "leave the selected row's background full-bleed, so selection still spans the whole row" in {
    val surface = new MockRenderSurface(40, 8)
    val overlay = TextOverlayView(
      rect = LayoutRect(0, 0, 12, 3),
      rows = List(OverlayRow("row", selected = true)),
      itemGapRows = 1.0
    )
    TextOverlayRenderer.render(surface, overlay, Theme.light, AppConfig.default, cursorVisible = false, font, metrics)

    val contentLeftPx = metrics.toPixelX(1)
    withClue("a selection fill starting right of the content rect would leave an unpainted notch: ") {
      surface.fillPixelRectCalls.map(_.xPx) should contain(contentLeftPx)
    }
  }
