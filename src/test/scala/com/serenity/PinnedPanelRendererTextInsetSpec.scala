package com.serenity

import java.awt.Font
import java.io.StringWriter

import com.serenity.config.{AppConfig, InterfaceDensity}
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.*
import com.serenity.ui.theme.Theme
import com.serenity.ui.tui.TerminalRenderSurface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A docked panel's glyphs sit one whole character cell from its border -- the same cramped look
  * `TextOverlayRendererTextInsetSpec` covers for floating surfaces (#1615), now fixed for pinned panels too: the
  * outline, diagnostics, directory-tree and markdown-split-preview panels that `PinnedPanelRenderer` paints. Covers the
  * sub-cell inset itself, and the things it must leave alone: the panel/selection background, and the terminal.
  */
class PinnedPanelRendererTextInsetSpec extends AnyFlatSpec with Matchers:

  private val cellMetrics = CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12))

  private def expectedInsetPx(config: AppConfig): Double =
    SurfaceTextInset.px(config)

  "A pinned panel row" should "inset its glyphs from the surface border by a sub-cell amount" in {
    val surface = new MockRenderSurface(40, 12)
    val config  = AppConfig.default
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      rows = List(TextPanelRow("Item 1"))
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, config, cellMetrics)

    val inset = expectedInsetPx(config)
    inset should be > 0.0
    surface.pixelTranslationCalls.map(_.xPx) should contain(inset)
  }

  it should "inset by less than one cell, so the text stays inside the frame it was laid out for" in {
    expectedInsetPx(AppConfig.default) should be < cellMetrics.charWidth.toDouble
  }

  it should "give a roomier interface a larger inset than a denser one, same as a floating overlay" in {
    val compact  = AppConfig.default.withInterfaceDensity(InterfaceDensity.Compact)
    val spacious = AppConfig.default.withInterfaceDensity(InterfaceDensity.Spacious)

    expectedInsetPx(compact) should be < expectedInsetPx(spacious)
  }

  it should "also inset the panel's title text" in {
    val surface = new MockRenderSurface(40, 12)
    val config  = AppConfig.default
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      rows = Nil
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, config, cellMetrics)

    surface.pixelTranslationCalls.map(_.xPx) should contain(expectedInsetPx(config))
  }

  it should "leave a selected row's highlight background full-bleed, spanning the whole content width" in {
    val surface = new MockRenderSurface(40, 12)
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      rows = List(TextPanelRow("repo"), TextPanelRow("  src", selected = true))
    )
    val contentRect = panel.resolvedContentRect

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default, cellMetrics)

    val leftPx  = cellMetrics.toPixelX(contentRect.x)
    val widthPx = cellMetrics.toPixelX(contentRect.right) - leftPx
    withClue("a highlight fill starting right of the content rect would leave an unpainted notch: ") {
      surface.fillPixelRectCalls.map(_.xPx) should contain(leftPx)
    }
    surface.fillPixelRectCalls
      .find(_.xPx == leftPx)
      .map(_.widthPx) shouldBe Some(widthPx)
    surface.fillPixelRectCalls.map(_.color) should contain(Theme.light.highlighted.background)
  }

  it should "leave a selected composition row's highlight background full-bleed too" in {
    val surface     = new MockRenderSurface(40, 12)
    val contentRect = SurfaceFrameLayout(LayoutRect(2, 2, 20, 6)).contentRect
    val composition = ResolvedSurfaceComposition(
      bounds = LogicalPixelRect(contentRect.x, contentRect.y, contentRect.width, contentRect.height),
      intrinsicSize = SurfaceIntrinsicSize(contentRect.width, contentRect.height),
      paintBoxes = List(
        SurfacePaintBox(
          kind = SurfacePaintKind.Text,
          rect = LogicalPixelRect(contentRect.x, contentRect.y, contentRect.width, 1),
          text = Some("picked"),
          selected = true
        )
      ),
      hitRegions = Nil,
      focusOrder = Nil
    )
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      rows = Nil,
      composition = Some(composition)
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default, cellMetrics)

    val leftPx  = cellMetrics.toPixelX(contentRect.x)
    val widthPx = cellMetrics.toPixelX(contentRect.right) - leftPx
    surface.fillPixelRectCalls.map(_.xPx) should contain(leftPx)
    surface.fillPixelRectCalls
      .find(_.xPx == leftPx)
      .map(_.widthPx) shouldBe Some(widthPx)
  }

  "A pinned panel rendered onto a terminal surface" should "produce identical output regardless of the sub-cell inset" in {
    def render(config: AppConfig): String =
      val writer  = new StringWriter()
      val metrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13)
      val surface = new TerminalRenderSurface(40, 12, writer, metrics)
      val panel = TextPanelView(
        rect = LayoutRect(2, 2, 20, 6),
        title = "outline",
        rows = List(TextPanelRow("repo"), TextPanelRow("  src", selected = true))
      )
      PinnedPanelRenderer.render(surface, panel, Theme.light, config, metrics)
      surface.flush()
      writer.toString

    val compact  = AppConfig.default.withInterfaceDensity(InterfaceDensity.Compact)
    val spacious = AppConfig.default.withInterfaceDensity(InterfaceDensity.Spacious)

    // `SurfaceTextInset.px` differs between these two configs on a GUI surface (see the density test above), so an
    // identical terminal output here proves `withPixelTranslation` genuinely had no effect, rather than the two
    // configs happening to resolve to the same inset.
    expectedInsetPx(compact) should not be expectedInsetPx(spacious)
    render(compact) shouldBe render(spacious)
  }
end PinnedPanelRendererTextInsetSpec
