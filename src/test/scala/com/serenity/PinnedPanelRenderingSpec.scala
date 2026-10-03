package com.serenity

import java.awt.{Color, Font}

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val cellMetrics = CellMetrics.fromFont(new Font(Font.MONOSPACED, Font.PLAIN, 12))

  "PinnedPanelRenderer" should "use semantic panel colors rather than hard-coded ANSI backgrounds" in {
    val surface = new MockRenderSurface(40, 12)
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      rows = List(TextPanelRow("Item 1"), TextPanelRow("Item 2"))
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default, cellMetrics)

    surface.getBg(panel.rect.x + 1, panel.rect.y + 1) shouldBe Theme.light.panel.background
    surface.strokeRectCalls.map(_.color) should contain(Theme.light.border)
  }

  it should "use the configured UI outline thickness for pinned panel borders" in {
    val surface = new MockRenderSurface(40, 12)
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      rows = List(TextPanelRow("Item 1"))
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default.withUiOutlineThicknessPx(4), cellMetrics)

    surface.strokeRectCalls.headOption.map(_.strokeWidth) shouldBe Some(4.0f)
  }

  it should "render selected rows using the theme highlight colors" in {
    val surface = new MockRenderSurface(40, 12)
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "explorer",
      rows = List(
        TextPanelRow("repo"),
        TextPanelRow("  src", selected = true)
      )
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default, cellMetrics)

    surface.getBg(panel.rect.x + 1, panel.rect.y + 2) shouldBe Theme.light.highlighted.background
    surface.getFg(panel.rect.x + 1, panel.rect.y + 2) shouldBe Theme.light.highlighted.foreground
    surface.getBg(panel.rect.x + 1, panel.rect.y + 1) shouldBe Theme.light.panel.background
  }

  it should "paint a box's tone as its foreground, unless the box is selected" in {
    val surface = new MockRenderSurface(40, 12)
    val rect    = LayoutRect(2, 2, 20, 6)
    val bounds  = LogicalPixelRect(3, 3, 18, 4)
    def box(text: String, row: Int, tone: OverlayTone, selected: Boolean = false) =
      SurfacePaintBox(
        SurfacePaintKind.Text,
        bounds.copy(y = bounds.y + row, height = 1),
        Some(text),
        selected = selected,
        tone = tone
      )
    val composition = ResolvedSurfaceComposition(
      bounds = bounds,
      intrinsicSize = SurfaceIntrinsicSize(bounds.width, bounds.height),
      paintBoxes = List(
        box("failed", 0, OverlayTone.Error),
        box("primary", 1, OverlayTone.Accent),
        box("picked", 2, OverlayTone.Accent, selected = true),
        box("quiet", 3, OverlayTone.Muted)
      ),
      hitRegions = Nil,
      focusOrder = Nil
    )
    val theme = Theme.light
    val panel = TextPanelView(rect = rect, title = "panel", composition = Some(composition))

    PinnedPanelRenderer.render(surface, panel, theme, AppConfig.default, cellMetrics)

    surface.getFg(3, 3) shouldBe theme.error.foreground
    surface.getBg(3, 3) shouldBe theme.error.background
    surface.getFg(3, 4) shouldBe theme.accent
    surface.getFg(3, 5) shouldBe theme.highlighted.foreground
    surface.getBg(3, 5) shouldBe theme.highlighted.background
    surface.getFg(3, 6) shouldBe theme.muted
  }

  it should "render rows inside the shared framed content rectangle" in {
    val surface = new MockRenderSurface(20, 8)
    val panel = TextPanelView(
      rect = LayoutRect(2, 1, 8, 5),
      title = "outline",
      rows = List(TextPanelRow("abcdefghi"))
    )
    val contentRect = SurfaceFrameLayout(panel.rect).contentRect

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default, cellMetrics)

    surface.getRow(contentRect.y).slice(contentRect.x, contentRect.right) shouldBe "abcdef"
    surface.getRow(contentRect.y)(panel.rect.x) should not be 'a'
    surface.getRow(contentRect.y).lift(contentRect.right).getOrElse(' ') should not be 'g'
  }

  it should "render header, item, and footer rows through the shared content row slots" in {
    val surface = new MockRenderSurface(30, 12)
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 16, 7),
      title = "workflow",
      rows = List(TextPanelRow("body")),
      header = Some(TextPanelRow("head")),
      footer = Some(TextPanelRow("foot"))
    )
    val frameLayout = SurfaceFrameLayout(panel.rect)
    val contentRect = frameLayout.contentRect

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default, cellMetrics)

    surface.getRow(contentRect.y).slice(contentRect.x, contentRect.x + 4) shouldBe "head"
    surface.getRow(contentRect.y + 1).slice(contentRect.x, contentRect.x + 4) shouldBe "body"
    surface.getRow(contentRect.bottom - 1).slice(contentRect.x, contentRect.x + 4) shouldBe "foot"
    surface.getRow(contentRect.y + 2).slice(contentRect.x, contentRect.x + 4) shouldBe "    "
  }

  it should "respect an explicit content rectangle for title and row placement" in {
    val surface = new MockRenderSurface(20, 10)
    val panel = TextPanelView(
      rect = LayoutRect(1, 1, 8, 5),
      contentRect = Some(LayoutRect(4, 3, 3, 1)),
      title = "title",
      rows = List(TextPanelRow("abcdef"))
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default, cellMetrics)

    surface.getRow(panel.titleRect.y).slice(panel.titleRect.x, panel.titleRect.right) shouldBe "tit"
    surface.getRow(3).slice(4, 7) shouldBe "abc"
    surface.getRow(2).slice(4, 7) shouldBe "   "
  }

  it should "paint from a composition when present, taking priority over the plain rows" in {
    val surface     = new MockRenderSurface(40, 12)
    val contentRect = SurfaceFrameLayout(LayoutRect(2, 2, 20, 6)).contentRect
    val composition = ResolvedSurfaceComposition(
      bounds = LogicalPixelRect(contentRect.x, contentRect.y, contentRect.width, contentRect.height),
      intrinsicSize = SurfaceIntrinsicSize(contentRect.width, contentRect.height),
      paintBoxes = List(
        SurfacePaintBox(
          kind = SurfacePaintKind.Text,
          rect = LogicalPixelRect(contentRect.x, contentRect.y, contentRect.width, 1),
          text = Some("from composition")
        )
      ),
      hitRegions = Nil,
      focusOrder = Nil
    )
    val panel = TextPanelView(
      rect = LayoutRect(2, 2, 20, 6),
      title = "outline",
      rows = List(TextPanelRow("from rows")),
      composition = Some(composition)
    )

    PinnedPanelRenderer.render(surface, panel, Theme.light, AppConfig.default, cellMetrics)

    surface.getRow(contentRect.y).slice(contentRect.x, contentRect.x + "from composition".length) shouldBe
      "from composition"
  }

  it should "highlight a selected composition row using the theme highlight colors" in {
    val surface     = new MockRenderSurface(40, 12)
    val contentRect = SurfaceFrameLayout(LayoutRect(2, 2, 20, 6)).contentRect
    val composition = ResolvedSurfaceComposition(
      bounds = LogicalPixelRect(contentRect.x, contentRect.y, contentRect.width, contentRect.height),
      intrinsicSize = SurfaceIntrinsicSize(contentRect.width, contentRect.height),
      paintBoxes = List(
        SurfacePaintBox(
          kind = SurfacePaintKind.Text,
          rect = LogicalPixelRect(contentRect.x, contentRect.y, contentRect.width, 1),
          text = Some("plain"),
          selected = false
        ),
        SurfacePaintBox(
          kind = SurfacePaintKind.Text,
          rect = LogicalPixelRect(contentRect.x, contentRect.y + 1, contentRect.width, 1),
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

    surface.getBg(contentRect.x + 1, contentRect.y + 1) shouldBe Theme.light.highlighted.background
    surface.getFg(contentRect.x + 1, contentRect.y + 1) shouldBe Theme.light.highlighted.foreground
    surface.getBg(contentRect.x + 1, contentRect.y) shouldBe Theme.light.panel.background
  }

end PinnedPanelRenderingSpec
