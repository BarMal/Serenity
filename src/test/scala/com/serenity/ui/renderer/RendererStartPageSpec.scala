package com.serenity.ui.renderer

import com.serenity.MockRenderSurface
import com.serenity.command.{Command, CommandIntent, SessionIntent}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CalculatedLayout, CellMetrics, LayoutRect, ViewportSize}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `RendererStartPage` (issue #1421): the empty-pane filler, the welcome text, and the
  * full-screen startup page's per-line colouring, selection highlight, and viewport clipping.
  *
  * Every fixture uses a `MockRenderSurface` with `fontRenderContextOverride = None` so the renderer takes the cell-only
  * fallback path (`CharacterRenderer.renderString`) instead of the pixel-measured `TextAlignment` path -- deterministic
  * across JVMs/fonts, and the same path every headless/TUI surface takes for this content.
  */
class RendererStartPageSpec extends AnyFlatSpec with Matchers:

  private val font        = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val cellMetrics = CellMetrics.fromFont(font)
  private val theme       = Theme.dark

  private def contextWith(surface: MockRenderSurface): RenderContext =
    RenderContext(
      surface = surface,
      layout = CalculatedLayout(
        editorPanelRect = LayoutRect(0, 0, surface.width, surface.height),
        leftSpacerRect = LayoutRect(0, 0, 0, 0),
        rightSpacerRect = LayoutRect(0, 0, 0, 0)
      ),
      codeFont = font,
      textFont = font,
      uiFont = font,
      cellMetrics = cellMetrics,
      uiMetrics = cellMetrics
    )

  "renderEmptyPane" should "center the empty-document message within the pane, on the pane's top row" in {
    val surface = new MockRenderSurface(60, 10, fontRenderContextOverride = None)
    val rect    = LayoutRect(0, 0, 60, 1)
    val line    = "Empty document — start typing"

    RendererStartPage.renderEmptyPane(rect, theme, contextWith(surface))

    val expectedX = rect.x + (rect.width - line.length) / 2
    (expectedX until expectedX + line.length).map(surface.getChar(_, 0)).mkString shouldBe line
  }

  it should "draw nothing at all when the pane's row falls entirely outside the surface's viewport" in {
    val surface = new MockRenderSurface(60, 5, fontRenderContextOverride = None)
    // Row 100 is far past the 5-row surface, so `renderAlignedTextLine`'s visibility check must skip drawing.
    val rect = LayoutRect(0, 100, 60, 1)

    RendererStartPage.renderEmptyPane(rect, theme, contextWith(surface))

    (0 until 5).foreach(row => surface.getRow(row).trim shouldBe "")
  }

  "renderWelcomeText" should "render each welcome line on its own row, in order, centered in the pane" in {
    val surface = new MockRenderSurface(60, 10, fontRenderContextOverride = None)
    val rect    = LayoutRect(0, 0, 60, 5)

    RendererStartPage.renderWelcomeText(rect, theme, contextWith(surface))

    val lines = List(
      "Welcome to Serenity!",
      "",
      "Start typing to edit text.",
      "",
      "Press Ctrl+P for command palette"
    )
    lines.zipWithIndex.foreach {
      case (line, row) =>
        val expectedX = rect.x + (rect.width - line.length) / 2
        if line.nonEmpty then
          (expectedX until expectedX + line.length).map(surface.getChar(_, row)).mkString shouldBe line
        else surface.getRow(row).trim shouldBe ""
    }
  }

  private def simplePage(selectedIndex: Int): StartupPage =
    StartupPage(
      title = "Serenity",
      selectedIndex = selectedIndex,
      actions = List(
        StartupAction(
          "one",
          "One",
          Command.typed("startup.one", "One", CommandIntent.Session(SessionIntent.StartupNewSession))
        ),
        StartupAction(
          "two",
          "Two",
          Command.typed("startup.two", "Two", CommandIntent.Session(SessionIntent.StartupNewSession))
        )
      )
    )

  "renderStartPage" should "highlight the selected action with the theme's highlight colours, background fill, and focus style" in {
    val surface      = new MockRenderSurface(80, 24, fontRenderContextOverride = None)
    val page         = simplePage(selectedIndex = 1) // "Two" is selected
    val viewportSize = ViewportSize(80, 24)

    RendererStartPage.renderStartPage(page, surface, viewportSize, theme, font, cellMetrics, cellMetrics)

    val bounds = page
      .actionBounds(viewportSize, cellMetrics, cellMetrics)
      .find(_.index == 1)
      .getOrElse(fail("expected bounds for the selected action"))
    val row = cellMetrics.toRow(bounds.yPx)

    val expectedX = (viewportSize.width - "Two".length) / 2
    (expectedX until expectedX + "Two".length).map(surface.getChar(_, row)).mkString shouldBe "Two"
    surface.getFg(expectedX, row) shouldBe theme.highlighted.foreground

    surface.fillPixelRectCalls.map(_.color) should contain(theme.highlighted.background)
    surface.styleCalls should contain(surface.StyleCall("enable", theme.focusStyle))
    surface.styleCalls should contain(surface.StyleCall("disable", theme.focusStyle))
  }

  it should "colour the title and every unselected option with the theme's plain foreground, and other lines muted" in {
    val surface      = new MockRenderSurface(80, 24, fontRenderContextOverride = None)
    val page         = simplePage(selectedIndex = 1) // "One" (line 3) is an unselected option
    val viewportSize = ViewportSize(80, 24)

    RendererStartPage.renderStartPage(page, surface, viewportSize, theme, font, cellMetrics, cellMetrics)

    // Mirrors renderStartPage's own vertical centering so each render-line index maps to the row it actually lands
    // on: the content (7 lines) is shorter than the 24-row viewport, so startYPx is not 0.
    val lineHeightPx               = cellMetrics.lineHeight
    val totalHeightPx              = page.renderLines.size * lineHeightPx
    val startYPx                   = math.max(0, (viewportSize.height * cellMetrics.lineHeight - totalHeightPx) / 2)
    def rowOf(lineIndex: Int): Int = cellMetrics.toRow(startYPx + lineIndex * lineHeightPx)

    def foregroundOfCenteredLine(row: Int, line: String): java.awt.Color =
      val x = (viewportSize.width - line.length) / 2
      surface.getFg(x, row)

    foregroundOfCenteredLine(rowOf(0), "Serenity") shouldBe theme.foreground           // title (line 0)
    foregroundOfCenteredLine(rowOf(1), "Choose a starting point") shouldBe theme.muted // plain, non-option line
    foregroundOfCenteredLine(rowOf(3), "One") shouldBe theme.foreground                // unselected option
  }

  it should "skip render lines whose computed row falls at or past the caller-given viewport height" in {
    // 7 render lines (title, subtitle, blank, "One", "Two", blank, footer) into a 3-row viewport: centering leaves
    // startYPx at 0 (the content is taller than the viewport), so lines 0-2 land inside rows [0,3) and lines 3-6
    // land at or past row 3 -- outside the viewport this call was asked to paint into, even though the backing
    // surface itself has plenty of spare rows.
    val surface      = new MockRenderSurface(80, 10, fontRenderContextOverride = None)
    val page         = simplePage(selectedIndex = 0)
    val viewportSize = ViewportSize(80, 3)

    RendererStartPage.renderStartPage(page, surface, viewportSize, theme, font, cellMetrics, cellMetrics)

    surface.getRow(0).trim shouldBe "Serenity" // line 0 (title): inside the viewport, must be drawn
    surface.getRow(3).trim shouldBe ""         // line 3 ("One"): at the viewport's height, must be skipped
    surface.getRow(4).trim shouldBe ""         // line 4 ("Two"): past the viewport, must be skipped
  }
