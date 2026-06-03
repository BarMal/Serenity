package com.serenity

import java.awt.Font

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.IO
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.effect.unsafe.implicits.global

class RendererTextLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private def renderState(
    content: String,
    cursor: CursorPosition,
    font: Font,
    viewport: Viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 10, visibleLines = 4),
    viewportSize: ViewportSize = ViewportSize(100, 30)
  ): MockRenderSurface =
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, content)
      .copy(
        language = Some(com.serenity.lsp.config.LanguageId.Markdown),
        cursors = List(cursor),
        viewport = viewport
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false)
    )

    val surface     = new MockRenderSurface(viewportSize.width, viewportSize.height)
    val cellMetrics = CellMetrics.fromFont(font)
    Renderer.render(state, cursorVisible = true, surface, viewportSize, font, font, cellMetrics, None)
    surface

  private def firstNonSpaceColumn(surface: MockRenderSurface, row: Int): Int =
    (0 until surface.width).find(x => surface.getChar(x, row) != ' ').getOrElse(-1)

  "Renderer.render" should "place a proportional-text cursor using measured advances rather than raw column count" in {
    val font        = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val cellMetrics = CellMetrics.fromFont(font)
    val surface     = renderState("iW", CursorPosition(0, 1), font)
    val textX       = firstNonSpaceColumn(surface, 1)
    val cursorRects = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor)

    textX should be >= 0
    cursorRects should have size 1
    cursorRects.head.widthPx should be < cellMetrics.charWidth
    cursorRects.head.xPx should be > (textX * cellMetrics.charWidth)
  }

  it should "wrap pane content according to measured text width for proportional text" in {
    val font        = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val cellMetrics = CellMetrics.fromFont(font)
    val viewport    = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 2, visibleLines = 4)
    val surface     = renderState("WWW", CursorPosition(0, 3), font, viewport, viewportSize = ViewportSize(1, 6))
    val row1X       = firstNonSpaceColumn(surface, 1)
    val row2X       = firstNonSpaceColumn(surface, 2)
    val row3X       = firstNonSpaceColumn(surface, 3)

    row1X should be >= 0
    row2X shouldBe row1X
    row3X shouldBe row1X
    surface.getChar(row1X, 1) shouldBe 'W'
    surface.getChar(row2X, 2) shouldBe 'W'
    surface.getChar(row3X, 3) shouldBe 'W'
    cellMetrics.charWidth should be > 0
  }

  it should "render from the viewport left column when horizontally scrolled" in {
    val font     = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val viewport = Viewport(topLine = 0, leftColumn = 2, visibleColumns = 10, visibleLines = 4)
    val surface  = renderState("abcdef", CursorPosition(0, 6), font, viewport)
    val rowX     = firstNonSpaceColumn(surface, 1)

    rowX should be >= 0
    surface.getChar(rowX, 1) shouldBe 'c'
  }
