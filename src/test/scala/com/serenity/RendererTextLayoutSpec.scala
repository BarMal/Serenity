package com.serenity

import java.awt.Font

import cats.effect.IO
import cats.effect.unsafe.implicits.global
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
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class RendererTextLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
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
    val font = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val cellMetrics = CellMetrics.fromFont(font)
    val surface     = renderState("iW", CursorPosition(0, 1), font)
    // Proportional text renders via drawRunPx, not putString.
    val runCalls    = surface.drawRunPxCalls
    val cursorRects = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor)

    // At least one drawRunPx call should exist for the rendered content.
    runCalls should not be empty
    cursorRects should have size 1
    // The cursor caret must be narrower than a full cell width.
    cursorRects.head.widthPx should be < cellMetrics.charWidth
    // The cursor must be positioned after the first character (measured advance > 0).
    cursorRects.head.xPx should be > runCalls.head.xPx.toInt
  }

  it should "render proportional text via drawRunPx with the full content" in {
    val font    = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val surface = renderState("WWW", CursorPosition(0, 0), font)
    // Proportional text must not go through putString.
    surface.putStringCalls.map(_.s).mkString should not include "WWW"
    // All drawRunPx runs together must account for the full text.
    val renderedText = surface.drawRunPxCalls.map(_.s).mkString
    renderedText should include("WWW")
    // Each drawRunPx call is positioned at a non-negative x coordinate.
    surface.drawRunPxCalls.foreach(c => c.xPx should be >= 0.0f)
  }

  it should "render from the viewport left column when horizontally scrolled" in {
    val font     = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val viewport = Viewport(topLine = 0, leftColumn = 2, visibleColumns = 10, visibleLines = 4)
    val surface  = renderState("abcdef", CursorPosition(0, 6), font, viewport)
    val rowX     = firstNonSpaceColumn(surface, 1)

    rowX should be >= 0
    surface.getChar(rowX, 1) shouldBe 'c'
  }
