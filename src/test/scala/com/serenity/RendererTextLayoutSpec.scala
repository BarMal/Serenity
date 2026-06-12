package com.serenity

import java.awt.{Color, Font}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, CursorColorConfig}
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
    viewportSize: ViewportSize = ViewportSize(100, 30),
    config: AppConfig = AppConfig.default.withLineNumbers(false).withGutter(false)
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
      config = config
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

  it should "render every active cursor in proportional text layout" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val font     = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val buffer = Buffer
      .fromString(bufferId, "iW")
      .copy(
        language = Some(com.serenity.lsp.config.LanguageId.Markdown),
        cursors = List(CursorPosition(0, 0), CursorPosition(0, 1), CursorPosition(0, 2))
      )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false)
    )
    val surface     = new MockRenderSurface(100, 30)
    val cellMetrics = CellMetrics.fromFont(font)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30), font, font, cellMetrics, None)

    val cursorRects = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor)
    cursorRects should have size 3
    cursorRects.map(_.xPx).distinct should have size 3
  }

  it should "blink only the primary cursor when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val font     = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val buffer = Buffer
      .fromString(bufferId, "iW")
      .copy(
        language = Some(com.serenity.lsp.config.LanguageId.Markdown),
        cursors = List(CursorPosition(0, 0), CursorPosition(0, 1), CursorPosition(0, 2))
      )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false)
    )
    val cellMetrics    = CellMetrics.fromFont(font)
    val visibleSurface = new MockRenderSurface(100, 30)
    val hiddenSurface  = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, visibleSurface, ViewportSize(100, 30), font, font, cellMetrics, None)
    Renderer.render(state, cursorVisible = false, hiddenSurface, ViewportSize(100, 30), font, font, cellMetrics, None)

    val visibleCursorRects = visibleSurface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor)
    val hiddenCursorRects  = hiddenSurface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor)

    visibleCursorRects should have size 3
    hiddenCursorRects should have size 2
    hiddenCursorRects.map(_.xPx) shouldBe visibleCursorRects.drop(1).map(_.xPx)
  }

  it should "use configured active and inactive cursor colours for primary and secondary cursors" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val font     = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val activeColor   = new Color(0x33, 0x66, 0xcc)
    val inactiveColor = new Color(0xcc, 0x66, 0x33)
    val buffer = Buffer
      .fromString(bufferId, "iW")
      .copy(
        language = Some(com.serenity.lsp.config.LanguageId.Markdown),
        cursors = List(CursorPosition(0, 0), CursorPosition(0, 1), CursorPosition(0, 2))
      )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withCursorColors(CursorColorConfig(active = Some(activeColor), inactive = Some(inactiveColor)))
    )
    val surface     = new MockRenderSurface(100, 30)
    val cellMetrics = CellMetrics.fromFont(font)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30), font, font, cellMetrics, None)

    surface.fillPixelRectCalls.count(_.color == activeColor) shouldBe 1
    surface.fillPixelRectCalls.count(_.color == inactiveColor) shouldBe 2
  }

  it should "hide the only cursor during the hidden blink phase" in {
    val font     = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val surface  = renderState("iW", CursorPosition(0, 1), font)
    val blinkOff = renderState("iW", CursorPosition(0, 1), font)

    surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor) should have size 1
    blinkOff.clear()
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "iW")
      .copy(
        language = Some(com.serenity.lsp.config.LanguageId.Markdown),
        cursors = List(CursorPosition(0, 1))
      )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false)
    )
    val cellMetrics = CellMetrics.fromFont(font)

    Renderer.render(state, cursorVisible = false, blinkOff, ViewportSize(100, 30), font, font, cellMetrics, None)

    blinkOff.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor) shouldBe empty
  }

  it should "render from the viewport left column when horizontally scrolled" in {
    val font     = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val viewport = Viewport(topLine = 0, leftColumn = 2, visibleColumns = 10, visibleLines = 4)
    val surface  = renderState("abcdef", CursorPosition(0, 6), font, viewport)
    val rowX     = firstNonSpaceColumn(surface, 1)

    rowX should be >= 0
    surface.getChar(rowX, 1) shouldBe 'c'
  }
