package com.serenity

import java.awt.{Color, Font}
import java.nio.file.Path

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, CursorColorConfig}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
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
    cellMetricsOverride: Option[CellMetrics] = None,
    textFontOverride: Option[Font] = None,
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
    val cellMetrics = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      font,
      textFontOverride.getOrElse(font),
      cellMetrics,
      None
    )
    surface

  private def firstNonSpaceColumn(surface: MockRenderSurface, row: Int): Int =
    (0 until surface.width).find(x => surface.getChar(x, row) != ' ').getOrElse(-1)

  "Renderer.render" should "place a proportional-text cursor using measured advances rather than raw column count" in {
    val font = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val cellMetrics = CellMetrics.fromFont(font)
    val surface     = renderState("iW", CursorPosition(0, 1), font)
    // Proportional text renders via drawRunPx, not putString.
    val runCalls    = surface.drawRunPxCalls.filter(_.s == "iW")
    val cursorRects = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor)

    // At least one drawRunPx call should exist for the rendered content.
    runCalls should not be empty
    cursorRects should have size 1
    // The cursor caret must be narrower than a full cell width.
    cursorRects.head.widthPx should be < cellMetrics.charWidth
    // The cursor must be positioned after the first character (measured advance > 0).
    cursorRects.head.xPx should be > runCalls.head.xPx.toInt
  }

  it should "leave wrapped continuation rows unnumbered after vertical visual scrolling" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma delta epsilon\nnext")
      .copy(
        language = Some(com.serenity.lsp.config.LanguageId.JsonLang),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 8, visibleLines = 4, topVisualLine = 1)
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
      config = AppConfig.default.withGutter(false).withWordWrap(true)
    )
    val viewportSize = ViewportSize(11, 6)
    val surface      = new MockRenderSurface(viewportSize.width, viewportSize.height)
    val font         = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val cellMetrics  = CellMetrics.fromFont(font)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, font, font, cellMetrics, None)

    surface.getRow(1).take(3).trim shouldBe ""
    surface.getRow(2).take(3).trim shouldBe ""
    surface.getRow(3).take(3).trim should not be "1"
  }

  it should "center the active buffer title across the painted header bar" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val title    = "api-spec.json"
    val buffer = Buffer
      .fromString(bufferId, "content")
      .copy(filePath = Some(Path.of(title)))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light
    )
    val viewportSize = ViewportSize(100, 30)
    val surface      = new MockRenderSurface(viewportSize.width, viewportSize.height)
    val font   = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val uiFont = Font(Font.SANS_SERIF, Font.PLAIN, font.getSize).deriveFont(font.getSize2D)
    val cellMetrics = CellMetrics.fromFont(font)
    val uiMetrics   = CellMetrics.fromFont(uiFont)
    val layout      = LayoutEngine.calculateLayout(state, viewportSize)
    val headerRect = List(
      Some(layout.leftSpacerRect),
      layout.lineNumberRect,
      Some(layout.editorPanelRect),
      Some(layout.rightSpacerRect)
    ).flatten
    val headerLeft  = headerRect.map(_.x).min
    val headerRight = headerRect.map(_.right).max
    val expectedXPx =
      TextAlignment
        .placeLine(
          title,
          TextAreaPx(
            xPx = cellMetrics.toPixelX(headerLeft).toFloat,
            yPx = 0,
            widthPx = (headerRight - headerLeft) * cellMetrics.charWidth.toFloat,
            heightPx = cellMetrics.lineHeight
          ),
          uiFont,
          cellMetrics.lineHeight,
          cellMetrics.ascent,
          TextHorizontalAlignment.Center,
          TextVerticalAlignment.Top,
          surface.fontRenderContext.get
        )
        .xPx

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      font,
      font,
      uiFont,
      cellMetrics,
      uiMetrics,
      None
    )

    val titleDraw = surface.drawRunPxCalls.find(_.s == title).getOrElse(fail("Expected measured title draw call"))
    titleDraw.font shouldBe Some(uiFont)
    titleDraw.xPx shouldBe expectedXPx +- 0.001f
    firstNonSpaceColumn(surface, 0) shouldBe math.floor(expectedXPx / uiMetrics.charWidth.toDouble).toInt
  }

  it should "render a measured editor cursor using the full primary row height" in {
    val font = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val textMetrics = CellMetrics.fromFont(font)
    val rowMetrics = CellMetrics(
      charWidth = textMetrics.charWidth,
      lineHeight = textMetrics.lineHeight + 6,
      ascent = textMetrics.ascent + 4
    )
    val surface     = renderState("iW", CursorPosition(0, 1), font, cellMetricsOverride = Some(rowMetrics))
    val cursorRects = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor)

    cursorRects should have size 1
    val contentRun = surface.drawRunPxCalls.find(_.s == "iW").getOrElse(fail("expected measured content draw"))

    cursorRects.head.yPx shouldBe contentRun.yPx
    cursorRects.head.heightPx shouldBe contentRun.lineHeightPx
  }

  it should "size document-font cursors from the buffer typography metrics rather than the code grid" in {
    val codeFont = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val textFont = FontLoader
      .loadTextFont(
        FontConfig(textFontFamily = "Serif", fontSize = 22.0f)
      )
      .unsafeRunSync()
    val codeMetrics = CellMetrics.fromFont(codeFont)
    val surface = renderState(
      "Serenity writes measured prose",
      CursorPosition(0, 8),
      codeFont,
      cellMetricsOverride = Some(codeMetrics),
      textFontOverride = Some(textFont)
    )
    val contentRun =
      surface.drawRunPxCalls.find(_.s.contains("Serenity writes")).getOrElse(fail("expected measured prose draw"))
    val cursorRect = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor).head

    cursorRect.yPx shouldBe contentRun.yPx
    cursorRect.heightPx shouldBe contentRun.lineHeightPx
    cursorRect.heightPx should not be codeMetrics.lineHeight
  }

  it should "baseline-align line numbers with document-font text rows" in {
    val codeFont = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val textFont = FontLoader
      .loadTextFont(
        FontConfig(textFontFamily = "Serif", fontSize = 22.0f)
      )
      .unsafeRunSync()
    val surface = renderState(
      "Alpha\nBeta",
      CursorPosition(0, 0),
      codeFont,
      cellMetricsOverride = Some(CellMetrics.fromFont(codeFont)),
      textFontOverride = Some(textFont),
      config = AppConfig.default.withLineNumbers(true).withGutter(false)
    )

    val firstTextRun =
      surface.drawRunPxCalls.find(_.s == "Alpha").getOrElse(fail("expected measured first prose line"))
    val firstLineNumber =
      surface.drawRunPxCalls.find(call => call.s.trim == "1").getOrElse(fail("expected measured line number"))

    firstLineNumber.yPx shouldBe firstTextRun.yPx
    firstLineNumber.lineHeightPx shouldBe firstTextRun.lineHeightPx
    firstLineNumber.ascentPx shouldBe firstTextRun.ascentPx
    firstLineNumber.font shouldBe firstTextRun.font
    firstLineNumber.font shouldBe Some(textFont)
  }

  it should "keep a measured cursor on the rendered text row below the first content row" in {
    val font       = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val rowMetrics = CellMetrics.fromFont(font)
    val surface =
      renderState("top\nbottom", CursorPosition(1, 1), font, cellMetricsOverride = Some(rowMetrics))
    val cursorRects = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor)
    val bottomRun   = surface.drawRunPxCalls.find(_.s == "bottom").getOrElse(fail("expected bottom row draw"))

    cursorRects should have size 1
    cursorRects.head.yPx shouldBe bottomRun.yPx
    cursorRects.head.heightPx shouldBe bottomRun.lineHeightPx
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

  it should "keep measured editor text, selections, and the end-of-line caret inside pane content" in {
    val paneId     = PaneId(0)
    val bufferId   = BufferId(1)
    val viewport   = ViewportSize(12, 4)
    val codeFont   = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val textFont   = Font(Font.SANS_SERIF, Font.PLAIN, 18)
    val cellMetric = CellMetrics.fromFont(codeFont)
    val text       = "W" * 32
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(
        language = Some(com.serenity.lsp.config.LanguageId.Markdown),
        cursors = List(CursorPosition(0, text.length)),
        selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, text.length)))
      )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    )
    val surface = new MockRenderSurface(viewport.width, viewport.height)
    val contentRect =
      LayoutEngine.calculateEditorPaneLayouts(state, LayoutEngine.calculateLayout(state, viewport))(paneId).contentRect
    val leftPx  = cellMetric.toPixelX(contentRect.x)
    val rightPx = cellMetric.toPixelX(contentRect.right)

    Renderer.render(state, cursorVisible = true, surface, viewport, codeFont, textFont, cellMetric, None)

    surface.drawRunPxCalls.filter(_.s.contains("W")).foreach { call =>
      call.xPx should be >= leftPx.toFloat
      call.xPx + call.bgWidthPx should be <= rightPx.toFloat
    }
    val caret = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor).last
    caret.xPx should be >= leftPx
    caret.xPx + caret.widthPx should be <= rightPx
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
    val surface = renderState(
      "abcdefgh",
      CursorPosition(0, 6),
      font,
      viewport,
      viewportSize = ViewportSize(5, 5),
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    )
    val rowX = firstNonSpaceColumn(surface, 1)

    rowX should be >= 0
    surface.getChar(rowX, 1) shouldBe 'c'
  }

  it should "render unwrapped rows to the pane width when viewport columns are stale" in {
    val font = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val content =
      "0123456789012345678901234567890123456789012345678901234567890123456789"
    val viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 12, visibleLines = 4)
    val surface = renderState(
      content,
      CursorPosition(0, 65),
      font,
      viewport,
      viewportSize = ViewportSize(80, 10),
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    )

    val renderedRow = surface.getRow(1)

    renderedRow.take(60).trim.length should be >= 60
    surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor).head.xPx should be >=
      (60 * CellMetrics.fromFont(font).charWidth)
  }

  it should "render unwrapped document-font rows to at least the grid pane width" in {
    val codeFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val textFont = Font(Font.SERIF, Font.PLAIN, 24)
    val content =
      "0123456789012345678901234567890123456789012345678901234-target-token"
    val viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 12, visibleLines = 4)
    val surface = renderState(
      content,
      CursorPosition(0, 0),
      codeFont,
      viewport,
      viewportSize = ViewportSize(80, 10),
      cellMetricsOverride = Some(CellMetrics.fromFont(codeFont)),
      textFontOverride = Some(textFont),
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    )
    val renderedText = surface.drawRunPxCalls.map(_.s).mkString

    renderedText should include("target-token")
  }

  it should "render unwrapped narrow document-font rows beyond the grid character budget" in {
    val codeFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val textFont = Font(Font.SERIF, Font.PLAIN, 12)
    val content =
      "0123456789012345678901234567890123456789012345678901234567890123456789" +
        "012345678901234567890123456789-target-token"
    val viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 12, visibleLines = 4)
    val surface = renderState(
      content,
      CursorPosition(0, 0),
      codeFont,
      viewport,
      viewportSize = ViewportSize(100, 10),
      cellMetricsOverride = Some(CellMetrics.fromFont(codeFont)),
      textFontOverride = Some(textFont),
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    )
    val renderedText = surface.drawRunPxCalls.map(_.s).mkString

    renderedText should include("target-token")
  }

  it should "dim text outside the active markdown body when focus mode is enabled" in {
    val font = FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)).unsafeRunSync()
    val surface = renderState(
      "Alpha\nBeta\n\nGamma",
      CursorPosition(0, 0),
      font,
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 6),
      viewportSize = ViewportSize(100, 12),
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withFocusedTextBody(true)
    )

    surface.drawRunPxCalls.find(_.s == "Alpha").map(_.foreground) shouldBe Some(Theme.light.foreground)
    surface.drawRunPxCalls.find(_.s == "Gamma").map(_.foreground) shouldBe Some(Theme.light.muted)
  }

  it should "clamp stale horizontal scroll when the pane is wider than the stored viewport" in {
    val font = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()
    val content =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val viewport = Viewport(topLine = 0, leftColumn = 80, visibleColumns = 12, visibleLines = 4)
    val surface = renderState(
      content,
      CursorPosition(0, 90),
      font,
      viewport,
      viewportSize = ViewportSize(80, 10),
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    )

    val rowX = firstNonSpaceColumn(surface, 1)

    surface.getRow(1).slice(rowX, rowX + 4) shouldBe "LMNO"
  }
