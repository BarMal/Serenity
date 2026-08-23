package com.serenity

import java.awt.Font

import cats.effect.IO
import com.serenity.config.{AppConfig, TextAreaInsets}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.CursorViewport
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class LineWrapMarginReproSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val codeFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont = Font(Font.SANS_SERIF, Font.PLAIN, 12)

  private def renderAndInspect(
    insets: TextAreaInsets,
    usesTextFont: Boolean,
    viewportSize: ViewportSize
  ): (List[(Int, List[MockRenderSurface#DrawRunPxCall])], Float, List[MockRenderSurface#FillPixelRectCall], String) =
    val paneId      = PaneId(0)
    val bufferId    = BufferId(1)
    val cellMetrics = CellMetrics.fromFont(codeFont)
    val content     = "abcdefghij" * 9

    // A Prose-role buffer uses the text (wider) font; a Code-role buffer uses the code font.
    val baseBuffer = Buffer.fromString(bufferId, content)
    val buffer =
      (if usesTextFont then baseBuffer
       else baseBuffer.copy(language = Some(LanguageId.Scala)))
        .copy(cursors = List(CursorPosition(0, content.length)))

    buffer.usesTextFont shouldBe usesTextFont

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
        .withTextAreaInsets(insets)
    )

    val contentRect =
      LayoutEngine
        .calculateEditorPaneLayouts(state, LayoutEngine.calculateLayout(state, viewportSize))(paneId)
        .contentRect
    val panelWidthPx = (contentRect.width * cellMetrics.charWidth).toFloat
    val surface      = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = true, surface, viewportSize, codeFont, textFont, cellMetrics, None)

    val contentRuns =
      surface.drawRunPxCalls.filter(call => call.s.nonEmpty && call.s.forall(char => char >= 'a' && char <= 'j'))
    val contentRows = contentRuns.groupBy(_.yPx).toList.sortBy(_._1)
    val cursorRects = surface.fillPixelRectCalls.filter(_.color == Theme.light.cursorColor)
    (contentRows, panelWidthPx, cursorRects, contentRuns.map(_.s).mkString)

  private def rowWidthPx(runs: List[MockRenderSurface#DrawRunPxCall], usesTextFont: Boolean): Float =
    val measureFont = if usesTextFont then textFont else codeFont
    TextLayoutSnapshot.caretXsForText(runs.map(_.s).mkString, measureFont).lastOption.getOrElse(0.0f)

  "Renderer.render with a large horizontal margin (text font)" should
    "keep every wrapped row within the margin-adjusted content width" in {
      val viewportSize                             = ViewportSize(80, 24)
      val largeMargin                              = TextAreaInsets(left = 0.35, right = 0.35)
      val (rows, panelWidthPx, cursorRects, drawn) = renderAndInspect(largeMargin, usesTextFont = true, viewportSize)

      info(s"panelWidthPx=$panelWidthPx rows=${rows.length}")
      rows.foreach { (y, runs) =>
        info(s"row y=$y widthPx=${rowWidthPx(runs, usesTextFont = true)} text=${runs.map(_.s).mkString}")
      }

      drawn shouldBe "abcdefghij" * 9
      rows.length should be >= 2
      rows.foreach((_, runs) => rowWidthPx(runs, usesTextFont = true) should be <= panelWidthPx)
      cursorRects should have size 1
      cursorRects.head.yPx shouldBe rows.last._1
    }

  "Renderer.render at default margin (text font)" should
    "keep every wrapped row within the content width" in {
      val viewportSize = ViewportSize(50, 14)
      val (rows, panelWidthPx, cursorRects, drawn) =
        renderAndInspect(TextAreaInsets(), usesTextFont = true, viewportSize)

      info(s"panelWidthPx=$panelWidthPx rows=${rows.length}")
      drawn shouldBe "abcdefghij" * 9
      rows.length should be >= 2
      rows.foreach((_, runs) => rowWidthPx(runs, usesTextFont = true) should be <= panelWidthPx)
      cursorRects should have size 1
      cursorRects.head.yPx shouldBe rows.last._1
    }

  "Renderer.render with a large horizontal margin (code font)" should
    "keep every wrapped row within the content width" in {
      val viewportSize                             = ViewportSize(80, 24)
      val largeMargin                              = TextAreaInsets(left = 0.35, right = 0.35)
      val (rows, panelWidthPx, cursorRects, drawn) = renderAndInspect(largeMargin, usesTextFont = false, viewportSize)

      info(s"panelWidthPx=$panelWidthPx rows=${rows.length}")
      drawn shouldBe "abcdefghij" * 9
      rows.length should be >= 2
      rows.foreach((_, runs) => rowWidthPx(runs, usesTextFont = false) should be <= panelWidthPx)
      cursorRects should have size 1
      cursorRects.head.yPx shouldBe rows.last._1
    }

  "TextLayoutSnapshot.gridWrapWidthPx" should "size the wrap width from the code font, not the buffer font" in {
    val config    = AppConfig.default.fontConfig
    val columns   = 40
    val codeWidth = CellMetrics.fromFont(FontLoader.previewFontForRole(config, TypographyRole.Code)).charWidth
    TextLayoutSnapshot.gridWrapWidthPx(columns, config) shouldBe columns * codeWidth
  }

  // The scroll bug: CursorViewport.adjustForCursor decided which visual row holds the cursor (and hence
  // topVisualLine / scroll offset) using visibleColumns * bufferFont('M' width). For a text-font buffer that width
  // is wider than the width the renderer wraps at (the code-font grid width), so the adjuster under-counted wrap
  // rows and scrolled the caret's true visual row out of the rendered viewport. It must scroll to the row measured
  // at the same grid width the renderer uses.
  "CursorViewport.adjustForCursor" should
    "scroll to the caret's wrapped row measured at the grid wrap width (text font)" in {
      val viewportSize = ViewportSize(80, 24)
      val largeMargin  = TextAreaInsets(left = 0.35, right = 0.35)
      val paneId       = PaneId(0)
      val bufferId     = BufferId(1)
      val content      = "abcdefghij" * 60

      val buffer = Buffer
        .fromString(bufferId, content)
        .copy(cursors = List(CursorPosition(0, content.length)))
      buffer.usesTextFont shouldBe true

      val baseState = AppState.initial.copy(
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
          .withTextAreaInsets(largeMargin),
        viewportSize = Some(viewportSize)
      )
      // Give the buffer the viewport dimensions the layout engine derives (visibleColumns/visibleLines in cells).
      val state        = LayoutEngine.syncViewportDimensions(baseState, viewportSize)
      val syncedBuffer = state.buffers(bufferId)
      val cursor       = syncedBuffer.cursors.head

      // The caret's true wrapped row: measured at the width the renderer wraps at (the code-font grid width), with
      // the buffer's own prose font for glyph advances.
      val proseFont = FontLoader.previewFontForRole(state.config.fontConfig, syncedBuffer.typographyRole)
      val gridWrapWidthPx =
        TextLayoutSnapshot.gridWrapWidthPx(syncedBuffer.viewport.visibleColumns, state.config.fontConfig)
      val trueVisualRow =
        TextLayoutSnapshot.visualLineIndexForCursor(content, cursor.column, gridWrapWidthPx, proseFont)

      val adjusted = CursorViewport.adjustForCursor(syncedBuffer, state, cursor)

      info(s"gridWrapWidthPx=$gridWrapWidthPx trueVisualRow=$trueVisualRow topVisualLine=${adjusted.topVisualLine}")

      // adjustForCursor centres the caret's visual row: topVisualLine = max(0, cursorVisualRow - visibleLines / 2).
      // Measuring that row at the buffer font's wider width (the bug) yields a different, too-small row here.
      adjusted.topVisualLine shouldBe math.max(0, trueVisualRow - syncedBuffer.viewport.visibleLines / 2)
    }
