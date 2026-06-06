package com.serenity

import java.awt.Font

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.AppConfig
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererFontIsolationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val textFont    = Font(Font.SANS_SERIF, Font.PLAIN, 14)
  private val uiFont      = Font(Font.SANS_SERIF, Font.BOLD, 16)
  private val cellMetrics = CellMetrics.fromFont(codeFont)

  private val paneId    = PaneId(0)
  private val bufferId  = BufferId(1)
  private val text      = "iiiiii"
  private val cursorCol = 3

  private def stateWithRunnerAndBuffer(language: Option[LanguageId]): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(
        cursors = List(CursorPosition(0, cursorCol)),
        language = language
      )
    val pane   = EditorPane.withBuffer(paneId, bufferId)
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val base = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      )
    )
    val (stateWithId, surfaceId) = base.allocateSurfaceId
    stateWithId.copy(
      uiSurfaces = List(
        UiSurface(
          surfaceId,
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(0, cursorCol)), SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(surfaceId)
    )

  private def stateWithRunnerAndMarkdownBuffer: AppState =
    stateWithRunnerAndBuffer(Some(LanguageId.Markdown))

  private def stateWithRunnerAndPlainTextBuffer: AppState =
    stateWithRunnerAndBuffer(None)

  "Renderer" should "use the buffer's text font for cursor positioning even when the command runner is active" in {
    val state        = stateWithRunnerAndMarkdownBuffer
    val cursorColor  = java.awt.Color.RED
    val surface      = new MockRenderSurface(80, 24)
    val viewportSize = ViewportSize(80, 24)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      codeFont,
      textFont,
      cellMetrics,
      Some(cursorColor)
    )

    val buffer       = state.buffers(bufferId)
    val layout       = LayoutEngine.calculateLayout(state, viewportSize)
    val paneRect     = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect  = LayoutRect(paneRect.x, paneRect.y + 1, paneRect.width, math.max(1, paneRect.height - 1))
    val panelWidthPx = contentRect.width * cellMetrics.charWidth

    val textSnapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx, textFont)
    val textXPx      = textSnapshot.xPxForCursor(CursorPosition(0, cursorCol)).getOrElse(fail("no text caret stop"))
    val expectedXPx  = cellMetrics.toPixelX(contentRect.x) + math.round(textXPx)

    val codeSnapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx, codeFont)
    val codeXPx      = codeSnapshot.xPxForCursor(CursorPosition(0, cursorCol)).getOrElse(fail("no code caret stop"))
    val codeExpected = cellMetrics.toPixelX(contentRect.x) + math.round(codeXPx)

    expectedXPx should not be codeExpected

    val cursorRects = surface.fillPixelRectCalls.filter(_.color == cursorColor)
    cursorRects should not be empty
    cursorRects.head.xPx shouldBe expectedXPx.toInt
  }

  it should "switch the surface font to the buffer's font before rendering editor content" in {
    val state        = stateWithRunnerAndMarkdownBuffer
    val surface      = new MockRenderSurface(80, 24)
    val viewportSize = ViewportSize(80, 24)

    Renderer.render(state, cursorVisible = true, surface, viewportSize, codeFont, textFont, cellMetrics, None)

    surface.setFontCalls should contain(textFont)
  }

  it should "switch the surface font to the UI font before rendering overlays" in {
    val state        = stateWithRunnerAndMarkdownBuffer
    val surface      = new MockRenderSurface(80, 24)
    val viewportSize = ViewportSize(80, 24)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      CellMetrics.fromFont(uiFont),
      None
    )

    surface.setFontCalls should contain(uiFont)
    surface.setFontCalls.last shouldBe uiFont
  }

  it should "use a stable UI font for overlays when the compatibility render overload is used" in {
    val state        = stateWithRunnerAndMarkdownBuffer
    val surface      = new MockRenderSurface(80, 24)
    val viewportSize = ViewportSize(80, 24)

    Renderer.render(state, cursorVisible = true, surface, viewportSize, codeFont, textFont, cellMetrics, None)

    surface.setFontCalls.last.getFamily shouldBe Font.SANS_SERIF
    surface.setFontCalls.last.getFamily should not be codeFont.getFamily
  }

  it should "use the text font for a plain-text buffer even when the command runner is active" in {
    val state        = stateWithRunnerAndPlainTextBuffer
    val cursorColor  = java.awt.Color.RED
    val surface      = new MockRenderSurface(80, 24)
    val viewportSize = ViewportSize(80, 24)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      codeFont,
      textFont,
      cellMetrics,
      Some(cursorColor)
    )

    val buffer       = state.buffers(bufferId)
    val layout       = LayoutEngine.calculateLayout(state, viewportSize)
    val paneRect     = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentRect  = LayoutRect(paneRect.x, paneRect.y + 1, paneRect.width, math.max(1, paneRect.height - 1))
    val panelWidthPx = contentRect.width * cellMetrics.charWidth

    val textSnapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx, textFont)
    val textXPx      = textSnapshot.xPxForCursor(CursorPosition(0, cursorCol)).getOrElse(fail("no text caret stop"))
    val expectedXPx  = cellMetrics.toPixelX(contentRect.x) + math.round(textXPx)

    val codeSnapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx, codeFont)
    val codeXPx      = codeSnapshot.xPxForCursor(CursorPosition(0, cursorCol)).getOrElse(fail("no code caret stop"))
    val codeExpected = cellMetrics.toPixelX(contentRect.x) + math.round(codeXPx)

    expectedXPx should not be codeExpected

    val cursorRects = surface.fillPixelRectCalls.filter(_.color == cursorColor)
    cursorRects should not be empty
    cursorRects.head.xPx shouldBe expectedXPx.toInt
  }
