package com.serenity

import java.awt.Font

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererMarkdownLensSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Renderer markdown lens" should "render the active markdown block as raw source" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# Lens\n\n# Raw\ncontinued")
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(2, 0)),
        viewport = Viewport.default.copy(visibleLines = 10)
      )
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config.withSyntaxHighlighting(true)
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    val boldCalls = surface.styleCalls.filter(call => call.action == "enable" && call.style == TextStyle.bold)

    boldCalls should not be empty
    boldCalls.length shouldBe 2
  }

  it should "render every markdown line with presentation styling when the cursor is outside the viewport" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# One\n# Two")
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(10, 0)),
        viewport = Viewport.default.copy(visibleLines = 10)
      )
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config.withSyntaxHighlighting(true)
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    surface.styleCalls.filter(call => call.action == "enable" && call.style == TextStyle.bold).length shouldBe 4
  }
