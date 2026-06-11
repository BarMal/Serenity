package com.serenity

import java.awt.Font

import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
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
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
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

    val renderedRows = rows(surface)
    surface.drawImageCalls should have size 1
    renderedRows.exists(_.contains("# Raw")) shouldBe true
    renderedRows.exists(_.contains("continued")) shouldBe false
  }

  it should "render markdown preview as the inline lens base when the cursor is outside the viewport" in {
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
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
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

    surface.drawImageCalls should have size 1
    rows(surface).exists(_.contains("# One")) shouldBe false
    rows(surface).exists(_.contains("# Two")) shouldBe false
  }

  it should "render every active cursor markdown block as raw source" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# First\nfirst body\n\n# Second\nsecond body\n\n# Third")
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(0, 0), CursorPosition(3, 0)),
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
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
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

    val renderedRows = rows(surface)
    surface.drawImageCalls should have size 1
    renderedRows.exists(_.contains("# First")) shouldBe true
    renderedRows.exists(_.contains("first body")) shouldBe false
    renderedRows.exists(_.contains("# Second")) shouldBe true
    renderedRows.exists(_.contains("second body")) shouldBe false
    renderedRows.exists(_.contains("# Third")) shouldBe false
  }

  it should "not pull an adjacent heading into a raw paragraph lens when there is no blank line" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# Title\nParagraph immediately after the heading")
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(1, 0)),
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
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
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

    val renderedRows = rows(surface)
    surface.drawImageCalls should have size 1
    renderedRows.exists(_.contains("Paragraph immediately after the heading")) shouldBe true
  }

  it should "draw a visible border around the raw source lens" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# Preview\n\nRaw paragraph\ncontinued")
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
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
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

    surface.strokeRoundRectCalls should not be empty
    val border = surface.strokeRoundRectCalls.head
    border.color shouldBe state.theme.border
    val paneRect =
      LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(paneId)
    border.w shouldBe paneRect.width * CellMetrics.fromFont(font).charWidth
    border.h shouldBe 2 * CellMetrics.fromFont(font).lineHeight
  }

  private def rows(surface: MockRenderSurface): List[String] =
    (0 until surface.height).map(surface.getRow).map(_.trim).filter(_.nonEmpty).toList
