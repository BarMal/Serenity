package com.serenity

import java.awt.Font

import com.serenity.config.AppConfig
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1105: on a surface reporting no `FontRenderContext` (e.g. a terminal), every measured-text call site must degrade
  * to cell-based `putString` rendering rather than silently vanishing behind a no-op `drawRunPx`.
  */
class RendererCellFallbackSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val propFont     = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val monoMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(80, 24)

  private def buildState(
    content: String,
    language: Option[LanguageId] = None,
    config: AppConfig = AppConfig.default,
    selection: Option[Selection] = None
  ): AppState =
    val paneId     = PaneId(0)
    val bufferId   = BufferId(1)
    val baseBuffer = Buffer.fromString(bufferId, content)
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(language = language),
      editing = baseBuffer.editing.copy(selection = selection)
    )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        theme = Theme.light,
        config = config
      )
    )

  "Renderer" should "render a Markdown buffer (proportional textFont) via putString when the surface has no FontRenderContext" in {
    val state = buildState("hello markdown", language = Some(LanguageId.Markdown))
    val surface =
      new MockRenderSurface(viewportSize.width, viewportSize.height, fontRenderContextOverride = None)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    surface.drawRunPxCalls shouldBe empty
    surface.putStringCalls.exists(_.s.contains("hello markdown")) shouldBe true
  }

  it should "render a plain-text buffer (Prose typography role) via putString when the surface has no FontRenderContext" in {
    val state = buildState("hello prose", language = None)
    val surface =
      new MockRenderSurface(viewportSize.width, viewportSize.height, fontRenderContextOverride = None)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    surface.drawRunPxCalls shouldBe empty
    surface.putStringCalls.exists(_.s.contains("hello prose")) shouldBe true
  }

  it should "still render a code buffer via putString when the surface has no FontRenderContext" in {
    val state = buildState("val x = 1")
    val surface =
      new MockRenderSurface(viewportSize.width, viewportSize.height, fontRenderContextOverride = None)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    surface.drawRunPxCalls shouldBe empty
    surface.putStringCalls.exists(_.s.contains("val x = 1")) shouldBe true
  }

  it should "render proportional selection highlights via putString/cell colors when the surface has no FontRenderContext" in {
    val state = buildState(
      "hello markdown",
      language = Some(LanguageId.Markdown),
      selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 5)))
    )
    val surface =
      new MockRenderSurface(viewportSize.width, viewportSize.height, fontRenderContextOverride = None)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    surface.drawRunPxCalls shouldBe empty
    // "hello" is selected -- its cells should carry the highlighted background/foreground.
    (0 until 5).foreach { col =>
      surface.getBg(col, 0) shouldBe Theme.light.highlighted.background
      surface.getFg(col, 0) shouldBe Theme.light.highlighted.foreground
    }
  }

  it should "still use drawRunPx for a Markdown buffer when the surface does report a FontRenderContext (GUI mode unchanged)" in {
    val state   = buildState("hello markdown", language = Some(LanguageId.Markdown))
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    surface.drawRunPxCalls should not be empty
  }

  it should "render the active pane's header title via putString when the surface has no FontRenderContext" in {
    val state = buildState("hello markdown", language = Some(LanguageId.Markdown))
    val surface =
      new MockRenderSurface(viewportSize.width, viewportSize.height, fontRenderContextOverride = None)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    val titledRows = (0 until viewportSize.height).map(surface.getRow)
    titledRows.exists(_.contains("Buffer 1")) shouldBe true
  }

  it should "render the gutter/status bar text via putString when the surface has no FontRenderContext" in {
    val state = buildState("val x = 1")
    val surface =
      new MockRenderSurface(viewportSize.width, viewportSize.height, fontRenderContextOverride = None)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    val rows = (0 until viewportSize.height).map(surface.getRow)
    rows.exists(_.contains("Line 1")) shouldBe true
  }
