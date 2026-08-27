package com.serenity

import java.awt.Font
import java.awt.font.TextAttribute

import com.serenity.config.AppConfig
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Verifies that a buffer with language=Markdown (triggering textFont which is proportional) renders via drawRunPx,
  * while a code buffer renders via putString.
  */
class RendererProportionalRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val propFont     = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val monoMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(80, 24)

  private def buildState(content: String, language: Option[LanguageId] = None): AppState =
    val paneId     = PaneId(0)
    val bufferId   = BufferId(1)
    val baseBuffer = Buffer.fromString(bufferId, content)
    val buffer     = baseBuffer.copy(document = baseBuffer.document.copy(language = language))
    val pane       = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        theme = Theme.light,
        config = AppConfig.default.withLineNumbers(false).withGutter(false)
      )
    )

  "Renderer" should "use drawRunPx for a Markdown buffer (proportional textFont)" in {
    val state   = buildState("hello markdown", language = Some(LanguageId.Markdown))
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    // codeFont = mono, textFont = proportional — Markdown buffer picks textFont
    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    surface.drawRunPxCalls should not be empty
    surface.putStringCalls.exists(_.s.contains("hello")) shouldBe false
  }

  it should "use drawRunPx for a code buffer when the code font advances drift from the cell grid" in {
    val state   = buildState("val x = 1")
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    surface.drawRunPxCalls.exists(_.s.contains("val x = 1")) shouldBe true
  }

  it should "use drawRunPx for a ligature-enabled code font that requires measured layout" in {
    val state         = buildState("->")
    val ligatureAttrs = new java.util.HashMap[TextAttribute, Any]()
    ligatureAttrs.put(TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
    val ligatureFont   = monoFont.deriveFont(ligatureAttrs)
    val ligatureMetric = CellMetrics.fromFont(ligatureFont)
    val surface        = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, ligatureFont, propFont, ligatureMetric, None)

    surface.drawRunPxCalls.exists(_.s.contains("->")) shouldBe true
  }

  it should "use the proportional font ascent rather than the code-font ascent" in {
    val state   = buildState("hello markdown", language = Some(LanguageId.Markdown))
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    val expectedAscent = com.serenity.ui.layout.TextLayoutSnapshot
      .fromBuffer(
        state.persisted.buffers(BufferId(1)),
        panelWidthPx = 400,
        propFont,
        surface.fontRenderContext.getOrElse(fail("Expected mock render surface font context"))
      )
      .ascentPx

    val renderedMarkdown = surface.drawRunPxCalls
      .find(_.s.contains("hello markdown"))
      .getOrElse(fail("Expected the Markdown buffer to render through drawRunPx"))
    renderedMarkdown.ascentPx shouldBe expectedAscent
  }

  it should "render proportional selections with highlight colors via drawRunPx" in {
    val bufferId   = BufferId(1)
    val paneId     = PaneId(0)
    val baseBuffer = Buffer.fromString(bufferId, "hello markdown")
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(language = Some(LanguageId.Markdown)),
      editing = baseBuffer.editing.copy(selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 5))))
    )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        theme = Theme.light,
        config = AppConfig.default.withLineNumbers(false).withGutter(false)
      )
    )
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    surface.drawRunPxCalls.exists(call =>
      call.s == "hello" &&
        call.foreground == Theme.light.highlighted.foreground &&
        call.background == Theme.light.highlighted.background &&
        call.clipGlyphToRun
    ) shouldBe true
  }
