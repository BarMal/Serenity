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

/** Verifies that a buffer with language=Markdown (triggering textFont which is proportional) renders
  * via drawRunPx, while a code buffer renders via putString.
  */
class RendererProportionalRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val propFont     = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val monoMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(80, 24)

  private def buildState(content: String, language: Option[LanguageId] = None): AppState =
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, content).copy(language = language)
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      buffers     = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes        = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      theme  = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false)
    )

  "Renderer" should "use drawRunPx for a Markdown buffer (proportional textFont)" in {
    val state   = buildState("hello markdown", language = Some(LanguageId.Markdown))
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    // codeFont = mono, textFont = proportional — Markdown buffer picks textFont
    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    surface.drawRunPxCalls should not be empty
    surface.putStringCalls.exists(_.s.contains("hello")) shouldBe false
  }

  it should "use putString for a code buffer (monospaced codeFont)" in {
    val state   = buildState("val x = 1")
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, propFont, monoMetrics, None)

    surface.putStringCalls.exists(_.s.contains("val x = 1")) shouldBe true
    surface.drawRunPxCalls shouldBe empty
  }
