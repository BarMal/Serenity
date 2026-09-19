package com.serenity

import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{RendererEntryPoints, RendererHighlights}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1530: focus mode's dim pass (`RendererPaneContent.focusedTextBodyLines`) and the diagnostic highlight pass
  * (`RendererHighlights.renderDiagnosticHighlights`) used to clobber each other -- the diagnostic pass ran second and
  * unconditionally repainted its column range with full-intensity theme colours, undoing the dim. A misspelled word in
  * a dimmed (out-of-focus) paragraph should still read as dimmed overall, with its diagnostic marker blended against
  * that dimmed colour rather than painted at full intensity.
  */
class DiagnosticDimmingCompositionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithDiagnosticOnLine(
    bufferId: BufferId,
    buffer: Buffer,
    paneId: PaneId,
    diagnosticLine: Int,
    range: (Int, Int)
  ): AppState =
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val diagnostic = Diagnostic(
      range = LspRange(LspPosition(diagnosticLine, range._1), LspPosition(diagnosticLine, range._2)),
      severity = Some(DiagnosticSeverity.Hint),
      message = "Unknown word",
      source = Some("spellcheck"),
      code = Some("unknown-word")
    )
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false).withFocusedTextBody(true)
      ),
      runtime = AppState.initial.runtime.copy(
        diagnosticsState = DiagnosticsState(diagnostics = Map(SpellChecker.diagnosticsUri(buffer) -> List(diagnostic)))
      )
    )

  // Cursor stays on line 0, so the blank-line-separated paragraph at line 2 ("misspelled wrod") is out of focus and
  // dimmed by `focusedTextBodyEnabled` -- see `FocusedTextBody.activeRange`'s blank-line paragraph boundary.
  private val content = "alpha beta gamma\n\nmisspelled wrod"

  "A diagnostic inside a dimmed, out-of-focus paragraph" should "blend against the dimmed colour, not full intensity" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, content)
      .copy(editing = EditingState(List(CursorPosition(0, 0))))
    val state = stateWithDiagnosticOnLine(bufferId, buffer, paneId, diagnosticLine = 2, range = (11, 15))

    val surface = new MockRenderSurface(100, 30)
    RendererEntryPoints.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val theme                   = state.persisted.theme
    val severityCode            = Some(DiagnosticSeverity.Hint.code)
    val fullIntensityBackground = RendererHighlights.diagnosticHighlightBackground(theme, severityCode)
    val dimmedBackground        = RendererHighlights.diagnosticHighlightBackground(theme, severityCode, dimmed = true)

    val highlightedRun =
      surface.drawRunPxCalls.find(_.s == "wrod").getOrElse(fail("Expected the misspelled word to be drawn"))

    dimmedBackground should not be fullIntensityBackground
    highlightedRun.background shouldBe dimmedBackground
    highlightedRun.background should not be fullIntensityBackground
  }

  "A diagnostic inside the active (in-focus) paragraph" should "still render at full intensity" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, content)
      .copy(editing = EditingState(List(CursorPosition(0, 0))))
    val state = stateWithDiagnosticOnLine(bufferId, buffer, paneId, diagnosticLine = 0, range = (6, 10))

    val surface = new MockRenderSurface(100, 30)
    RendererEntryPoints.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val theme                   = state.persisted.theme
    val severityCode            = Some(DiagnosticSeverity.Hint.code)
    val fullIntensityBackground = RendererHighlights.diagnosticHighlightBackground(theme, severityCode)

    val highlightedRun =
      surface.drawRunPxCalls.find(_.s == "beta").getOrElse(fail("Expected the diagnostic word to be drawn"))

    highlightedRun.background shouldBe fullIntensityBackground
  }

end DiagnosticDimmingCompositionSpec
