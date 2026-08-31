package com.serenity

import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for issue #1246: spell-check (and any other diagnostic source sharing the same
  * `DiagnosticsState` pipeline) reaches `Renderer` as line-indexed `Diagnostic`s and already paints a gutter "!" marker
  * (`Renderer.renderDiagnosticIndicator`), but nothing painted a highlight over the diagnostic's own text range --
  * neither in the measured (GUI/proportional-font) drawing path nor the cell-based (TUI) one.
  */
class EditorDiagnosticRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithDiagnostic(bufferId: BufferId, buffer: Buffer, paneId: PaneId): AppState =
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val diagnostic = Diagnostic(
      range = LspRange(LspPosition(0, 6), LspPosition(0, 10)),
      severity = Some(DiagnosticSeverity.Hint),
      message = "Unknown word: beta",
      source = Some("spellcheck"),
      code = Some("unknown-word")
    )
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        theme = Theme.light,
        config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
      ),
      runtime = AppState.initial.runtime.copy(
        diagnosticsState = DiagnosticsState(diagnostics = Map(SpellChecker.diagnosticsUri(buffer) -> List(diagnostic)))
      )
    )

  "Renderer.render" should "highlight a misspelled word's diagnostic range in the measured (GUI) drawing path" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma")
      .copy(editing = EditingState(cursors = List(CursorPosition(0, 0))))
    val state = stateWithDiagnostic(bufferId, buffer, paneId)

    val surface = new MockRenderSurface(100, 30)
    val diagnosticBackground =
      Renderer.diagnosticHighlightBackground(state.persisted.theme, Some(DiagnosticSeverity.Hint.code))

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val highlightedRuns = surface.drawRunPxCalls.filter(_.background == diagnosticBackground).map(_.s)

    highlightedRuns should contain("beta")
  }

  it should "highlight a misspelled word's diagnostic range in the cell-based (TUI) drawing path" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma")
      .copy(editing = EditingState(cursors = List(CursorPosition(0, 0))))
    val state = stateWithDiagnostic(bufferId, buffer, paneId)

    // fontRenderContextOverride = None models a terminal surface with no FontRenderContext, forcing the cell-based
    // (non-measured) drawing path -- see #1105.
    val surface = new MockRenderSurface(100, 30, fontRenderContextOverride = None)
    val diagnosticBackground =
      Renderer.diagnosticHighlightBackground(state.persisted.theme, Some(DiagnosticSeverity.Hint.code))

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val highlightedCells = for
      x <- 0 until surface.width
      if "beta".contains(surface.getChar(x, 1))
      if surface.getBg(x, 1) == diagnosticBackground
    yield x

    highlightedCells should have size 4
  }

  it should "not confuse a diagnostic highlight with the selection or document-comment colours" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma")
      .copy(editing = EditingState(cursors = List(CursorPosition(0, 0))))
    val state = stateWithDiagnostic(bufferId, buffer, paneId)

    val surface = new MockRenderSurface(100, 30)
    val diagnosticBackground =
      Renderer.diagnosticHighlightBackground(state.persisted.theme, Some(DiagnosticSeverity.Hint.code))

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    diagnosticBackground should not be state.persisted.theme.highlighted.background
    diagnosticBackground should not be Renderer.commentHighlightBackground(state.persisted.theme)
  }
