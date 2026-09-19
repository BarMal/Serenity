package com.serenity

import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{RendererEntryPoints, RendererHighlights}
import com.serenity.ui.theme.{TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1529: the misspelled-word highlight's foreground was hardcoded to `theme.foreground` regardless of severity, its
  * background blend weight was a hardcoded literal, and it never applied `TextStyle.isUnderlined` from the theme's own
  * `error`/`warning` colours even though other paint sites (`ThemeManager`) already treat `ThemeColor.style` as
  * genuinely theme-configurable.
  */
class DiagnosticHighlightThemeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "RendererHighlights.diagnosticHighlightForeground" should "use the error colour for an error-severity diagnostic" in {
    val theme = Theme.light
    RendererHighlights.diagnosticHighlightForeground(theme, Some(DiagnosticSeverity.Error.code)) shouldBe
      theme.error.foreground
  }

  it should "use the warning colour for a warning-severity diagnostic" in {
    val theme = Theme.light
    RendererHighlights.diagnosticHighlightForeground(theme, Some(DiagnosticSeverity.Warning.code)) shouldBe
      theme.warning.foreground
  }

  it should "fall back to the plain foreground for an unrecognised severity" in {
    val theme = Theme.light
    RendererHighlights.diagnosticHighlightForeground(theme, None) shouldBe theme.foreground
  }

  "RendererHighlights.diagnosticHighlightBackground" should "honour a caller-supplied blend weight" in {
    val theme        = Theme.light
    val severityCode = Some(DiagnosticSeverity.Warning.code)

    val lightlyBlended = RendererHighlights.diagnosticHighlightBackground(theme, severityCode, blendWeight = 0.1)
    val heavilyBlended = RendererHighlights.diagnosticHighlightBackground(theme, severityCode, blendWeight = 0.9)

    lightlyBlended should not be heavilyBlended
  }

  "A theme whose warning colour is underlined" should "render the diagnostic run underlined" in {
    val underlinedWarning = Theme.light.warning.copy(style = TextStyle(isUnderlined = true))
    val theme             = Theme.light.copy(warning = underlinedWarning)

    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma")
      .copy(editing = EditingState(List(CursorPosition(0, 0))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val diagnostic = Diagnostic(
      range = LspRange(LspPosition(0, 6), LspPosition(0, 10)),
      severity = Some(DiagnosticSeverity.Warning),
      message = "Unknown word: beta",
      source = Some("spellcheck"),
      code = Some("unknown-word")
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = theme,
        config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
      ),
      runtime = AppState.initial.runtime.copy(
        diagnosticsState = DiagnosticsState(diagnostics = Map(SpellChecker.diagnosticsUri(buffer) -> List(diagnostic)))
      )
    )

    val surface = new MockRenderSurface(100, 30)
    RendererEntryPoints.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val highlightedRun = surface.drawRunPxCalls.find(_.s == "beta").getOrElse(fail("Expected \"beta\" to be drawn"))
    highlightedRun.activeStyle.isUnderlined shouldBe true
  }

  it should "not underline the diagnostic run when the theme's warning colour is not underlined" in {
    val theme = Theme.light // default warning colour carries no underline

    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma")
      .copy(editing = EditingState(List(CursorPosition(0, 0))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val diagnostic = Diagnostic(
      range = LspRange(LspPosition(0, 6), LspPosition(0, 10)),
      severity = Some(DiagnosticSeverity.Warning),
      message = "Unknown word: beta",
      source = Some("spellcheck"),
      code = Some("unknown-word")
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = theme,
        config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
      ),
      runtime = AppState.initial.runtime.copy(
        diagnosticsState = DiagnosticsState(diagnostics = Map(SpellChecker.diagnosticsUri(buffer) -> List(diagnostic)))
      )
    )

    val surface = new MockRenderSurface(100, 30)
    RendererEntryPoints.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val highlightedRun = surface.drawRunPxCalls.find(_.s == "beta").getOrElse(fail("Expected \"beta\" to be drawn"))
    highlightedRun.activeStyle.isUnderlined shouldBe false
  }

end DiagnosticHighlightThemeSpec
