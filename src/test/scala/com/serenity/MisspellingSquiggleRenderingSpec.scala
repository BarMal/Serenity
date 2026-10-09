package com.serenity

import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1809: a misspelling read as a yellow warning highlight. It is drawn as a squiggle under the word in the theme's
  * error colour instead, leaving the word's own background untouched.
  */
class MisspellingSquiggleRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def misspelling(severity: DiagnosticSeverity) = Diagnostic(
    range = LspRange(LspPosition(0, 6), LspPosition(0, 10)),
    severity = Some(severity),
    message = "Possible spelling issue: beta",
    source = Some(SpellChecker.Source),
    code = Some("unknown-word")
  )

  private def render(theme: Theme, diagnostic: Diagnostic): MockRenderSurface =
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma")
      .copy(editing = EditingState(List(CursorPosition(0, 0))))
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = theme,
        config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
      ),
      runtime = AppState.initial.runtime.copy(
        languageService = LanguageServiceState(
          diagnosticsState =
            DiagnosticsState(diagnostics = Map(SpellChecker.diagnosticsUri(buffer) -> List(diagnostic)))
        )
      )
    )
    val surface = new MockRenderSurface(100, 30)
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      ViewportSize(100, 30),
      com.serenity.state.manager.RenderCaches.create()
    )
    surface

  private def wordRun(surface: MockRenderSurface, word: String): MockRenderSurface#DrawRunPxCall =
    surface.drawRunPxCalls.filter(_.s.contains(word)).lastOption.getOrElse(fail(s"Expected \"$word\" to be drawn"))

  "A misspelled word" should "keep the background of the words around it" in
    List(Theme.light, Theme.dark).foreach { theme =>
      val surface = render(theme, misspelling(DiagnosticSeverity.Warning))

      wordRun(surface, "beta").background shouldBe wordRun(surface, "alpha").background
    }

  it should "be underlined by a squiggle in the theme's error colour spanning the word" in
    List(Theme.light, Theme.dark).foreach { theme =>
      val surface  = render(theme, misspelling(DiagnosticSeverity.Warning))
      val line     = wordRun(surface, "alpha")
      val squiggle = surface.fillPixelRectCalls.filter(_.color == theme.error.foreground)

      squiggle should not be empty
      squiggle.map(_.yPx).distinct.size should be > 1
      squiggle.map(_.yPx).min should be >= line.yPx + line.ascentPx
      squiggle.map(_.xPx).max - squiggle.map(_.xPx).min should be > 0
    }

  it should "be drawn the same way whatever severity the diagnostic carries" in {
    val surface = render(Theme.light, misspelling(DiagnosticSeverity.Information))

    surface.fillPixelRectCalls.filter(_.color == Theme.light.error.foreground) should not be empty
  }
