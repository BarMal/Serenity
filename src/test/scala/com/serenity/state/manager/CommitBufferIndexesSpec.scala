package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange, SemanticToken, SemanticTokenData}
import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.{AppState, BufferId, DiagnosticsState, LanguageServiceState, SemanticTokensState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1864: every commit leaves the paned buffers' indexes current, so the copies the next event makes (and the frames
  * rendered from them) read the committed index instead of rebuilding it.
  */
class CommitBufferIndexesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "prepareCommit" should "commit a state whose paned buffer indexes later copies reuse" in {
    val state = AppState.initial
    val committed =
      StateManagerOperationBoundary.prepareCommit(state, state).fold(errors => fail(errors.mkString), identity)
    val copied = committed.copy(runtime = committed.runtime.observeTyping(1L))

    copied.annotationIndex(BufferId(0)).getOrElse(fail("no annotation index")) should be theSameInstanceAs
      committed.annotationIndex(BufferId(0)).getOrElse(fail("no annotation index"))
    copied.semanticTokensAvailability(BufferId(0)).getOrElse(fail("no semantic tokens")) should be theSameInstanceAs
      committed.semanticTokensAvailability(BufferId(0)).getOrElse(fail("no semantic tokens"))
  }

  it should "keep the refreshed indexes through a run of keystrokes and rebuild only the one whose input changed" in {
    val initial = AppState.initial
    val buffer  = initial.persisted.buffers(BufferId(0))
    val fileBacked =
      buffer.copy(document = buffer.document.copy(filePath = Some(Paths.get("/tmp/serenity-commit-index-spec.scala"))))
    val uri = SpellChecker.diagnosticsUri(fileBacked)
    def diagnostic(line: Int) = Diagnostic(
      range = LspRange(LspPosition(line, 0), LspPosition(line, 4)),
      severity = Some(DiagnosticSeverity.Hint),
      message = "Unknown word",
      source = Some(SpellChecker.Source),
      code = None
    )
    val withLanguageData = initial.copy(
      persisted = initial.persisted.copy(buffers = Map(BufferId(0) -> fileBacked)),
      runtime = initial.runtime.copy(languageService =
        LanguageServiceState(
          diagnosticsState = DiagnosticsState(diagnostics = Map(uri -> List(diagnostic(0)))),
          semanticTokensState = SemanticTokensState(byUri =
            Map(uri -> SemanticTokenData.from(List(SemanticToken(0, 0, 3, "keyword", Set.empty))))
          )
        )
      )
    )
    def annotations(state: AppState) = state.annotationIndex(BufferId(0)).getOrElse(fail("no annotation index"))
    def tokens(state: AppState) = state.semanticTokensAvailability(BufferId(0)).getOrElse(fail("no semantic tokens"))

    val first = withLanguageData.withBufferIndexesRefreshed
    val typed = (1 to 50).foldLeft(first): (state, keystroke) =>
      val edited = state.persisted.buffers(BufferId(0))
      val next = state.copy(persisted =
        state.persisted.copy(buffers =
          Map(BufferId(0) -> edited.copy(document = edited.document.replacedWith(Rope("x" * keystroke))))
        )
      )
      next.withBufferIndexesRefreshed

    annotations(typed) should be theSameInstanceAs annotations(first)
    tokens(typed) should be theSameInstanceAs tokens(first)

    val rediagnosed = typed
      .copy(runtime =
        typed.runtime.copy(languageService =
          typed.runtime.languageService
            .copy(diagnosticsState = DiagnosticsState(diagnostics = Map(uri -> List(diagnostic(3)))))
        )
      )
      .withBufferIndexesRefreshed
    annotations(rediagnosed).diagnosticsByLine.keySet shouldBe Set(3)
    tokens(rediagnosed) should be theSameInstanceAs tokens(first)
  }
