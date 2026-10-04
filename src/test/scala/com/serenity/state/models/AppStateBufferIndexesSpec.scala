package com.serenity.state.models

import java.nio.file.Paths

import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange, SemanticToken}
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1864: the paned buffers' annotation and semantic-token indexes are derived once per change of their inputs, not
  * rebuilt (with a `Path.toUri` stat each) by every `AppState` copy.
  */
class AppStateBufferIndexesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def diagnostic(line: Int): Diagnostic =
    Diagnostic(
      range = LspRange(LspPosition(line, 0), LspPosition(line, 4)),
      severity = Some(DiagnosticSeverity.Hint),
      message = "Unknown word",
      source = Some(SpellChecker.Source),
      code = None
    )

  private def stateWithLanguageData: AppState =
    val initial = AppState.initial
    val buffer  = initial.persisted.buffers(bufferId)
    val fileBacked =
      buffer.copy(document = buffer.document.copy(filePath = Some(Paths.get("/tmp/serenity-index-spec.scala"))))
    val uri = SpellChecker.diagnosticsUri(fileBacked)
    initial.copy(
      persisted = initial.persisted.copy(buffers = Map(bufferId -> fileBacked)),
      runtime = initial.runtime.copy(languageService =
        LanguageServiceState(
          diagnosticsState = DiagnosticsState(diagnostics = Map(uri -> List(diagnostic(0)))),
          semanticTokensState =
            SemanticTokensState(byUri = Map(uri -> List(SemanticToken(0, 0, 3, "keyword", Set.empty))))
        )
      )
    )

  private def annotations(state: AppState): AnnotationLineIndex =
    state.annotationIndex(bufferId).getOrElse(fail("expected an annotation index for a paned buffer"))

  private def tokens(state: AppState): SemanticTokensAvailability =
    state.semanticTokensAvailability(bufferId).getOrElse(fail("expected semantic tokens for a paned buffer"))

  "AppState" should "serve the committed indexes to a copy of an unrelated field without recomputing them" in {
    val committed = stateWithLanguageData.withBufferIndexesRefreshed
    val copied    = committed.copy(runtime = committed.runtime.observeTyping(1L))

    annotations(copied) should be theSameInstanceAs annotations(committed)
    tokens(copied) should be theSameInstanceAs tokens(committed)
  }

  it should "recompute an index once one of its inputs changes, never serving the stale one" in {
    val committed = stateWithLanguageData.withBufferIndexesRefreshed
    val uri       = SpellChecker.diagnosticsUri(committed.persisted.buffers(bufferId))
    val language  = committed.runtime.languageService
    val rediagnosed = committed.copy(runtime =
      committed.runtime.copy(languageService =
        language.copy(diagnosticsState = DiagnosticsState(diagnostics = Map(uri -> List(diagnostic(3)))))
      )
    )

    annotations(rediagnosed).diagnosticsByLine.keySet shouldBe Set(3)
    annotations(rediagnosed.withBufferIndexesRefreshed).diagnosticsByLine.keySet shouldBe Set(3)
    tokens(rediagnosed.withBufferIndexesRefreshed) should be theSameInstanceAs tokens(committed)
  }

  it should "return the very same state from a refresh when no index input changed" in {
    val committed = stateWithLanguageData.withBufferIndexesRefreshed

    committed.withBufferIndexesRefreshed should be theSameInstanceAs committed
  }

  it should "compare equal whether or not its indexes have been refreshed" in {
    val state = stateWithLanguageData

    state.withBufferIndexesRefreshed shouldBe state
    state.withBufferIndexesRefreshed.hashCode shouldBe state.hashCode
  }
