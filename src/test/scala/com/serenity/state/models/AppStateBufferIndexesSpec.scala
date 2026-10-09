package com.serenity.state.models

import java.nio.file.Paths

import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange, SemanticToken, SemanticTokenData}
import com.serenity.rope.{Balance, Rope}
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
          semanticTokensState = SemanticTokensState(byUri =
            Map(uri -> SemanticTokenData.from(List(SemanticToken(0, 0, 3, "keyword", Set.empty))))
          )
        )
      )
    )

  private val otherUri = DocumentUri("file:///tmp/serenity-index-spec-other.scala")

  private def withLanguageService(state: AppState, update: LanguageServiceState => LanguageServiceState): AppState =
    state.copy(runtime = state.runtime.copy(languageService = update(state.runtime.languageService)))

  private def withSemanticTokens(state: AppState, semanticTokens: SemanticTokensState): AppState =
    withLanguageService(state, _.copy(semanticTokensState = semanticTokens))

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

  it should "keep a buffer's indexes when language data arrives only for another document" in {
    val committed = stateWithLanguageData.withBufferIndexesRefreshed
    val elsewhere = withLanguageService(
      committed,
      language =>
        language.copy(
          diagnosticsState = language.diagnosticsState.copy(diagnostics =
            language.diagnosticsState.diagnostics.updated(otherUri, List(diagnostic(1)))
          ),
          semanticTokensState = language.semanticTokensState.copy(byUri =
            language.semanticTokensState.byUri
              .updated(otherUri, SemanticTokenData.from(List(SemanticToken(1, 0, 2, "type", Set.empty))))
          )
        )
    ).withBufferIndexesRefreshed

    annotations(elsewhere) should be theSameInstanceAs annotations(committed)
    tokens(elsewhere) should be theSameInstanceAs tokens(committed)
  }

  it should "derive a file-backed buffer's document URI once, not again after every edit" in {
    val committed = stateWithLanguageData.withBufferIndexesRefreshed
    val buffer    = committed.persisted.buffers(bufferId)
    val edited = committed
      .copy(persisted =
        committed.persisted
          .copy(buffers = Map(bufferId -> buffer.copy(document = buffer.document.withContent(Rope("edited")))))
      )
      .withBufferIndexesRefreshed

    def uriText(state: AppState): String = state.documentUri(bufferId).getOrElse(fail("no document URI")).value

    uriText(edited) shouldBe SpellChecker.diagnosticsUri(buffer).value
    uriText(edited) should be theSameInstanceAs uriText(committed)
  }

  it should "tell a pending, an empty and a confirmed-absent token response apart across refreshes" in {
    val uri     = SpellChecker.diagnosticsUri(stateWithLanguageData.persisted.buffers(bufferId))
    val pending = withSemanticTokens(stateWithLanguageData, SemanticTokensState()).withBufferIndexesRefreshed
    val empty = withSemanticTokens(
      pending,
      SemanticTokensState(byUri = Map(uri -> SemanticTokenData.empty))
    ).withBufferIndexesRefreshed
    val absent =
      withSemanticTokens(empty, SemanticTokensState(unavailableUris = Set(uri))).withBufferIndexesRefreshed

    tokens(pending) shouldBe SemanticTokensAvailability.Pending
    tokens(empty) shouldBe SemanticTokensAvailability.Available(Map.empty)
    tokens(absent) shouldBe SemanticTokensAvailability.Unavailable
    tokens(withSemanticTokens(absent, SemanticTokensState()).withBufferIndexesRefreshed) shouldBe
      SemanticTokensAvailability.Pending
  }
