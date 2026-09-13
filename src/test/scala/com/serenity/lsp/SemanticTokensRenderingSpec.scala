package com.serenity.lsp

import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.model.SemanticToken
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, SemanticTokensAvailability}
import com.serenity.state.reducers.SystemEventReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the state-side half of issue #859/#1177's rendering slice: `LspSemanticTokensReceived`/
  * `LspSemanticTokensUnavailable` reaching `runtime.semanticTokensState`, and `AppState.semanticTokensAvailability`
  * grouping received tokens by line and distinguishing all three of `Pending` (nothing confirmed yet -- a request may
  * be in flight), `Unavailable` (confirmed no server/capability), and `Available` (tokens received, however many). The
  * request/decode side is covered by `LspProtocolSpec`/`LspManagerSpec`; `ThemeManager`'s consumption of the per-line
  * result is covered by `LanguageAwareHighlightingSpec`.
  */
class SemanticTokensRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val defaultBufferUri = "buffer:0" // AppState.initial's sole buffer (BufferId(0)) has no file path.
  private val defaultBufferId  = com.serenity.state.models.BufferId(0)

  private def token(line: Int, startCharacter: Int, length: Int, tokenType: String): SemanticToken =
    SemanticToken(line, startCharacter, length, tokenType, Set.empty)

  "SystemEventReducer" should "store semantic tokens from LspSemanticTokensReceived" in {
    val tokens = List(token(0, 0, 3, "keyword"), token(2, 4, 5, "string"))

    val result =
      SystemEventReducer.reduce(LspEvent.LspSemanticTokensReceived(defaultBufferUri, tokens), AppState.initial)

    result.state.runtime.semanticTokensState.byUri should contain key defaultBufferUri
    result.state.runtime.semanticTokensState.byUri(defaultBufferUri) shouldBe tokens
    result.effects shouldBe empty
  }

  it should "replace semantic tokens for the same URI rather than accumulate them" in {
    val initial = AppState.initial
    val state = initial.copy(runtime =
      initial.runtime.copy(semanticTokensState =
        initial.runtime.semanticTokensState.copy(byUri = Map(defaultBufferUri -> List(token(0, 0, 3, "keyword"))))
      )
    )

    val newTokens = List(token(0, 0, 3, "string"))
    val result    = SystemEventReducer.reduce(LspEvent.LspSemanticTokensReceived(defaultBufferUri, newTokens), state)

    result.state.runtime.semanticTokensState.byUri(defaultBufferUri) shouldBe newTokens
  }

  "AppState.semanticTokensAvailability" should "be Pending for a document that has never received semantic tokens" in {
    AppState.initial
      .semanticTokensAvailability(defaultBufferId)
      .getOrElse(fail("expected an index for a buffer that exists")) shouldBe SemanticTokensAvailability.Pending
  }

  it should "group a document's tokens by line once received" in {
    val tokens = List(token(0, 0, 3, "keyword"), token(0, 4, 1, "operator"), token(2, 0, 5, "string"))
    val result =
      SystemEventReducer.reduce(LspEvent.LspSemanticTokensReceived(defaultBufferUri, tokens), AppState.initial)

    result.state
      .semanticTokensAvailability(defaultBufferId)
      .getOrElse(fail("expected an index for a buffer that exists")) match
      case SemanticTokensAvailability.Available(byLine) =>
        byLine.getOrElse(0, Nil) should contain theSameElementsAs List(tokens(0), tokens(1))
        byLine.getOrElse(1, Nil) shouldBe Nil
        byLine.getOrElse(2, Nil) shouldBe List(tokens(2))
      case other => fail(s"expected Available, got $other")
  }

  it should "distinguish 'connected with no tokens at all' from 'never received'" in {
    val result =
      SystemEventReducer.reduce(LspEvent.LspSemanticTokensReceived(defaultBufferUri, Nil), AppState.initial)

    result.state
      .semanticTokensAvailability(defaultBufferId)
      .getOrElse(fail("expected an index for a buffer that exists")) shouldBe SemanticTokensAvailability.Available(
      Map.empty
    )
  }

  it should "report Unavailable, not Pending, once LspSemanticTokensUnavailable is applied" in {
    val result = SystemEventReducer.reduce(LspEvent.LspSemanticTokensUnavailable(defaultBufferUri), AppState.initial)

    result.state
      .semanticTokensAvailability(defaultBufferId)
      .getOrElse(fail("expected an index for a buffer that exists")) shouldBe SemanticTokensAvailability.Unavailable
  }

  it should "become Available again once tokens finally arrive for a document previously marked Unavailable" in {
    val unavailable =
      SystemEventReducer.reduce(LspEvent.LspSemanticTokensUnavailable(defaultBufferUri), AppState.initial).state

    val tokens = List(token(0, 0, 3, "keyword"))
    val result = SystemEventReducer.reduce(LspEvent.LspSemanticTokensReceived(defaultBufferUri, tokens), unavailable)

    result.state.runtime.semanticTokensState.unavailableUris shouldNot contain(defaultBufferUri)
    result.state
      .semanticTokensAvailability(defaultBufferId)
      .getOrElse(fail("expected an index for a buffer that exists")) shouldBe SemanticTokensAvailability.Available(
      Map(0 -> tokens)
    )
  }

  "SystemEventReducer" should "mark a document Unavailable and clear any previously received tokens for it" in {
    val received =
      SystemEventReducer
        .reduce(LspEvent.LspSemanticTokensReceived(defaultBufferUri, List(token(0, 0, 3, "keyword"))), AppState.initial)
        .state

    val result = SystemEventReducer.reduce(LspEvent.LspSemanticTokensUnavailable(defaultBufferUri), received)

    result.state.runtime.semanticTokensState.byUri shouldNot contain key defaultBufferUri
    result.state.runtime.semanticTokensState.unavailableUris should contain(defaultBufferUri)
    result.effects shouldBe empty
  }
