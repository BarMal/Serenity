package com.serenity.lsp

import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.model.SemanticToken
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import com.serenity.state.reducers.SystemEventReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the state-side half of issue #859/#1177's rendering slice: `LspSemanticTokensReceived` reaching
  * `runtime.semanticTokensState`, and `AppState.semanticTokensIndexByBuffer` grouping it by line and distinguishing
  * "no tokens received for this document" from "received, but this line has none". The request/decode side is covered
  * by `LspProtocolSpec`/`LspManagerSpec`; `ThemeManager`'s consumption of the per-line result is covered by
  * `LanguageAwareHighlightingSpec`.
  */
class SemanticTokensRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val defaultBufferUri = "buffer:0" // AppState.initial's sole buffer (BufferId(0)) has no file path.
  private val defaultBufferId  = com.serenity.state.models.BufferId(0)

  private def token(line: Int, startCharacter: Int, length: Int, tokenType: String): SemanticToken =
    SemanticToken(line, startCharacter, length, tokenType, Set.empty)

  "SystemEventReducer" should "store semantic tokens from LspSemanticTokensReceived" in {
    val tokens = List(token(0, 0, 3, "keyword"), token(2, 4, 5, "string"))

    val result = SystemEventReducer.reduce(LspEvent.LspSemanticTokensReceived(defaultBufferUri, tokens), AppState.initial)

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

  "AppState.semanticTokensIndexByBuffer" should "be absent for a document that has never received semantic tokens" in {
    AppState.initial.semanticTokensIndexByBuffer(defaultBufferId)() shouldBe None
  }

  it should "group a document's tokens by line once received" in {
    val tokens = List(token(0, 0, 3, "keyword"), token(0, 4, 1, "operator"), token(2, 0, 5, "string"))
    val result = SystemEventReducer.reduce(LspEvent.LspSemanticTokensReceived(defaultBufferUri, tokens), AppState.initial)

    val index = result.state.semanticTokensIndexByBuffer(defaultBufferId)()
    index shouldBe defined
    index.get.getOrElse(0, Nil) should contain theSameElementsAs List(tokens(0), tokens(1))
    index.get.getOrElse(1, Nil) shouldBe Nil
    index.get.getOrElse(2, Nil) shouldBe List(tokens(2))
  }

  it should "distinguish 'connected with no tokens at all' from 'never received'" in {
    val result =
      SystemEventReducer.reduce(LspEvent.LspSemanticTokensReceived(defaultBufferUri, Nil), AppState.initial)

    val index = result.state.semanticTokensIndexByBuffer(defaultBufferId)()
    index shouldBe Some(Map.empty)
  }
