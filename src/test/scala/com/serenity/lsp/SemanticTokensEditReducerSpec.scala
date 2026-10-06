package com.serenity.lsp

import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.model.{LspPosition, LspRange, SemanticToken, SemanticTokenData, TextChangeDiff}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, SemanticTokensAvailability}
import com.serenity.state.reducers.SystemEventReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tokens in state keep colouring the same characters between a local edit and the server's next answer (#1837), and a
  * range answer replaces only the lines it covers.
  */
class SemanticTokensEditReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferUri = "buffer:0" // AppState.initial's sole buffer has no file path.
  private val bufferId  = BufferId(0)

  private def token(line: Int, start: Int, length: Int, tokenType: String): SemanticToken =
    SemanticToken(line, start, length, tokenType, Set.empty)

  private def reduce(event: LspEvent, state: AppState): AppState = SystemEventReducer.reduce(event, state).state

  private def received(tokens: SemanticToken*): LspEvent =
    LspEvent.LspSemanticTokensReceived(bufferUri, SemanticTokenData.from(tokens.toList))

  private def edited(line: Int, character: Int, text: String): LspEvent =
    LspEvent.LspSemanticTokensEdited(
      bufferUri,
      TextChangeDiff.Change(LspRange(LspPosition(line, character), LspPosition(line, character)), 0, text)
    )

  private def byLine(state: AppState): Map[Int, List[SemanticToken]] =
    state.semanticTokensAvailability(bufferId) match
      case Some(SemanticTokensAvailability.Available(lines)) => lines
      case other                                             => fail(s"expected Available, got $other")

  "SystemEventReducer" should "move highlighted lines down with the text when a newline is inserted above them" in {
    val highlighted = reduce(
      received(token(0, 0, 6, "keyword"), token(1, 4, 3, "string"), token(2, 0, 7, "comment")),
      AppState.initial
    )

    val afterNewline = reduce(edited(0, 0, "\n"), highlighted)

    byLine(afterNewline) shouldBe Map(
      1 -> List(token(1, 0, 6, "keyword")),
      2 -> List(token(2, 4, 3, "string")),
      3 -> List(token(3, 0, 7, "comment"))
    )
  }

  it should "replace the shifted tokens with the server's answer once it arrives" in {
    val highlighted  = reduce(received(token(0, 0, 6, "keyword")), AppState.initial)
    val afterNewline = reduce(edited(0, 0, "\n"), highlighted)

    val answered = reduce(received(token(1, 0, 6, "keyword"), token(2, 0, 2, "string")), afterNewline)

    byLine(answered) shouldBe Map(1 -> List(token(1, 0, 6, "keyword")), 2 -> List(token(2, 0, 2, "string")))
  }

  it should "leave a document with no tokens held without tokens when it is edited" in {
    val state = reduce(edited(0, 0, "\n"), AppState.initial)

    state.semanticTokensAvailability(bufferId) shouldBe Some(SemanticTokensAvailability.Pending)
  }

  it should "keep a document that was marked unavailable unavailable when it is edited" in {
    val unavailable = reduce(LspEvent.LspSemanticTokensUnavailable(bufferUri), AppState.initial)

    reduce(edited(0, 0, "\n"), unavailable)
      .semanticTokensAvailability(bufferId) shouldBe Some(SemanticTokensAvailability.Unavailable)
  }

  it should "replace only the lines a range answer covers" in {
    val held = reduce(
      received(token(0, 0, 3, "keyword"), token(5, 0, 3, "keyword"), token(9, 0, 3, "keyword")),
      AppState.initial
    )

    val ranged = reduce(
      LspEvent.LspSemanticTokensRangeReceived(
        bufferUri,
        4,
        6,
        SemanticTokenData.from(List(token(5, 2, 4, "string")))
      ),
      held
    )

    byLine(ranged) shouldBe Map(
      0 -> List(token(0, 0, 3, "keyword")),
      5 -> List(token(5, 2, 4, "string")),
      9 -> List(token(9, 0, 3, "keyword"))
    )
  }

  it should "show a range answer for a document that holds no tokens yet" in {
    val ranged = reduce(
      LspEvent.LspSemanticTokensRangeReceived(
        bufferUri,
        4,
        6,
        SemanticTokenData.from(List(token(5, 2, 4, "string")))
      ),
      AppState.initial
    )

    byLine(ranged) shouldBe Map(5 -> List(token(5, 2, 4, "string")))
    ranged.runtime.languageService.semanticTokensState.unavailableUris shouldNot contain(DocumentUri(bufferUri))
  }
