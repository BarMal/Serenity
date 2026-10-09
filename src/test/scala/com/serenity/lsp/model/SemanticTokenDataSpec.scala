package com.serenity.lsp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SemanticTokenDataSpec extends AnyFlatSpec with Matchers:

  private val legend = SemanticTokensLegend(List("keyword", "string", "comment"), List("declaration", "readonly"))

  private def token(line: Int, start: Int, length: Int, tokenType: String = "keyword"): SemanticToken =
    SemanticToken(line, start, length, tokenType, Set.empty)

  private def data(tokens: SemanticToken*): SemanticTokenData = SemanticTokenData.from(tokens.toList)

  private def change(
    startLine: Int,
    startChar: Int,
    endLine: Int,
    endChar: Int,
    text: String
  ): TextChangeDiff.Change =
    TextChangeDiff.Change(
      LspRange(LspPosition(startLine, startChar), LspPosition(endLine, endChar)),
      rangeLength = 0,
      text = text
    )

  private def insertion(line: Int, character: Int, text: String): TextChangeDiff.Change =
    change(line, character, line, character, text)

  "SemanticTokenData.decode" should "resolve relative positions, the legend and the modifier bits" in {
    val raw = IArray(0, 0, 3, 0, 0, 0, 4, 5, 1, 3, 2, 1, 10, 2, 0)

    SemanticTokenData.decode(raw, legend).tokens shouldBe List(
      token(0, 0, 3),
      SemanticToken(0, 4, 5, "string", Set("declaration", "readonly")),
      token(2, 1, 10, "comment")
    )
  }

  it should "leave out a token whose type is outside the legend but keep positions relative to it" in {
    val raw = IArray(0, 0, 3, 7, 0, 0, 4, 2, 0, 0)

    SemanticTokenData.decode(raw, legend).tokens shouldBe List(token(0, 4, 2))
  }

  it should "ignore a trailing partial token" in {
    SemanticTokenData.decode(IArray(0, 0, 3, 0, 0, 1, 2), legend).tokens shouldBe List(token(0, 0, 3))
  }

  it should "decode an empty array as no tokens" in {
    SemanticTokenData.decode(IArray.empty[Int], legend).isEmpty shouldBe true
  }

  "SemanticTokenData.acceptEdit" should "move every token down a line when a line break is inserted above them" in {
    val held = data(token(0, 0, 3), token(1, 4, 5, "string"), token(2, 0, 2, "comment"))

    held.acceptEdit(insertion(0, 0, "\n")).tokens shouldBe List(
      token(1, 0, 3),
      token(2, 4, 5, "string"),
      token(3, 0, 2, "comment")
    )
  }

  it should "move tokens up a line when a line break above them is deleted" in {
    val held = data(token(1, 2, 3), token(2, 0, 4))

    held.acceptEdit(change(0, 5, 1, 0, "")).tokens shouldBe List(token(0, 7, 3), token(1, 0, 4))
  }

  it should "leave tokens before the edit where they are" in {
    val held = data(token(0, 0, 3), token(1, 0, 4))

    held.acceptEdit(insertion(5, 0, "\n\n")).tokens shouldBe held.tokens
  }

  it should "shift a token on the edited line to the right of the edit by the inserted length" in {
    val held = data(token(0, 0, 3), token(0, 10, 4, "string"))

    held.acceptEdit(insertion(0, 5, "abc")).tokens shouldBe List(token(0, 0, 3), token(0, 13, 4, "string"))
  }

  it should "shift a token after a replaced span by the difference in length" in {
    val held = data(token(0, 10, 4, "string"))

    held.acceptEdit(change(0, 2, 0, 6, "x")).tokens shouldBe List(token(0, 7, 4, "string"))
  }

  it should "place a token after a line break inserted on its own line against the new line's start" in {
    val held = data(token(0, 10, 4, "string"))

    held.acceptEdit(insertion(0, 5, "ab\ncd")).tokens shouldBe List(token(1, 7, 4, "string"))
  }

  it should "keep a token that begins exactly where text is inserted moving with the text after it" in {
    val held = data(token(0, 4, 3))

    held.acceptEdit(insertion(0, 4, "  ")).tokens shouldBe List(token(0, 6, 3))
  }

  it should "leave a token that ends exactly where text is inserted as it was" in {
    val held = data(token(0, 4, 3))

    held.acceptEdit(insertion(0, 7, "x")).tokens shouldBe List(token(0, 4, 3))
  }

  it should "grow a token when text is typed inside it" in {
    val held = data(token(0, 4, 6))

    held.acceptEdit(insertion(0, 7, "ab")).tokens shouldBe List(token(0, 4, 8))
  }

  it should "shrink a token when characters are deleted inside it" in {
    val held = data(token(0, 4, 6))

    held.acceptEdit(change(0, 5, 0, 7, "")).tokens shouldBe List(token(0, 4, 4))
  }

  it should "drop a token that the edit cuts across" in {
    val held = data(token(0, 4, 6), token(0, 12, 2))

    held.acceptEdit(change(0, 8, 0, 13, "z")).tokens shouldBe Nil
  }

  it should "drop a token that is split by an inserted line break" in {
    data(token(0, 4, 6)).acceptEdit(insertion(0, 6, "\n")).tokens shouldBe Nil
  }

  it should "drop a token the edit deletes entirely" in {
    data(token(0, 4, 6), token(0, 20, 2)).acceptEdit(change(0, 4, 0, 10, "")).tokens shouldBe List(token(0, 14, 2))
  }

  it should "keep applying successive edits to what the earlier ones left" in {
    val held = data(token(0, 0, 3), token(2, 4, 5, "string"))

    val shifted = held
      .acceptEdit(insertion(0, 0, "\n"))
      .acceptEdit(insertion(1, 0, "ab"))
      .acceptEdit(insertion(0, 0, "\n\n"))

    shifted.tokens shouldBe List(token(3, 2, 3), token(5, 4, 5, "string"))
  }

  it should "preserve modifiers and types of the tokens it moves" in {
    val held = SemanticTokenData.decode(IArray(0, 0, 3, 1, 3), legend)

    held.acceptEdit(insertion(0, 0, "\n")).tokens shouldBe
      List(SemanticToken(1, 0, 3, "string", Set("declaration", "readonly")))
  }

  "SemanticTokenData.replaceLines" should "replace only the tokens on the given lines" in {
    val held        = data(token(0, 0, 3), token(5, 0, 3), token(6, 0, 3), token(9, 0, 3))
    val replacement = data(token(5, 1, 2, "string"), token(7, 0, 4, "string"), token(20, 0, 1))

    held.replaceLines(5, 8, replacement).tokens shouldBe List(
      token(0, 0, 3),
      token(5, 1, 2, "string"),
      token(7, 0, 4, "string"),
      token(9, 0, 3)
    )
  }

  it should "take the replacement's tokens when nothing is held on those lines" in {
    SemanticTokenData.empty.replaceLines(1, 3, data(token(2, 0, 3))).tokens shouldBe List(token(2, 0, 3))
  }

  it should "replace on the packed tokens when both are packed against the same legend" in {
    val held        = SemanticTokenData.decode(IArray(0, 0, 3, 0, 0, 5, 0, 3, 1, 0, 1, 2, 4, 2, 0), legend)
    val replacement = SemanticTokenData.decode(IArray(5, 1, 2, 2, 0), legend)

    held.replaceLines(5, 6, replacement).tokens shouldBe List(token(0, 0, 3), token(5, 1, 2, "comment"))
  }

  it should "merge tokens packed against different legends by name" in {
    val held        = SemanticTokenData.decode(IArray(0, 0, 3, 0, 0, 5, 0, 3, 1, 0), legend)
    val replacement = SemanticTokenData.decode(IArray(5, 0, 2, 0, 0), SemanticTokensLegend(List("string"), Nil))

    held.replaceLines(4, 6, replacement).tokens shouldBe List(token(0, 0, 3), token(5, 0, 2, "string"))
  }

  "SemanticTokenData" should "group tokens by line" in {
    data(token(0, 0, 3), token(0, 4, 1), token(2, 0, 5)).byLine shouldBe Map(
      0 -> List(token(0, 0, 3), token(0, 4, 1)),
      2 -> List(token(2, 0, 5))
    )
  }

  it should "be equal to another holding the same tokens, however it was packed" in {
    val decoded = SemanticTokenData.decode(IArray(0, 0, 3, 1, 0), legend)

    decoded shouldBe data(token(0, 0, 3, "string"))
    decoded.hashCode shouldBe data(token(0, 0, 3, "string")).hashCode
    decoded should not be data(token(0, 0, 3, "keyword"))
  }
