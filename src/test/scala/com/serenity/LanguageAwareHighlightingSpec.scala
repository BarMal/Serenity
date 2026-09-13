package com.serenity

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.SemanticToken
import com.serenity.ui.theme.{SyntaxElement, Theme, ThemeManager}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Contract tests for issue #859/#1177: LSP `textDocument/semanticTokens` is the only syntax-coloring mechanism for any
  * declared language (the handwritten, Scala-shaped regex tokenizer this replaced is gone -- see
  * `LspManagerSemanticTokensRenderingSpec`/`LspProtocolSpec` for the request/decode side). A line renders with real
  * per-language tokens when they've been supplied, a visibly distinct "unavailable" style when the document has none
  * yet, and unstyled plain text only when no language is declared at all.
  */
class LanguageAwareHighlightingSpec extends AnyFlatSpec with Matchers:

  private val theme = Theme.dark

  private def token(startCharacter: Int, length: Int, tokenType: String): SemanticToken =
    SemanticToken(
      line = 0,
      startCharacter = startCharacter,
      length = length,
      tokenType = tokenType,
      tokenModifiers = Set.empty
    )

  "highlightLine" should "color a JavaScript identifier by its real semantic token type, not a Scala keyword table" in {
    // `trait` is a plain identifier in JavaScript -- no Scala-shaped rule should treat it as a keyword, and the only
    // way it gets a Keyword color now is a server literally classifying it as `keyword`.
    val tokens = List(token(0, 5, "keyword"), token(6, 5, "variable"))
    val styled = ThemeManager.highlightLine("const trait = 1;", theme, Some(LanguageId.JavaScript), Some(tokens))

    styled.map(_.content).mkString shouldBe "const trait = 1;"
    styled.exists(s => s.content == "const" && s.style == theme.colorFor(SyntaxElement.Keyword).style) shouldBe true
    styled.exists(s => s.content == "trait" && s.style == theme.colorFor(SyntaxElement.Identifier).style) shouldBe true
    styled.exists(s => s.content == "trait" && s.style == theme.colorFor(SyntaxElement.Keyword).style) shouldBe false
  }

  it should "render a visibly distinct 'unavailable' style when a declared language has no semantic tokens" in {
    val styled =
      ThemeManager.highlightLine("def foo(): return True", theme, Some(LanguageId.Python), semanticTokens = None)

    styled shouldBe List(
      com.serenity.ui.theme.StyledText(
        "def foo(): return True",
        com.serenity.ui.theme.TextStyle.italic,
        theme.muted,
        theme.background
      )
    )
  }

  it should "render undeclared-language lines as plain text, distinct from the 'unavailable' style" in {
    val styled = ThemeManager.highlightLine("some text", theme, language = None)

    styled shouldBe List(
      com.serenity.ui.theme
        .StyledText("some text", com.serenity.ui.theme.TextStyle.normal, theme.foreground, theme.background)
    )
  }

  it should "still color Scala via semantic tokens rather than any special-cased handwritten path" in {
    val tokens = List(token(0, 3, "keyword"))
    val styled = ThemeManager.highlightLine("val x = 1", theme, Some(LanguageId.Scala), Some(tokens))

    styled.exists(s => s.content == "val" && s.style == theme.colorFor(SyntaxElement.Keyword).style) shouldBe true
  }

  it should "show the unavailable style for Scala too when it has no semantic tokens" in {
    val styled = ThemeManager.highlightLine("val x = 1", theme, Some(LanguageId.Scala), semanticTokens = None)

    styled shouldBe List(
      com.serenity.ui.theme
        .StyledText("val x = 1", com.serenity.ui.theme.TextStyle.italic, theme.muted, theme.background)
    )
  }

  it should "fill the gap between two tokens, and after the last one, with Normal-styled text" in {
    val tokens = List(token(0, 3, "keyword"), token(8, 3, "string"))
    val styled = ThemeManager.highlightLine("val x = \"y\" !", theme, Some(LanguageId.Scala), Some(tokens))

    styled.map(_.content) shouldBe List("val", " x = ", "\"y\"", " !")
    styled(1).style shouldBe theme.colorFor(SyntaxElement.Normal).style
    styled(3).style shouldBe theme.colorFor(SyntaxElement.Normal).style
  }

  it should "clamp a token whose span runs past the end of the line rather than throw" in {
    val tokens = List(token(4, 100, "string"))
    val styled = ThemeManager.highlightLine("val \"unterminated", theme, Some(LanguageId.Scala), Some(tokens))

    styled.map(_.content).mkString shouldBe "val \"unterminated"
    styled.last.content shouldBe "\"unterminated"
    styled.last.style shouldBe theme.colorFor(SyntaxElement.String).style
  }

  "SyntaxElement.fromLspTokenType" should "map LSP semantic token types onto this theme's coarser vocabulary" in {
    SyntaxElement.fromLspTokenType("keyword") shouldBe SyntaxElement.Keyword
    SyntaxElement.fromLspTokenType("modifier") shouldBe SyntaxElement.Keyword
    SyntaxElement.fromLspTokenType("string") shouldBe SyntaxElement.String
    SyntaxElement.fromLspTokenType("regexp") shouldBe SyntaxElement.String
    SyntaxElement.fromLspTokenType("comment") shouldBe SyntaxElement.Comment
    SyntaxElement.fromLspTokenType("number") shouldBe SyntaxElement.Number
    SyntaxElement.fromLspTokenType("operator") shouldBe SyntaxElement.Operator
    SyntaxElement.fromLspTokenType("class") shouldBe SyntaxElement.Type
    SyntaxElement.fromLspTokenType("typeParameter") shouldBe SyntaxElement.Type
    SyntaxElement.fromLspTokenType("variable") shouldBe SyntaxElement.Identifier
    SyntaxElement.fromLspTokenType("function") shouldBe SyntaxElement.Identifier
    SyntaxElement.fromLspTokenType("some-unrecognized-future-type") shouldBe SyntaxElement.Normal
  }

  "LanguageId.fromString" should "recognize the languages covered by these contract tests" in {
    LanguageId.fromString("python") shouldBe Some(LanguageId.Python)
    LanguageId.fromString("javascript") shouldBe Some(LanguageId.JavaScript)
    LanguageId.fromString("typescript") shouldBe Some(LanguageId.TypeScript)
    LanguageId.fromString("json") shouldBe Some(LanguageId.JsonLang)
    LanguageId.fromString("scala") shouldBe Some(LanguageId.Scala)
  }
