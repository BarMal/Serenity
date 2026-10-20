package com.serenity.document

import com.serenity.lsp.config.LanguageId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommentSyntaxSpec extends AnyFlatSpec with Matchers:

  "CommentSyntax" should "give C-family languages // and /* */" in {
    val cStyle = CommentSyntax(Some("//"), Some(BlockCommentDelimiters("/*", "*/")))

    List(
      LanguageId.Scala,
      LanguageId.Java,
      LanguageId.JavaScript,
      LanguageId.TypeScript,
      LanguageId.Go,
      LanguageId.Rust,
      LanguageId.Kotlin,
      LanguageId.CSharp,
      LanguageId.Cpp,
      LanguageId.C
    ).foreach(language => CommentSyntax.of(language) shouldBe cStyle)
  }

  it should "give scripting and config languages # and no block comment" in
    List(LanguageId.Python, LanguageId.Ruby, LanguageId.Toml, LanguageId.Yaml)
      .foreach(language => CommentSyntax.of(language) shouldBe CommentSyntax(Some("#"), None))

  it should "give markup languages only <!-- --> and CSS only /* */" in {
    val markup = CommentSyntax(None, Some(BlockCommentDelimiters("<!--", "-->")))

    List(LanguageId.Markdown, LanguageId.Html, LanguageId.Xml).foreach(language =>
      CommentSyntax.of(language) shouldBe markup
    )
    CommentSyntax.of(LanguageId.Css) shouldBe CommentSyntax(None, Some(BlockCommentDelimiters("/*", "*/")))
  }

  it should "give the dash-comment languages their own block delimiters" in {
    CommentSyntax.of(LanguageId.Haskell) shouldBe CommentSyntax(Some("--"), Some(BlockCommentDelimiters("{-", "-}")))
    CommentSyntax.of(LanguageId.Sql) shouldBe CommentSyntax(Some("--"), Some(BlockCommentDelimiters("/*", "*/")))
    CommentSyntax.of(LanguageId.Lua) shouldBe CommentSyntax(Some("--"), Some(BlockCommentDelimiters("--[[", "]]")))
  }

  it should "give JSON no comments" in {
    CommentSyntax.of(LanguageId.JsonLang) shouldBe CommentSyntax(None, None)
  }
