package com.serenity.document

import com.serenity.lsp.config.LanguageId

final case class BlockCommentDelimiters(open: String, close: String)

/** How a language writes comments. Either part may be absent: Python has no block comment, CSS and HTML have no line
  * comment, JSON has neither.
  */
final case class CommentSyntax(line: Option[String], block: Option[BlockCommentDelimiters])

object CommentSyntax:

  private def block(open: String, close: String): Option[BlockCommentDelimiters] =
    Some(BlockCommentDelimiters(open, close))

  private val cStyle   = CommentSyntax(Some("//"), block("/*", "*/"))
  private val hashOnly = CommentSyntax(Some("#"), None)
  private val markup   = CommentSyntax(None, block("<!--", "-->"))

  /** Exhaustive, so a language added to [[LanguageId]] has to say how it comments. */
  def of(language: LanguageId): CommentSyntax =
    language match
      case LanguageId.Scala | LanguageId.Java | LanguageId.JavaScript | LanguageId.TypeScript | LanguageId.Go |
          LanguageId.Rust | LanguageId.Kotlin | LanguageId.CSharp | LanguageId.Cpp | LanguageId.C =>
        cStyle
      case LanguageId.Python | LanguageId.Ruby | LanguageId.Toml | LanguageId.Yaml =>
        hashOnly
      case LanguageId.Haskell => CommentSyntax(Some("--"), block("{-", "-}"))
      case LanguageId.Sql     => CommentSyntax(Some("--"), block("/*", "*/"))
      case LanguageId.Lua     => CommentSyntax(Some("--"), block("--[[", "]]"))
      case LanguageId.Css     => CommentSyntax(None, block("/*", "*/"))
      case LanguageId.Markdown | LanguageId.Html | LanguageId.Xml =>
        markup
      case LanguageId.JsonLang =>
        CommentSyntax(None, None)
