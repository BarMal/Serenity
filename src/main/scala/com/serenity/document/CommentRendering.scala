package com.serenity.document

import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.state.models.Buffer

case class RenderedComment(
    sourceLine: Int,
    raw: String,
    inlineMarkdown: String
)

object CommentRendering:

  def atCursor(buffer: Buffer): Option[RenderedComment] =
    for
      cursor <- buffer.cursors.headOption
      line   <- buffer.content.getLine(cursor.line)
      body   <- commentBody(line, buffer.language)
    yield RenderedComment(cursor.line, line.trim, renderInlineComment(body))

  private def commentBody(line: String, language: Option[LanguageId]): Option[String] =
    val trimmed = line.trim
    language match
      case Some(LanguageId.Markdown | LanguageId.Html | LanguageId.Xml) =>
        htmlCommentBody(trimmed)
      case Some(LanguageId.Python | LanguageId.Ruby | LanguageId.Toml | LanguageId.Yaml) =>
        prefixedCommentBody(trimmed, "#")
      case Some(LanguageId.Haskell | LanguageId.Sql) =>
        prefixedCommentBody(trimmed, "--")
      case Some(LanguageId.Lua) =>
        prefixedCommentBody(trimmed, "--").orElse(blockCommentBody(trimmed, "--[[", "]]"))
      case Some(LanguageId.Css) =>
        blockCommentBody(trimmed, "/*", "*/")
      case Some(_) =>
        prefixedCommentBody(trimmed, "//")
          .orElse(blockCommentBody(trimmed, "/*", "*/"))
          .orElse(prefixedCommentBody(trimmed, "*"))
      case None =>
        None

  private def prefixedCommentBody(line: String, prefix: String): Option[String] =
    Option.when(line.startsWith(prefix))(line.drop(prefix.length).trim)

  private def blockCommentBody(line: String, start: String, end: String): Option[String] =
    Option
      .when(line.startsWith(start) && line.endsWith(end)) {
        line.drop(start.length).dropRight(end.length).trim
      }

  private def htmlCommentBody(line: String): Option[String] =
    blockCommentBody(line, "<!--", "-->")

  private def renderInlineComment(body: String): String =
    stripInlineMarkers(MarkdownDocumentPreview.renderInlineLine(body)).trim

  private def stripInlineMarkers(text: String): String =
    text
      .replaceAll("""!\[([^\]]*)\]\([^)]+\)""", "$1")
      .replaceAll("""\[([^\]]+)\]\([^)]+\)""", "$1")
      .replaceAll("""\*\*([^*]+)\*\*""", "$1")
      .replaceAll("""__([^_]+)__""", "$1")
      .replaceAll("""\*([^*]+)\*""", "$1")
      .replaceAll("""_([^_]+)_""", "$1")
      .replaceAll("""`([^`]+)`""", "$1")

end CommentRendering
