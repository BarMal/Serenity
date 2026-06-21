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
      cursor  <- buffer.cursors.headOption
      comment <- authoredCommentAt(buffer, cursor).orElse(commentAtLine(buffer, cursor.line))
    yield comment

  private def authoredCommentAt(
    buffer: Buffer,
    cursor: com.serenity.state.models.CursorPosition
  ): Option[RenderedComment] =
    buffer.documentComments
      .find(_.contains(cursor))
      .map { comment =>
        val lines = comment.text.linesIterator.toVector
        RenderedComment(
          sourceLine = comment.start.line,
          raw = comment.text,
          inlineMarkdown = renderInlineCommentLines(lines).mkString("\n")
        )
      }

  private def commentAtLine(buffer: Buffer, lineIndex: Int): Option[RenderedComment] =
    val lines = bufferLines(buffer)
    blockCommentAt(lines, lineIndex, buffer.language).orElse {
      for
        line <- lines.lift(lineIndex)
        body <- lineCommentBody(line, buffer.language)
      yield RenderedComment(lineIndex, line.trim, renderInlineComment(body))
    }

  private def bufferLines(buffer: Buffer): Vector[String] =
    (0 until buffer.content.lineCount).toVector.map(line => buffer.content.getLine(line).getOrElse(""))

  private def lineCommentBody(line: String, language: Option[LanguageId]): Option[String] =
    val trimmed = line.trim
    language match
      case Some(LanguageId.Markdown | LanguageId.Html | LanguageId.Xml) =>
        blockCommentBody(trimmed, BlockCommentSyntax("<!--", "-->"))
      case Some(LanguageId.Python | LanguageId.Ruby | LanguageId.Toml | LanguageId.Yaml) =>
        prefixedCommentBody(trimmed, "#")
      case Some(LanguageId.Haskell | LanguageId.Sql) =>
        prefixedCommentBody(trimmed, "--")
      case Some(LanguageId.Lua) =>
        prefixedCommentBody(trimmed, "--").orElse(blockCommentBody(trimmed, BlockCommentSyntax("--[[", "]]")))
      case Some(LanguageId.Css) =>
        blockCommentBody(trimmed, BlockCommentSyntax("/*", "*/", stripLeadingStars = true))
      case Some(_) =>
        prefixedCommentBody(trimmed, "//")
          .orElse(blockCommentBody(trimmed, BlockCommentSyntax("/*", "*/", stripLeadingStars = true)))
          .orElse(prefixedCommentBody(trimmed, "*"))
      case None =>
        None

  private case class BlockCommentSyntax(start: String, end: String, stripLeadingStars: Boolean = false)

  private def blockCommentAt(
    lines: Vector[String],
    lineIndex: Int,
    language: Option[LanguageId]
  ): Option[RenderedComment] =
    blockCommentSyntaxes(language).view
      .flatMap(syntax => blockCommentRange(lines, lineIndex, syntax).map((syntax, _)))
      .headOption
      .map {
        case (syntax, range) =>
          val rawLines = lines.slice(range.start, range.end + 1).map(_.trim)
          RenderedComment(
            sourceLine = range.start,
            raw = rawLines.mkString("\n"),
            inlineMarkdown = renderInlineCommentLines(blockCommentBodyLines(rawLines, syntax)).mkString("\n")
          )
      }

  private def blockCommentSyntaxes(language: Option[LanguageId]): List[BlockCommentSyntax] =
    language match
      case Some(LanguageId.Markdown | LanguageId.Html | LanguageId.Xml) =>
        List(BlockCommentSyntax("<!--", "-->"))
      case Some(LanguageId.Lua) =>
        List(BlockCommentSyntax("--[[", "]]"))
      case Some(LanguageId.Css) =>
        List(BlockCommentSyntax("/*", "*/", stripLeadingStars = true))
      case Some(
            LanguageId.Python | LanguageId.Ruby | LanguageId.Toml | LanguageId.Yaml | LanguageId.Haskell |
            LanguageId.Sql
          ) =>
        Nil
      case Some(_) =>
        List(BlockCommentSyntax("/*", "*/", stripLeadingStars = true))
      case None =>
        Nil

  private def blockCommentRange(
    lines: Vector[String],
    lineIndex: Int,
    syntax: BlockCommentSyntax
  ): Option[Range.Inclusive] =
    if lineIndex < 0 || lineIndex >= lines.length then None
    else
      for
        start <- (lineIndex to 0 by -1).find(index => lines(index).trim.startsWith(syntax.start))
        end   <- (start until lines.length).find(index => lines(index).trim.endsWith(syntax.end))
        if lineIndex <= end
      yield start to end

  private def prefixedCommentBody(line: String, prefix: String): Option[String] =
    Option.when(line.startsWith(prefix))(line.drop(prefix.length).trim)

  private def blockCommentBody(line: String, syntax: BlockCommentSyntax): Option[String] =
    Option
      .when(line.startsWith(syntax.start) && line.endsWith(syntax.end)) {
        line.drop(syntax.start.length).dropRight(syntax.end.length).trim
      }

  private def blockCommentBodyLines(rawLines: Vector[String], syntax: BlockCommentSyntax): Vector[String] =
    rawLines.zipWithIndex
      .map {
        case (line, index) =>
          val withoutStart =
            if index == 0 then line.drop(syntax.start.length).trim
            else line
          val withoutEnd =
            if index == rawLines.size - 1 then withoutStart.dropRight(syntax.end.length).trim
            else withoutStart
          normalizeBlockBodyLine(withoutEnd, syntax)
      }
      .filter(_.nonEmpty)

  private def normalizeBlockBodyLine(line: String, syntax: BlockCommentSyntax): String =
    val trimmed = line.trim
    if syntax.stripLeadingStars && trimmed.startsWith("*") then trimmed.drop(1).trim
    else trimmed

  private def renderInlineComment(body: String): String =
    stripInlineMarkers(MarkdownDocumentPreview.renderInlineLine(body)).trim

  private def renderInlineCommentLines(lines: Vector[String]): Vector[String] =
    lines.map(renderInlineComment)

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
