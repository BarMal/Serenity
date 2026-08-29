package com.serenity.document

import scala.jdk.CollectionConverters.*

import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{ParagraphRole, RichTextDocument}
import com.serenity.state.models.Buffer
import com.serenity.ui.layout.{Location, Symbol, SymbolKind}
import org.commonmark.Extension
import org.commonmark.ext.gfm.tables.TablesExtension
import org.commonmark.ext.task.list.items.TaskListItemsExtension
import org.commonmark.node.{Heading, Node}
import org.commonmark.parser.{IncludeSourceSpans, Parser}
import org.commonmark.renderer.text.TextContentRenderer

object DocumentOutline:

  private val MaxPlainTextSectionNameLength = 54

  /** Same extension set as [[com.serenity.markdown.MarkdownDocumentPreview]], so heading extraction agrees with what
    * the Markdown preview itself parses.
    */
  private val extensions: java.util.List[Extension] =
    List[Extension](TablesExtension.create(), TaskListItemsExtension.create()).asJava

  private val markdownParser: Parser =
    Parser.builder().extensions(extensions).includeSourceSpans(IncludeSourceSpans.BLOCKS).build()

  private val headingTextRenderer: TextContentRenderer =
    TextContentRenderer.builder().build()

  def forBuffer(buffer: Buffer): List[Symbol] =
    richTextHeadings(
      buffer.richText.richTextDocument.filter(
        _.matchesPlainTextShape(buffer.document.content.lineCount, buffer.document.content.weight)
      )
    )
      .filter(_.nonEmpty) match
      case Some(symbols) => symbols
      case None =>
        buffer.document.language match
          case Some(LanguageId.Markdown) =>
            markdownHeadings(buffer)
          case None =>
            plainTextSections(buffer)
          case _ =>
            Nil

  private def richTextHeadings(document: Option[RichTextDocument]): Option[List[Symbol]] =
    document.map(_.paragraphs.zipWithIndex.collect {
      case (paragraph, line) if isHeading(paragraph.role) && paragraph.plainText.trim.nonEmpty =>
        Symbol(
          name = paragraph.plainText.trim,
          kind = SymbolKind.Heading,
          location = Location(line, 0)
        )
    })

  private def isHeading(role: ParagraphRole): Boolean =
    role match
      case ParagraphRole.Heading(_) => true
      case ParagraphRole.Body       => false

  /** Walks the commonmark AST for `Heading` nodes rather than regex-scanning raw lines, so a `#`-prefixed line inside a
    * fenced code block is never mistaken for a heading.
    */
  private def markdownHeadings(buffer: Buffer): List[Symbol] =
    val source   = bufferLines(buffer).map(_._1).mkString("\n")
    val document = markdownParser.parse(source)
    headingNodes(document).flatMap { heading =>
      val title = headingTextRenderer.render(heading).trim
      heading.getSourceSpans.asScala.headOption
        .filter(_ => title.nonEmpty)
        .map(span => Symbol(name = title, kind = SymbolKind.Heading, location = Location(span.getLineIndex, 0)))
    }

  private def headingNodes(node: Node): List[Heading] =
    children(node).flatMap {
      case heading: Heading => List(heading)
      case other            => headingNodes(other)
    }

  private def children(node: Node): List[Node] =
    @annotation.tailrec
    def loop(current: Node, acc: List[Node]): List[Node] =
      if current == null then acc.reverse else loop(current.getNext, current :: acc)
    loop(node.getFirstChild, Nil)

  private def plainTextSections(buffer: Buffer): List[Symbol] =
    val sections = bufferLines(buffer)
      .foldLeft((List.empty[(String, Int)], true)) {
        case ((acc, sectionStart), (line, index)) =>
          val title = line.trim
          if title.isEmpty then (acc, true)
          else if sectionStart then ((title, index) :: acc, false)
          else (acc, false)
      }
      ._1
      .reverse

    if sections.size < 2 then Nil
    else
      sections.map {
        case (title, line) =>
          Symbol(
            name = clippedSectionName(title),
            kind = SymbolKind.Section,
            location = Location(line, 0)
          )
      }

  private def clippedSectionName(title: String): String =
    if title.length <= MaxPlainTextSectionNameLength then title
    else title.take(MaxPlainTextSectionNameLength - 3) + "..."

  private def bufferLines(buffer: Buffer): Iterator[(String, Int)] =
    buffer.document.content.linesIteratorFrom(0).map { case (line, text) => text -> line }
