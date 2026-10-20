package com.serenity.document

import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.RichTextDocument
import com.serenity.state.models.{Buffer, HeadingIdentity}
import com.serenity.ui.layout.{Location, Symbol, SymbolKind}

object DocumentOutline:

  private val MaxPlainTextSectionNameLength = 54

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

  /** The headings of `buffer` with their chapter-note identity. A Markdown buffer answers from its heading index, which
    * has normalised each title already.
    */
  def identifiedHeadings(buffer: Buffer): List[(HeadingIdentity, Symbol)] =
    val rich = richTextHeadings(
      buffer.richText.richTextDocument.filter(
        _.matchesPlainTextShape(buffer.document.content.lineCount, buffer.document.content.weight)
      )
    ).filter(_.nonEmpty)
    if rich.isEmpty && buffer.document.language.contains(LanguageId.Markdown) then
      buffer.document.markdownHeadings.identified
    else HeadingIdentity.forHeadings(forBuffer(buffer))

  private def richTextHeadings(document: Option[RichTextDocument]): Option[List[Symbol]] =
    document.map(_.headingParagraphs.map {
      case (line, paragraph) =>
        Symbol(
          name = paragraph.plainText.trim,
          kind = SymbolKind.Heading,
          location = Location(line, 0)
        )
    })

  private def markdownHeadings(buffer: Buffer): List[Symbol] =
    buffer.document.markdownHeadings.headings.toList

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
