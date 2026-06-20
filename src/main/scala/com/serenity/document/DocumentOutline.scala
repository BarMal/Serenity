package com.serenity.document

import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.Buffer
import com.serenity.ui.layout.{Location, Symbol, SymbolKind}

object DocumentOutline:

  private val MarkdownHeading               = """^(#{1,6})\s+(.+)$""".r
  private val MaxPlainTextSectionNameLength = 54

  def forBuffer(buffer: Buffer): List[Symbol] =
    buffer.language match
      case Some(LanguageId.Markdown) =>
        markdownHeadings(buffer.content.collect())
      case None =>
        plainTextSections(buffer.content.collect())
      case _ =>
        Nil

  private def markdownHeadings(content: String): List[Symbol] =
    content.linesIterator.zipWithIndex.collect {
      case (MarkdownHeading(_, title), line) =>
        Symbol(
          name = title.trim,
          kind = SymbolKind.Heading,
          location = Location(line, 0)
        )
    }.toList

  private def plainTextSections(content: String): List[Symbol] =
    val sections = content.linesIterator.zipWithIndex
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
