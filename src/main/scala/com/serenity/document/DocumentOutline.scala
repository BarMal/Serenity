package com.serenity.document

import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.Buffer
import com.serenity.ui.layout.{Location, Symbol, SymbolKind}

object DocumentOutline:

  private val MarkdownHeading = """^(#{1,6})\s+(.+)$""".r

  def forBuffer(buffer: Buffer): List[Symbol] =
    buffer.language match
      case Some(LanguageId.Markdown) =>
        markdownHeadings(buffer.content.collect())
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
