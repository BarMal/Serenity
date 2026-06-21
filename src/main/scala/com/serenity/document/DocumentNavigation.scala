package com.serenity.document

import com.serenity.state.models.{CursorPosition, DocumentComment}
import com.serenity.ui.layout.{Location, Symbol, SymbolKind}

object DocumentNavigation:

  def nextSymbol(symbols: List[Symbol], cursor: CursorPosition): Option[Symbol] =
    sortedSymbols(symbols) match
      case Nil => None
      case sorted =>
        sorted.find(symbol => isAfter(symbol, cursor)).orElse(sorted.headOption)

  def previousSymbol(symbols: List[Symbol], cursor: CursorPosition): Option[Symbol] =
    sortedSymbols(symbols) match
      case Nil => None
      case sorted =>
        sorted.reverse.find(symbol => isBefore(symbol, cursor)).orElse(sorted.lastOption)

  def currentSymbol(symbols: List[Symbol], cursor: CursorPosition): Option[Symbol] =
    sortedSymbols(symbols).reverse.find(symbol => isAtOrBefore(symbol, cursor))

  def bookmarkSymbols(bookmarks: List[CursorPosition]): List[Symbol] =
    sortedPositions(bookmarks).map { cursor =>
      Symbol(
        name = s"Bookmark ${cursor.line + 1}:${cursor.column + 1}",
        kind = SymbolKind.Bookmark,
        location = Location(cursor.line, cursor.column)
      )
    }

  def commentSymbols(comments: List[DocumentComment]): List[Symbol] =
    comments
      .sortBy(comment => (comment.start.line, comment.start.column, comment.text))
      .map { comment =>
        Symbol(
          name = s"Comment: ${commentTitle(comment.text)}",
          kind = SymbolKind.Comment,
          location = Location(comment.start.line, comment.start.column)
        )
      }

  private def sortedSymbols(symbols: List[Symbol]): List[Symbol] =
    symbols.sortBy(symbol => (symbol.location.line, symbol.location.column))

  private def sortedPositions(bookmarks: List[CursorPosition]): List[CursorPosition] =
    bookmarks.distinct.sortBy(cursor => (cursor.line, cursor.column))

  private def commentTitle(text: String): String =
    text.linesIterator.find(_.trim.nonEmpty).map(_.trim).getOrElse("Untitled")

  private def isAfter(symbol: Symbol, cursor: CursorPosition): Boolean =
    symbol.location.line > cursor.line ||
      (symbol.location.line == cursor.line && symbol.location.column > cursor.column)

  private def isBefore(symbol: Symbol, cursor: CursorPosition): Boolean =
    symbol.location.line < cursor.line ||
      (symbol.location.line == cursor.line && symbol.location.column < cursor.column)

  private def isAtOrBefore(symbol: Symbol, cursor: CursorPosition): Boolean =
    symbol.location.line < cursor.line ||
      (symbol.location.line == cursor.line && symbol.location.column <= cursor.column)
