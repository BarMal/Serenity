package com.serenity.document

import com.serenity.state.models.CursorPosition
import com.serenity.ui.layout.Symbol

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

  private def sortedSymbols(symbols: List[Symbol]): List[Symbol] =
    symbols.sortBy(symbol => (symbol.location.line, symbol.location.column))

  private def isAfter(symbol: Symbol, cursor: CursorPosition): Boolean =
    symbol.location.line > cursor.line ||
      (symbol.location.line == cursor.line && symbol.location.column > cursor.column)

  private def isBefore(symbol: Symbol, cursor: CursorPosition): Boolean =
    symbol.location.line < cursor.line ||
      (symbol.location.line == cursor.line && symbol.location.column < cursor.column)
