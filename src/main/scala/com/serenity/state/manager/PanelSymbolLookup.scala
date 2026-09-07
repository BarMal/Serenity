package com.serenity.state.manager

import com.serenity.document.{DocumentNavigation, DocumentOutline}
import com.serenity.state.models.*
import com.serenity.ui.layout.{Location, Symbol}

/** Pure symbol-list lookups shared by panel population (outline/comments panels), UI preset restoration, and
  * symbol navigation -- kept dependency-free so none of those concerns need an IO port just to read them.
  */
private[manager] object PanelSymbolLookup:

  def outlineSymbols(state: AppState): List[Symbol] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .map(outlineSymbolsForBuffer)
      .getOrElse(Nil)

  def outlineSymbolsForBuffer(buffer: Buffer): List[Symbol] =
    (
      DocumentOutline.forBuffer(buffer) ++
        DocumentNavigation.bookmarkSymbols(buffer.annotations.bookmarks)
    )
      .sortBy(symbol => (symbol.location.line, symbol.location.column, symbol.name))

  def commentPanelSymbols(state: AppState): List[Symbol] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .map(buffer => DocumentNavigation.commentSymbols(buffer.annotations.documentComments))
      .getOrElse(Nil)

  def currentSymbolActiveLocation(symbols: List[Symbol], state: AppState): Option[Location] =
    state.activeCursorPosition
      .flatMap(cursor => DocumentNavigation.currentSymbol(symbols, cursor))
      .map(_.location)
