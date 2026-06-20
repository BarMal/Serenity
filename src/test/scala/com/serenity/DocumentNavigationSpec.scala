package com.serenity

import com.serenity.document.DocumentNavigation
import com.serenity.state.models.CursorPosition
import com.serenity.ui.layout.{Location, Symbol, SymbolKind}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocumentNavigationSpec extends AnyFlatSpec with Matchers:

  private val symbols = List(
    Symbol("Chapter One", SymbolKind.Heading, Location(0, 0)),
    Symbol("Scene Two", SymbolKind.Heading, Location(4, 0)),
    Symbol("Beat Three", SymbolKind.Heading, Location(8, 0))
  )

  "DocumentNavigation" should "find the next document symbol after the current cursor" in {
    DocumentNavigation.nextSymbol(symbols, CursorPosition(2, 5)) shouldBe Some(symbols(1))
  }

  it should "wrap to the first document symbol when moving next past the final symbol" in {
    DocumentNavigation.nextSymbol(symbols, CursorPosition(8, 0)) shouldBe Some(symbols.head)
  }

  it should "find the previous document symbol before the current cursor" in {
    DocumentNavigation.previousSymbol(symbols, CursorPosition(7, 3)) shouldBe Some(symbols(1))
  }

  it should "wrap to the final document symbol when moving previous before the first symbol" in {
    DocumentNavigation.previousSymbol(symbols, CursorPosition(0, 0)) shouldBe Some(symbols.last)
  }

  it should "find the current document symbol at or before the current cursor" in {
    DocumentNavigation.currentSymbol(symbols, CursorPosition(6, 3)) shouldBe Some(symbols(1))
  }

  it should "leave cursors before the first document symbol without a current symbol" in {
    DocumentNavigation.currentSymbol(symbols, CursorPosition(0, 0)) shouldBe Some(symbols.head)
    DocumentNavigation.currentSymbol(symbols, CursorPosition(0, -1)) shouldBe None
  }

  it should "leave empty symbol lists without a navigation target" in {
    DocumentNavigation.nextSymbol(Nil, CursorPosition(0, 0)) shouldBe None
    DocumentNavigation.previousSymbol(Nil, CursorPosition(0, 0)) shouldBe None
    DocumentNavigation.currentSymbol(Nil, CursorPosition(0, 0)) shouldBe None
  }
end DocumentNavigationSpec
