package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `OutlineSurfaceComposition` (issue #819, slice 4): the pinned/expanded outline panel resolved into one
  * paint/hit-test plan, reusing `PanelContentResolver.outlineRowViews`'s row text so painting and mouse hit-testing can
  * never disagree about which symbol a row represents.
  */
class OutlineSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private val symbols = List(
    Symbol("Serenity", SymbolKind.Class, Location(1, 1)),
    Symbol("render", SymbolKind.Method, Location(10, 3)),
    Symbol("state", SymbolKind.Variable, Location(20, 5))
  )

  "forOutline" should "paint one text box per row, matching PanelContentResolver's row text" in {
    val frameRect    = LayoutRect(0, 0, 18, 40)
    val expectedRows = PanelContentResolver.outlineRowViews(frameRect, symbols, activeLocation = None)

    val resolved = OutlineSurfaceComposition.forOutline(symbols, activeLocation = None, frameRect)

    resolved.paintBoxes.map(_.text) shouldBe expectedRows.map(view => Some(view.row.plainText))
  }

  it should "resolve hitAt to the exact symbol clicked in vertical/square geometry" in {
    val frameRect = LayoutRect(0, 0, 18, 40)

    val resolved = OutlineSurfaceComposition.forOutline(symbols, activeLocation = None, frameRect)

    val renderBox = resolved.paintBoxes.find(_.text.exists(_.contains("render"))).getOrElse(fail("expected a row"))
    val hit       = resolved.hitAt(renderBox.rect.x, renderBox.rect.y)

    hit.flatMap(_.actionId) shouldBe Some(SurfaceActionId("outline-symbol-1"))
  }

  it should "emit no hit regions for the horizontal summary row" in {
    val frameRect = LayoutRect(0, 0, 60, 10)

    val resolved = OutlineSurfaceComposition.forOutline(symbols, activeLocation = None, frameRect)

    resolved.hitRegions shouldBe Nil
  }

  it should "emit no hit regions for the compact summary rows" in {
    val frameRect = LayoutRect(0, 0, 14, 4)

    val resolved = OutlineSurfaceComposition.forOutline(symbols, Some(Location(10, 3)), frameRect)

    resolved.hitRegions shouldBe Nil
  }

  it should "mark the active symbol's row as selected" in {
    val frameRect = LayoutRect(0, 0, 18, 40)

    val resolved = OutlineSurfaceComposition.forOutline(symbols, Some(Location(10, 3)), frameRect)

    resolved.paintBoxes.map(_.selected) shouldBe List(false, true, false)
  }
end OutlineSurfaceCompositionSpec
