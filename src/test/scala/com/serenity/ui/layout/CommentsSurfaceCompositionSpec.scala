package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `CommentsSurfaceComposition` (issue #819, slice 5): the pinned/expanded comments panel resolved into
  * one paint/hit-test plan, reusing `PanelContentResolver.commentsRowViews`'s row text so painting and mouse
  * hit-testing can never disagree about which comment a row represents.
  */
class CommentsSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private val symbols = List(
    Symbol("Comment: Revise opening", SymbolKind.Comment, Location(2, 0)),
    Symbol("Comment: Tighten pacing", SymbolKind.Comment, Location(15, 4)),
    Symbol("Comment: Cut redundancy", SymbolKind.Comment, Location(30, 1))
  )

  "forComments" should "paint one text box per row, matching PanelContentResolver's row text" in {
    val frameRect    = LayoutRect(0, 0, 18, 40)
    val expectedRows = PanelContentResolver.commentsRowViews(frameRect, symbols, activeLocation = None)

    val resolved = CommentsSurfaceComposition.forComments(symbols, activeLocation = None, frameRect)

    resolved.paintBoxes.map(_.text) shouldBe expectedRows.map(view => Some(view.row.plainText))
  }

  it should "resolve hitAt to the exact comment clicked in vertical geometry" in {
    val frameRect = LayoutRect(0, 0, 18, 40)

    val resolved = CommentsSurfaceComposition.forComments(symbols, activeLocation = None, frameRect)

    val secondBox = resolved.paintBoxes.find(_.text.exists(_.contains("Tighten pacing"))).getOrElse(fail("row"))
    val hit       = resolved.hitAt(secondBox.rect.x, secondBox.rect.y)

    hit.flatMap(_.actionId) shouldBe Some(SurfaceActionId("comments-symbol-1"))
  }

  it should "resolve hitAt to the exact comment clicked in square geometry" in {
    val frameRect = LayoutRect(0, 0, 24, 20)

    val resolved = CommentsSurfaceComposition.forComments(symbols, activeLocation = None, frameRect)

    val firstBox = resolved.paintBoxes.find(_.text.exists(_.contains("Revise opening"))).getOrElse(fail("row"))
    val hit      = resolved.hitAt(firstBox.rect.x, firstBox.rect.y)

    hit.flatMap(_.actionId) shouldBe Some(SurfaceActionId("comments-symbol-0"))
  }

  it should "emit no hit regions for the horizontal summary row" in {
    val frameRect = LayoutRect(0, 0, 60, 10)

    val resolved = CommentsSurfaceComposition.forComments(symbols, activeLocation = None, frameRect)

    resolved.hitRegions shouldBe Nil
  }

  it should "emit no hit regions for the compact summary rows" in {
    val frameRect = LayoutRect(0, 0, 14, 4)

    val resolved = CommentsSurfaceComposition.forComments(symbols, Some(Location(15, 4)), frameRect)

    resolved.hitRegions shouldBe Nil
  }

  it should "mark the active comment's row as selected" in {
    val frameRect = LayoutRect(0, 0, 18, 40)

    val resolved = CommentsSurfaceComposition.forComments(symbols, Some(Location(15, 4)), frameRect)

    resolved.paintBoxes.map(_.selected) shouldBe List(false, true, false)
  }
end CommentsSurfaceCompositionSpec
