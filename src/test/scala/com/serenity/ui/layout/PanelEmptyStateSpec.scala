package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A list panel with nothing to list says so, muted and unselectable, rather than rendering blank. */
class PanelEmptyStateSpec extends AnyFlatSpec with Matchers:

  private val vertical   = LayoutRect(0, 0, 20, 40)
  private val horizontal = LayoutRect(0, 0, 80, 6)

  private def muted(row: OverlayRow): Boolean = row.segments.map(_.tone) == List(OverlayTone.Muted)

  "An empty outline" should "say the document has no headings" in {
    for rect <- List(vertical, horizontal) do
      val rows = PanelContentResolver.outlineRowViews(rect, Nil, None)
      rows.map(_.row.plainText) shouldBe List("No headings in this document")
      rows.map(_.symbolIndex) shouldBe List(None)
      rows.forall(view => muted(view.row)) shouldBe true
  }

  "An empty comments panel" should "say the document has no comments" in {
    for rect <- List(vertical, horizontal) do
      val rows = PanelContentResolver.commentsRowViews(rect, Nil, None)
      rows.map(_.row.plainText) shouldBe List("No comments in this document")
      rows.map(_.symbolIndex) shouldBe List(None)
  }

  "An empty diagnostics panel" should "say the document has no problems" in {
    val rows = PanelContentResolver.diagnosticsRowViews(vertical, Nil, None)

    rows.map(_.row.plainText) shouldBe List("No problems in this document")
    rows.map(_.issueIndex) shouldBe List(None)
    rows.forall(view => muted(view.row)) shouldBe true
  }
end PanelEmptyStateSpec
