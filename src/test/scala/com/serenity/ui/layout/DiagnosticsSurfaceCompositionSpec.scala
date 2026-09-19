package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `DiagnosticsSurfaceComposition` (issue #819, slice 4): the pinned/expanded diagnostics panel resolved
  * into one paint/hit-test plan, reusing `PanelContentResolver.diagnosticsRowViews`'s row text so painting and mouse
  * hit-testing can never disagree about which issue a row represents.
  */
class DiagnosticsSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private val issues = List(
    Diagnostic("unused import", DiagnosticSeverity.Warning, Location(0, 1)),
    Diagnostic("type mismatch", DiagnosticSeverity.Error, Location(2, 3))
  )

  "forDiagnostics" should "paint one text box per row, matching PanelContentResolver's row text" in {
    val frameRect    = LayoutRect(0, 0, 18, 40)
    val expectedRows = PanelContentResolver.diagnosticsRowViews(frameRect, issues, activeLocation = None)

    val resolved = DiagnosticsSurfaceComposition.forDiagnostics(issues, activeLocation = None, frameRect)

    resolved.paintBoxes.map(_.text) shouldBe expectedRows.map(view => Some(view.row.plainText))
  }

  it should "resolve hitAt to the exact issue clicked in vertical geometry" in {
    val frameRect = LayoutRect(0, 0, 18, 40)

    val resolved = DiagnosticsSurfaceComposition.forDiagnostics(issues, activeLocation = None, frameRect)

    val secondBox = resolved.paintBoxes.find(_.text.exists(_.contains("type mismatch"))).getOrElse(fail("row"))
    val hit       = resolved.hitAt(secondBox.rect.x, secondBox.rect.y)

    hit.flatMap(_.actionId) shouldBe Some(SurfaceActionId("diagnostics-issue-1"))
  }

  it should "resolve hitAt to the exact issue clicked in square geometry, skipping the leading summary row" in {
    val frameRect = LayoutRect(0, 0, 24, 20)

    val resolved = DiagnosticsSurfaceComposition.forDiagnostics(issues, activeLocation = None, frameRect)

    val firstIssueBox =
      resolved.paintBoxes.find(_.text.exists(_.contains("unused import"))).getOrElse(fail("row"))
    val hit = resolved.hitAt(firstIssueBox.rect.x, firstIssueBox.rect.y)

    hit.flatMap(_.actionId) shouldBe Some(SurfaceActionId("diagnostics-issue-0"))
  }

  it should "emit no hit region for the square geometry's leading summary row" in {
    val frameRect = LayoutRect(0, 0, 24, 20)

    val resolved = DiagnosticsSurfaceComposition.forDiagnostics(issues, activeLocation = None, frameRect)

    val summaryBox = resolved.paintBoxes.head
    resolved.hitAt(summaryBox.rect.x, summaryBox.rect.y) shouldBe None
  }

  it should "emit no hit regions for horizontal or compact geometry" in {
    val horizontal = DiagnosticsSurfaceComposition.forDiagnostics(issues, None, LayoutRect(0, 0, 60, 10))
    val compact    = DiagnosticsSurfaceComposition.forDiagnostics(issues, None, LayoutRect(0, 0, 14, 4))

    horizontal.hitRegions shouldBe Nil
    compact.hitRegions shouldBe Nil
  }

  it should "mark the active issue's row as selected" in {
    val frameRect = LayoutRect(0, 0, 18, 40)

    val resolved = DiagnosticsSurfaceComposition.forDiagnostics(issues, Some(Location(2, 3)), frameRect)

    resolved.paintBoxes.map(_.selected) shouldBe List(false, true)
  }
end DiagnosticsSurfaceCompositionSpec
