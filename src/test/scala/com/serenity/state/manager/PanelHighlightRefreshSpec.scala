package com.serenity.state.manager

import com.serenity.state.models.SurfaceContent
import com.serenity.ui.layout.{Diagnostic, DiagnosticSeverity, Location, Symbol, SymbolKind}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A list panel rebuilt from new content keeps the row it had highlighted, as long as that row is still there. */
class PanelHighlightRefreshSpec extends AnyFlatSpec with Matchers:

  private def heading(line: Int) = Symbol(s"Heading $line", SymbolKind.Heading, Location(line, 0))
  private def issue(line: Int)   = Diagnostic(s"issue $line", DiagnosticSeverity.Error, Location(line, 0))

  "A refreshed list" should "keep its highlight on a row that is still listed" in {
    PanelContentSync.withHighlightKept(
      SurfaceContent.Outline(List(heading(1), heading(5)), Some(Location(5, 0))),
      SurfaceContent.Outline(List(heading(1), heading(5), heading(9)))
    ) shouldBe SurfaceContent.Outline(List(heading(1), heading(5), heading(9)), Some(Location(5, 0)))

    PanelContentSync.withHighlightKept(
      SurfaceContent.Diagnostics(List(issue(2), issue(4)), Some(Location(4, 0))),
      SurfaceContent.Diagnostics(List(issue(4)))
    ) shouldBe SurfaceContent.Diagnostics(List(issue(4)), Some(Location(4, 0)))
  }

  it should "drop the highlight when that row has gone" in {
    PanelContentSync.withHighlightKept(
      SurfaceContent.Comments(List(heading(1), heading(5)), Some(Location(5, 0))),
      SurfaceContent.Comments(List(heading(1)))
    ) shouldBe SurfaceContent.Comments(List(heading(1)), None)
  }
