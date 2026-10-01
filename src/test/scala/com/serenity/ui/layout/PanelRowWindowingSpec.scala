package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A panel shows the part of its content the keyboard has moved to: a long list scrolls to its highlighted row, and
  * project output ends at its scroll position -- the newest line while following.
  */
class PanelRowWindowingSpec extends AnyFlatSpec with Matchers:

  private val issues =
    (0 until 30).toList.map(line => Diagnostic(s"issue $line", DiagnosticSeverity.Warning, Location(line, 0)))

  "A long list" should "scroll to keep its highlighted row in view" in {
    val views = PanelContentResolver.diagnosticsRowViews(LayoutRect(0, 0, 20, 12), issues, Some(Location(25, 0)))

    views.find(_.row.selected).flatMap(_.issueIndex) shouldBe Some(25)
  }

  it should "start at the top while nothing further down is highlighted" in {
    val views = PanelContentResolver.diagnosticsRowViews(LayoutRect(0, 0, 20, 12), issues, Some(Location(2, 0)))

    views.flatMap(_.issueIndex).headOption shouldBe Some(0)
  }

  "Project output" should "show its newest lines while following, and earlier ones once scrolled back" in {
    val text = (0 until 50).map(line => s"out $line").mkString("\n")
    def rows(cursor: Int) =
      PanelContentResolver
        .resolveTerminal(LayoutRect(0, 0, 40, 10), SurfaceRenderMode.Pinned, text, cursor)
        .rows
        .map(_.plainText)

    rows(text.length).lastOption shouldBe Some("out 49")
    rows(text.indexOf("out 20")).lastOption shouldBe Some("out 20")
    rows(text.length) should have size 8
  }
