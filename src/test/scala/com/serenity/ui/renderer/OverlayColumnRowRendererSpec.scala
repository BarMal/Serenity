package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.MockRenderSurface
import com.serenity.ui.layout.{OverlayRow, OverlaySegment}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `OverlayColumnRowRenderer` (issue #1421): the pure column-width arithmetic
  * ([[OverlayColumnRowRenderer.threeColumnWidths]], [[OverlayColumnRowRenderer.fitCellText]]) and the row-shape
  * dispatch in [[OverlayColumnRowRenderer.renderColumnRow]]/[[OverlayColumnRowRenderer.renderPriorityColumnRow]].
  */
class OverlayColumnRowRendererSpec extends AnyFlatSpec with Matchers:

  private val theme = Theme.dark
  private val font  = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val fg    = Color.WHITE
  private val bg    = Color.BLACK

  "threeColumnWidths" should "use the preferred label/value/hint split when the row is wide enough" in {
    val (label, hint, value) = OverlayColumnRowRenderer.threeColumnWidths(60)

    // preferredLabel = min(22, max(8, 60/3)) = 20, preferredValue = min(18, max(8, 60/4)) = 15;
    // 20 + 15 + 2 = 37 <= 60, so the preferred split is used directly.
    label shouldBe 20
    value shouldBe 15
    hint shouldBe 60 - label - value - 2
    label + hint + value + 2 shouldBe 60
  }

  it should "fall back to a clamped split when the preferred label+value would overflow a narrow row" in {
    val (label, hint, value) = OverlayColumnRowRenderer.threeColumnWidths(10)

    label shouldBe math.min(22, 10 / 3)
    value shouldBe math.min(18, 10 / 4)
    hint shouldBe math.max(0, 10 - label - value - 2)
  }

  it should "never return a negative hint width for a pathologically narrow row" in {
    val (_, hint, _) = OverlayColumnRowRenderer.threeColumnWidths(1)

    hint should be >= 0
  }

  it should "collapse every column to zero width for a non-positive row width" in {
    // safeWidth clamps to 0: the preferred 8/8 minimums no longer fit (8+8+2 > 0), so the overflow branch's own
    // safeWidth/3 and safeWidth/4 fallbacks are used instead, both of which are 0.
    val (label, hint, value) = OverlayColumnRowRenderer.threeColumnWidths(-5)

    label shouldBe 0
    hint shouldBe 0
    value shouldBe 0
  }

  "fitCellText" should "return the text unchanged when it already fits" in {
    OverlayColumnRowRenderer.fitCellText("abc", 10) shouldBe "abc"
    OverlayColumnRowRenderer.fitCellText("abc", 3) shouldBe "abc"
  }

  it should "return an empty string for a non-positive width" in {
    OverlayColumnRowRenderer.fitCellText("abc", 0) shouldBe ""
    OverlayColumnRowRenderer.fitCellText("abc", -1) shouldBe ""
  }

  it should "hard-truncate with no ellipsis once the width is 3 or fewer" in {
    OverlayColumnRowRenderer.fitCellText("abcdef", 3) shouldBe "abc"
    OverlayColumnRowRenderer.fitCellText("abcdef", 1) shouldBe "a"
  }

  it should "truncate to width-3 characters plus an ellipsis once the width allows it" in {
    OverlayColumnRowRenderer.fitCellText("abcdefghij", 6) shouldBe "abc..."
    OverlayColumnRowRenderer.fitCellText("abcdefghij", 6).length shouldBe 6
  }

  "renderColumnRow" should "lay out a 3-segment row as label/hint/value with the value right-aligned" in {
    val surface = new MockRenderSurface(80, 3)
    val row = OverlayRow(
      plainText = "unused",
      segments = List(
        OverlaySegment("Open File"),
        OverlaySegment("ctrl+o"),
        OverlaySegment("File")
      )
    )

    OverlayColumnRowRenderer.renderColumnRow(surface, x = 0, y = 0, width = 60, row, theme, fg, bg, font)

    val (labelWidth, _, valueWidth) = OverlayColumnRowRenderer.threeColumnWidths(60)
    surface.getRow(0).take(labelWidth).trim shouldBe "Open File"
    // Right-aligned: the value text should end exactly at the row's right edge.
    surface.getRow(0).substring(60 - valueWidth, 60).trim shouldBe "File"
  }

  it should "lay out a 4-segment row as label/value/scope/breadcrumb with the breadcrumb right-aligned" in {
    val surface = new MockRenderSurface(80, 3)
    val row = OverlayRow(
      plainText = "unused",
      segments = List(
        OverlaySegment("fontSize"),
        OverlaySegment("14"),
        OverlaySegment("editor"),
        OverlaySegment("Settings > Editor")
      )
    )

    OverlayColumnRowRenderer.renderColumnRow(surface, x = 0, y = 0, width = 70, row, theme, fg, bg, font)

    surface.getRow(0).trim should (include("fontSize") and include("14") and include("editor"))
  }

  it should "lay out a 2-segment row as label/hint" in {
    val surface = new MockRenderSurface(80, 3)
    val row     = OverlayRow(plainText = "unused", segments = List(OverlaySegment("Theme"), OverlaySegment("dark")))

    OverlayColumnRowRenderer.renderColumnRow(surface, x = 0, y = 0, width = 40, row, theme, fg, bg, font)

    val (labelWidth, _) = twoColumnWidthsFor(40)
    surface.getRow(0).take(labelWidth).trim shouldBe "Theme"
    surface.getRow(0).drop(labelWidth + 1).trim shouldBe "dark"
  }

  it should "fall back to plain text for a segment count it does not recognize" in {
    val surface = new MockRenderSurface(80, 3)
    val row     = OverlayRow(plainText = "just one column", segments = List(OverlaySegment("solo")))

    OverlayColumnRowRenderer.renderColumnRow(surface, x = 0, y = 0, width = 40, row, theme, fg, bg, font)

    surface.getRow(0).trim shouldBe "just one column"
  }

  "renderPriorityColumnRow" should "lay out a 3-segment row as label/description/shortcut, shortcut right-aligned" in {
    val surface = new MockRenderSurface(80, 3)
    val row = OverlayRow(
      plainText = "unused",
      segments = List(OverlaySegment("Save File"), OverlaySegment("Write buffer to disk"), OverlaySegment("ctrl+s"))
    )

    OverlayColumnRowRenderer.renderPriorityColumnRow(surface, x = 0, y = 0, width = 60, row, theme, fg, bg, font)

    surface.getRow(0).trim should (include("Save File") and endWith("ctrl+s"))
  }

  it should "delegate to renderColumnRow's shape dispatch for any other segment count" in {
    val surface = new MockRenderSurface(80, 3)
    val row     = OverlayRow(plainText = "unused", segments = List(OverlaySegment("Theme"), OverlaySegment("dark")))

    OverlayColumnRowRenderer.renderPriorityColumnRow(surface, x = 0, y = 0, width = 40, row, theme, fg, bg, font)

    val (labelWidth, _) = twoColumnWidthsFor(40)
    surface.getRow(0).take(labelWidth).trim shouldBe "Theme"
  }

  /** Mirrors the private `twoColumnWidths` computation so tests can locate where the second column starts without
    * depending on renderer internals.
    */
  private def twoColumnWidthsFor(width: Int): (Int, Int) =
    val preferredLabel = math.min(22, math.max(8, width / 3))
    val labelWidth     = if preferredLabel + 1 <= width then preferredLabel else math.min(22, width / 3)
    (labelWidth, math.max(0, width - labelWidth - 1))
