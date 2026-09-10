package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.MockRenderSurface
import com.serenity.ui.layout.{OverlayRow, OverlaySegment, OverlayTone}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `OverlaySegmentRowRenderer` (issue #1421): the segment-layout arithmetic for
  * `Distributed`/`Split`/plain inline rows, and the tone/selection colour rules in
  * [[OverlaySegmentRowRenderer.renderSegmentText]] every one of those layouts shares.
  */
class OverlaySegmentRowRendererSpec extends AnyFlatSpec with Matchers:

  private val theme = Theme.dark
  private val font  = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val fg    = Color.WHITE
  private val bg    = Color.BLACK

  private def surface(width: Int = 80): MockRenderSurface = new MockRenderSurface(width, 3)

  "renderDistributedRow" should "fall back to plain text when the row carries no segments" in {
    val s   = surface()
    val row = OverlayRow(plainText = "no segments here")

    OverlaySegmentRowRenderer.renderDistributedRow(s, x = 0, y = 0, width = 40, row, theme, fg, bg, font)

    s.getRow(0).trim shouldBe "no segments here"
  }

  it should "split the row into equal-ish cells, giving the leading cells the width remainder" in {
    val s = surface()
    val row =
      OverlayRow(plainText = "unused", segments = List(OverlaySegment("a"), OverlaySegment("b"), OverlaySegment("c")))

    // width=10 over 3 segments: base=3, remainder=1 -> cell widths [4, 3, 3].
    OverlaySegmentRowRenderer.renderDistributedRow(s, x = 0, y = 0, width = 10, row, theme, fg, bg, font)

    // 'a' is centered in a 4-wide cell [0,4): leftPad = (4-1)/2 = 1.
    s.getChar(1, 0) shouldBe 'a'
    // 'b' is centered in the next 3-wide cell [4,7): leftPad = (3-1)/2 = 1.
    s.getChar(5, 0) shouldBe 'b'
    // 'c' is centered in the final 3-wide cell [7,10).
    s.getChar(8, 0) shouldBe 'c'
  }

  it should "route to the compact layout once any segment carries an allocatedWidth" in {
    val s = surface()
    val row = OverlayRow(
      plainText = "unused",
      segments = List(OverlaySegment("x", allocatedWidth = Some(2)), OverlaySegment("y"))
    )

    OverlaySegmentRowRenderer.renderDistributedRow(s, x = 0, y = 0, width = 10, row, theme, fg, bg, font)

    // Compact layout starts at x + leadingPadding (0 here), not the equal-cell layout's centering.
    s.getChar(0, 0) shouldBe 'x'
  }

  "renderCompactDistributedRow" should "honor leadingPadding and each segment's own allocatedWidth" in {
    val s = surface()
    val row = OverlayRow(
      plainText = "unused",
      leadingPadding = 2,
      segments = List(OverlaySegment("ab", allocatedWidth = Some(2)), OverlaySegment("c", allocatedWidth = Some(1)))
    )

    OverlaySegmentRowRenderer.renderCompactDistributedRow(s, x = 0, y = 0, width = 20, row, theme, fg, bg, font)

    s.getRow(0).take(6) shouldBe "  ab c"
  }

  it should "draw a separator glyph after a segment marked trailingSeparator, when room remains" in {
    val s = surface()
    val row = OverlayRow(
      plainText = "unused",
      segments = List(
        OverlaySegment("a", allocatedWidth = Some(1), trailingSeparator = true),
        OverlaySegment("b", allocatedWidth = Some(1))
      )
    )

    OverlaySegmentRowRenderer.renderCompactDistributedRow(s, x = 0, y = 0, width = 10, row, theme, fg, bg, font)

    s.getChar(0, 0) shouldBe 'a'
    s.getChar(1, 0) shouldBe '│' // '│'
  }

  "renderSplitRow" should "left-anchor the first segment and right-anchor the remaining segments as a group" in {
    val s   = surface()
    val row = OverlayRow(plainText = "unused", segments = List(OverlaySegment("Find"), OverlaySegment("3/12")))

    OverlaySegmentRowRenderer.renderSplitRow(s, x = 0, y = 0, width = 20, row, theme, fg, bg, font)

    s.getRow(0).take(4) shouldBe "Find"
    s.getRow(0).substring(16, 20) shouldBe "3/12"
  }

  it should "route to the editable layout when the row carries a cursor column" in {
    val s = surface()
    val row = OverlayRow(
      plainText = "unused",
      cursorColumn = Some(0),
      segments = List(OverlaySegment("Replace:"), OverlaySegment("term"))
    )

    OverlaySegmentRowRenderer.renderSplitRow(s, x = 0, y = 0, width = 20, row, theme, fg, bg, font)

    // The editable layout places the right segment immediately after the left one (+1 gap), not right-anchored.
    s.getRow(0).take(8) shouldBe "Replace:"
    s.getRow(0).substring(9, 13) shouldBe "term"
  }

  it should "fall back to plain text for a single-segment or empty row" in {
    val s   = surface()
    val row = OverlayRow(plainText = "single column", segments = List(OverlaySegment("single column")))

    OverlaySegmentRowRenderer.renderSplitRow(s, x = 0, y = 0, width = 20, row, theme, fg, bg, font)

    s.getRow(0).trim shouldBe "single column"
  }

  "renderInlineSegments" should "concatenate segments left to right with a single-column gap between them" in {
    val s   = surface()
    val row = OverlayRow(plainText = "unused", segments = List(OverlaySegment("one"), OverlaySegment("two")))

    OverlaySegmentRowRenderer.renderInlineSegments(s, x = 0, y = 0, width = 20, row, theme, fg, bg, font)

    s.getRow(0).take(8) shouldBe "one two "
  }

  "renderSegmentText" should "use the theme's highlighted colours for a selected segment, overriding tone" in {
    val s       = surface()
    val segment = OverlaySegment("sel", selected = true, tone = OverlayTone.Error)

    OverlaySegmentRowRenderer.renderSegmentText(s, x = 0, y = 0, width = 3, "sel", segment, theme, fg, bg, font)

    s.getFg(0, 0) shouldBe withAlphaOf(theme.highlighted.foreground, fg)
    s.getBg(0, 0) shouldBe withAlphaOf(theme.highlighted.background, bg)
  }

  it should "use the theme's error colours for an unselected Error-tone segment" in {
    val s       = surface()
    val segment = OverlaySegment("err", tone = OverlayTone.Error)

    OverlaySegmentRowRenderer.renderSegmentText(s, x = 0, y = 0, width = 3, "err", segment, theme, fg, bg, font)

    s.getFg(0, 0) shouldBe withAlphaOf(theme.error.foreground, fg)
    s.getBg(0, 0) shouldBe withAlphaOf(theme.error.background, bg)
  }

  it should "use the theme's muted foreground for an unselected Muted-tone segment, but the default background" in {
    val s       = surface()
    val segment = OverlaySegment("mut", tone = OverlayTone.Muted)

    OverlaySegmentRowRenderer.renderSegmentText(s, x = 0, y = 0, width = 3, "mut", segment, theme, fg, bg, font)

    s.getFg(0, 0) shouldBe withAlphaOf(theme.muted, fg)
    s.getBg(0, 0) shouldBe bg
  }

  it should "prefer an explicit per-segment foreground/background colour over every tone rule" in {
    val s        = surface()
    val customFg = new Color(10, 20, 30)
    val customBg = new Color(40, 50, 60)
    val segment = OverlaySegment(
      "custom",
      selected = true,
      tone = OverlayTone.Error,
      foregroundColor = Some(customFg),
      backgroundColor = Some(customBg)
    )

    OverlaySegmentRowRenderer.renderSegmentText(s, x = 0, y = 0, width = 6, "custom", segment, theme, fg, bg, font)

    s.getFg(0, 0) shouldBe withAlphaOf(customFg, fg)
    s.getBg(0, 0) shouldBe withAlphaOf(customBg, bg)
  }

  it should "draw nothing for a zero (or negative) width segment" in {
    val s       = surface()
    val segment = OverlaySegment("hidden")

    OverlaySegmentRowRenderer.renderSegmentText(s, x = 0, y = 0, width = 0, "hidden", segment, theme, fg, bg, font)

    s.getRow(0).trim shouldBe ""
  }

  /** Mirrors `ColorFormat.withAlpha`: `color`'s RGB channels with `alphaFrom`'s alpha channel -- the renderer's own
    * rule for keeping the surface's overall translucency consistent across styled segments.
    */
  private def withAlphaOf(color: Color, alphaFrom: Color): Color =
    new Color(color.getRed, color.getGreen, color.getBlue, alphaFrom.getAlpha)
