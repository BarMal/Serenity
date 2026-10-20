package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.renderer.ShapedTextCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The font-keyed measurements layout and painting share belong to the cache that wraps with them, not to the JVM: a
  * font probed for one owner is still unprobed for another.
  */
class LayoutMemosOwnershipSpec extends AnyFlatSpec with Matchers:

  private val frc  = TextLayoutSnapshot.defaultFontRenderContext()
  private val font = new Font(Font.SERIF, Font.BOLD, 47)

  "A wrap cache's line extents" should "be probed once per owner" in {
    val (owner, other) = (WrappedLineCache.bounded(), WrappedLineCache.bounded())

    Vector.fill(10)(owner.layoutMemos.lineExtents.of(font, frc))

    owner.layoutMemos.lineExtents.measurements shouldBe 1L
    other.layoutMemos.lineExtents.measurements shouldBe 0L
  }

  "A wrap cache's drop cap glyph widths" should "be measured once per owner" in {
    val (owner, other) = (WrappedLineCache.bounded(), WrappedLineCache.bounded())

    Vector.fill(10)(owner.layoutMemos.dropCapWidths(font, frc, "W"))

    owner.layoutMemos.dropCapWidths.measurements shouldBe 1L
    other.layoutMemos.dropCapWidths.measurements shouldBe 0L
  }

  "A wrap cache that is not remembering" should "keep nothing between asks" in {
    WrappedLineCache.Uncached.layoutMemos.lineExtents.of(font, frc)

    WrappedLineCache.Uncached.layoutMemos.lineExtents.measurements shouldBe 0L
  }

  "Exact measurements" should "be counted by the shaped-text cache they were built through, not by the thread" in {
    val kerned = new Font(Font.SERIF, Font.PLAIN, 15)
      .deriveFont(java.util.Map.of(java.awt.font.TextAttribute.KERNING, java.awt.font.TextAttribute.KERNING_ON))
    val (owner, other) = (ShapedTextCache(), ShapedTextCache())

    ExactCaretXs.layoutCaretXs("Tales of AVAST waves", 0, TextCaretMeasurement.singleFontResolver(kerned), frc, owner)

    owner.exactMeasurements shouldBe 1L
    other.exactMeasurements shouldBe 0L
  }
