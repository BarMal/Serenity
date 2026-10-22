package com.serenity.ui.layout

import java.awt.Font

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2365: on a display-width-aware grid an emoji cluster takes two cells as one unit, so the caret stops after it, the
  * wrap and a tab that follows it all count it as two cells, and no stop or wrap falls inside it.
  */
class ClusterCellLayoutSpec extends AnyFlatSpec with Matchers:

  private def s(codePoints: Int*): String = String(codePoints.toArray, 0, codePoints.length)

  private val heart  = s(0x2764, 0xfe0f)
  private val family = s(0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467)

  private val frc    = TextLayoutSnapshot.defaultFontRenderContext()
  private val font   = Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val cellPx = CellMetrics.fromFont(font).charWidth

  private def rows(text: String, widthPx: Int = 10_000) =
    TextLayoutSnapshot.boundedVisualLinesForText(text, 0, widthPx, font, frc, forceCellLayout = true)

  "The terminal-grid carets" should "put the caret after a variation-selector-16 emoji two cells on" in {
    TextCaretMeasurement.displayWidthCaretXs("a" + heart + "b", 1.0f, 4).toVector shouldBe
      Vector(0.0f, 1.0f, 3.0f, 3.0f, 4.0f)
  }

  it should "count a joined family as the one two-cell unit, with no width in its joiners" in {
    TextCaretMeasurement.displayWidthCaretXs("a" + family + "b", 1.0f, 4).toVector shouldBe
      Vector(0.0f, 1.0f) ++ Vector.fill(8)(3.0f) :+ 4.0f
  }

  it should "start a tab after a cluster at the cluster's width" in {
    TextCaretMeasurement.displayWidthCaretXs(heart + "\tb", 1.0f, 4).toVector shouldBe
      Vector(0.0f, 2.0f, 2.0f, 4.0f, 5.0f)
  }

  it should "leave a row without cluster characters as it was" in {
    TextCaretMeasurement.displayWidthCaretXs("a中b", 1.0f, 4).toVector shouldBe Vector(0.0f, 1.0f, 3.0f, 4.0f)
  }

  "fittingDisplayWidthSegmentLength" should "never split a cluster, and count it as two cells" in {
    val text = "a" + family + "b"
    TextCaretMeasurement.fittingDisplayWidthSegmentLength(text, 0, 3, 1, 4) shouldBe 1 + family.length
    TextCaretMeasurement.fittingDisplayWidthSegmentLength(text, 0, 2, 1, 4) shouldBe 1
  }

  "A GUI cell-grid row" should "place the caret stops after an emoji cluster by its two cells" in {
    val row = rows("a" + heart + "b").head
    row.row.caretXs.toVector.map(_ / cellPx.toFloat) shouldBe Vector(0.0f, 1.0f, 3.0f, 4.0f)
    row.row.caretOffsets.toVector shouldBe Vector(0, 1, 3, 4)
    row.widthPx shouldBe 4.0f * cellPx +- 0.001f
  }

  it should "wrap on the cells the clusters take, never inside one" in {
    rows("x" + family + "y", 3 * cellPx).map(_.text) shouldBe Vector("x" + family, "y")
  }

  "TextWrapping.wrap on cells" should "count an emoji cluster as two cells" in {
    TextWrapping.wrap(family, 2) shouldBe List(family)
    TextWrapping.wrap("ab " + family, 4) shouldBe List("ab", family)
  }

  it should "never break inside a cluster when one word alone is too long" in {
    val word = family + heart + family
    TextWrapping.wrap(word, 3) shouldBe List(family, heart, family)
    TextWrapping.wrap(word, 1) shouldBe List(family, heart, family)
  }
