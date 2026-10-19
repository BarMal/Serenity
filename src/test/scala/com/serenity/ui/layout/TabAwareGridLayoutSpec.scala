package com.serenity.ui.layout

import java.awt.Font

import com.serenity.state.models.IndentationSettings
import com.serenity.ui.layout.TextCaretMeasurement.singleFontResolver
import com.serenity.ui.renderer.ShapedTextCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** On a cell grid a tab spans to the next tab stop, and caret placement, the wrap and horizontal scrolling all measure
  * it with the one width the renderer paints it at.
  */
class TabAwareGridLayoutSpec extends AnyFlatSpec with Matchers:

  private val font = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val frc  = TextLayoutSnapshot.defaultFontRenderContext()

  private val oneCell     = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)
  private val oneWideCell = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0, displayWidthAware = true)
  private val eightPxCell = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 12)

  private def caretXs(text: String, tabSize: Int, cells: CellMetrics): List[Float] =
    TextCaretMeasurement
      .caretXs(text, 0, singleFontResolver(font, tabSize), frc, measuredLayout = false, cells, ShapedTextCache())
      .toList

  "Grid caret stops" should "advance a tab to the next stop at the configured size" in {
    caretXs("a\tb", 4, oneCell) shouldBe List(0f, 1f, 4f, 5f)
    caretXs("a\tb", 8, oneCell) shouldBe List(0f, 1f, 8f, 9f)
    caretXs("a\tb", 2, oneCell) shouldBe List(0f, 1f, 2f, 3f)
  }

  it should "give consecutive tabs a full stop each" in {
    caretXs("\t\tx", 4, oneCell) shouldBe List(0f, 4f, 8f, 9f)
  }

  it should "scale a tab's cells by the cell width" in {
    caretXs("a\tb", 4, eightPxCell) shouldBe List(0f, 8f, 32f, 40f)
  }

  it should "agree on a display-width-aware grid, where a wide glyph takes two cells" in {
    caretXs("a\tb", 4, oneWideCell) shouldBe List(0f, 1f, 4f, 5f)
    caretXs("中\tb", 4, oneWideCell) shouldBe List(0f, 2f, 4f, 5f)
  }

  it should "measure text without a tab exactly as before" in {
    caretXs("abc", 8, oneCell) shouldBe List(0f, 1f, 2f, 3f)
  }

  "Fitting a row on a grid" should "count a tab as its stop's width" in {
    TextCaretMeasurement.fittingUniformSegmentLength("a\tbcdef", 0, 6, 1, 4) shouldBe 4
    TextCaretMeasurement.fittingUniformSegmentLength("a\tbcdef", 0, 6, 1, 8) shouldBe 1
    TextCaretMeasurement.fittingUniformSegmentLength("\t\tx", 0, 6, 1, 4) shouldBe 1
  }

  it should "start the tab stops at the row it starts, not at the line" in {
    TextCaretMeasurement.fittingUniformSegmentLength("abcdefg\th", 7, 6, 1, 4) shouldBe 2
  }

  it should "fit text without a tab as before, at least one character" in {
    TextCaretMeasurement.fittingUniformSegmentLength("abcdef", 0, 4, 1, 8) shouldBe 4
    TextCaretMeasurement.fittingUniformSegmentLength("abcdef", 0, 0, 1, 8) shouldBe 1
  }

  it should "count a tab as its stop's width on a display-width-aware grid" in {
    TextCaretMeasurement.fittingDisplayWidthSegmentLength("a\tbcdef", 0, 6, 1, 4) shouldBe 4
    TextCaretMeasurement.fittingDisplayWidthSegmentLength("a\tbcdef", 0, 6, 1, 8) shouldBe 1
    TextCaretMeasurement.fittingDisplayWidthSegmentLength("中\tbcdef", 0, 6, 1, 4) shouldBe 4
  }

  "Wrapping a line with tabs" should "fit every row of more than one character in the panel at the tab size in use" in {
    val line = "a\tb\tc\td\te\tf\tg\th"
    def rowsAt(tabSize: Int) =
      TextLayoutSnapshot.boundedVisualLinesForText(
        line,
        0,
        12,
        font,
        frc,
        cellMetricsOverride = Some(oneCell),
        forceCellLayout = true,
        richText = RichTextContext.plain.copy(tabSize = tabSize)
      )
    List(2, 4, 8).foreach { tabSize =>
      val rows = rowsAt(tabSize)
      rows.map(_.text).mkString shouldBe line
      rows.filter(_.text.length > 1).foreach(row => row.widthPx should be <= 12f)
    }
    rowsAt(8).length should be > rowsAt(2).length
  }

  it should "place each row's carets from the row's own start" in {
    val rows = TextLayoutSnapshot.boundedVisualLinesForText(
      "abcdefgh\tij",
      0,
      10,
      font,
      frc,
      cellMetricsOverride = Some(oneCell),
      forceCellLayout = true,
      richText = RichTextContext.plain.copy(tabSize = 4)
    )
    rows.map(_.text) shouldBe Vector("abcdefgh", "\tij")
    rows.last.caretStops.map(_.xPx) shouldBe Vector(0f, 4f, 5f, 6f)
  }

  "Horizontal scrolling on a grid" should "follow the cursor through the tab's width" in {
    def leftColumn(tabSize: Int) =
      TextLayoutSnapshot.leftColumnForCursorVisibility(
        "\t\t\t\tabcdef",
        10,
        10,
        font,
        ShapedTextCache(),
        frc,
        cellMetricsOverride = Some(oneCell),
        forceCellLayout = true,
        tabSize = tabSize
      )
    leftColumn(2) shouldBe 2
    leftColumn(4) shouldBe 3
  }

  it should "keep the plain column arithmetic for a line without a tab" in {
    TextLayoutSnapshot.leftColumnForCursorVisibility(
      "abcdefghijkl",
      12,
      10,
      font,
      ShapedTextCache(),
      frc,
      cellMetricsOverride = Some(oneCell),
      forceCellLayout = true
    ) shouldBe 3
  }

  "The visual row cache stamp" should "differ when only the tab size does" in {
    val atTwo  = VisualLineIndexStore.stampOf(RichTextContext.plain.copy(tabSize = 2))
    val atFour = VisualLineIndexStore.stampOf(RichTextContext.plain.copy(tabSize = 4))
    VisualLineIndexStore.stampsRelate(atTwo, atFour) shouldBe VisualLineIndexStore.StampRelation.Unrelated
  }

  it should "relate two stamps of the same tab size" in {
    val first  = VisualLineIndexStore.stampOf(RichTextContext.plain.copy(tabSize = 2))
    val second = VisualLineIndexStore.stampOf(RichTextContext.plain.copy(tabSize = 2))
    VisualLineIndexStore.stampsRelate(first, second) should not be VisualLineIndexStore.StampRelation.Unrelated
  }

  it should "stay the plain stamp at the default tab size" in {
    VisualLineIndexStore.stampOf(RichTextContext.plain) shouldBe VisualLineIndexStore.Unstamped
    RichTextContext.plain.tabSize shouldBe IndentationSettings.DefaultTabSize
  }
