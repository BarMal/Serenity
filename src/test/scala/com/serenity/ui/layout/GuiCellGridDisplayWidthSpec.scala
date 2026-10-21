package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.renderer.ShapedTextCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866 (PR A2): on the GUI cell grid -- the layout a monospaced font with ligatures off and no advance drift gets --
  * a character the terminal counts as two cells takes two, as it does on the terminal grid, so the carets, the wrap and
  * the scroll column all agree with where the painter puts the glyphs.
  */
class GuiCellGridDisplayWidthSpec extends AnyFlatSpec with Matchers:

  private val frc      = TextLayoutSnapshot.defaultFontRenderContext()
  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val cellPx   = CellMetrics.fromFont(font).charWidth
  private val cellGrid = true

  private def rows(text: String, widthPx: Int) =
    TextLayoutSnapshot.boundedVisualLinesForText(text, 0, widthPx, font, frc, forceCellLayout = cellGrid)

  "GUI font metrics" should "allow a glyph to take more than one cell" in {
    FontLoader.isMonospacedFont(font) shouldBe true
    CellMetrics.fromFont(font).displayWidthAware shouldBe true
  }

  "A cell-grid row with a wide character" should "put the next caret two cells later" in {
    val row = rows("a中b", 10_000).head
    row.row.caretXs.toVector.map(_ / cellPx.toFloat) shouldBe Vector(0.0f, 1.0f, 3.0f, 4.0f)
    row.widthPx shouldBe 4.0f * cellPx +- 0.001f
  }

  it should "give each wide character in a run two cells" in {
    rows("中文a", 10_000).head.row.caretXs.toVector.map(_ / cellPx.toFloat) shouldBe Vector(0.0f, 2.0f, 4.0f, 5.0f)
  }

  it should "wrap on the cells the wide characters take" in {
    rows("中文中文", 5 * cellPx).map(_.text) shouldBe Vector("中文", "中文")
  }

  it should "leave a row of narrow characters one cell each" in {
    rows("abc", 10_000).head.row.caretXs.toVector.map(_ / cellPx.toFloat) shouldBe Vector(0.0f, 1.0f, 2.0f, 3.0f)
  }

  it should "scroll to a cursor past wide characters by the cells they take" in {
    val left = TextLayoutSnapshot.leftColumnForCursorVisibility(
      "中中中中中中中中中中",
      9,
      6 * cellPx,
      font,
      ShapedTextCache(),
      frc,
      forceCellLayout = cellGrid
    )
    // Column 9 sits at cell 18, so a six-cell window ending there starts at cell 12, which is column 6. One cell a
    // character would give column 4, which leaves the cursor three wide characters off screen.
    left shouldBe 6
  }
