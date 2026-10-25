package com.serenity.ui.renderer

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.ReferenceFrames
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866 (PR A2): a string painted on the GUI cell grid puts each wide glyph on two cells, so what follows it starts on
  * the cell the caret stops say, and the background fills every cell the string takes.
  */
class GuiCellGridPaintSpec extends AnyFlatSpec with Matchers:

  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val metrics  = CellMetrics.fromFont(font)
  private val cell     = metrics.charWidth
  private val red      = RenderColor.fromRgba(255, 0, 0)
  private val white    = RenderColor.fromRgba(255, 255, 255)
  private val black    = RenderColor.fromRgba(0, 0, 0)
  private val redRgb   = 0xff0000
  private val whiteRgb = 0xffffff

  /** `text` painted from cell 1 in the foreground `ink` on the red background. */
  private def painted(text: String, ink: RenderColor = black): BufferedImage =
    val reference = ReferenceFrames.over(40 * cell, 3 * metrics.lineHeight, metrics, font, 40 * cell, 3 * metrics.lineHeight)
    val surface = reference.surface
    surface.setForegroundColor(ink)
    surface.setBackgroundColor(white)
    surface.clearViewport(white)
    surface.setBackgroundColor(red)
    surface.putString(1, 1, text)
    reference.image

  private def rgb(image: BufferedImage, x: Int, y: Int): Int = image.getRGB(x, y) & 0xffffff

  /** The pixel columns within `[fromCell, untilCell)` of the painted row that differ from the red background. */
  private def inkColumns(image: BufferedImage, fromCell: Int, untilCell: Int): Vector[Int] =
    (fromCell * cell until untilCell * cell).filter { x =>
      (metrics.lineHeight until 2 * metrics.lineHeight).exists(y => rgb(image, x, y) != redRgb)
    }.toVector

  "A wide glyph on the GUI cell grid" should "fill the background of every cell the string takes" in {
    // Drawn in the background's own colour, so no glyph's ink can show through the pixels sampled.
    val image = painted("中a", ink = red)
    // 中 takes two cells and `a` one, starting at cell 1: the string covers cells 1 to 3.
    (1 to 3).foreach(column => rgb(image, column * cell + cell - 1, metrics.lineHeight + 1) shouldBe redRgb)
    rgb(image, 4 * cell + 1, metrics.lineHeight + 1) shouldBe whiteRgb
  }

  it should "start the glyph after it on the cell after the wide glyph's two" in {
    val image = painted("中a")
    inkColumns(image, 1, 3) should not be empty
    val letter = inkColumns(image, 3, 4)
    letter should not be empty
    letter.head should be >= (3 * cell)
  }

  "A combining mark on the GUI cell grid" should "fill no cell of its own" in {
    val image = painted("e\u0301x", ink = red)
    // `e` with its mark is one cell and `x` another: cells 1 and 2, nothing past them.
    (1 to 2).foreach(column => rgb(image, column * cell + cell - 1, metrics.lineHeight + 1) shouldBe redRgb)
    rgb(image, 3 * cell + 1, metrics.lineHeight + 1) shouldBe whiteRgb
  }

  it should "stay with a wide glyph, drawn with it on its two cells" in {
    val image = painted("中\u0301a", ink = red)
    (1 to 3).foreach(column => rgb(image, column * cell + cell - 1, metrics.lineHeight + 1) shouldBe redRgb)
    rgb(image, 4 * cell + 1, metrics.lineHeight + 1) shouldBe whiteRgb
  }
