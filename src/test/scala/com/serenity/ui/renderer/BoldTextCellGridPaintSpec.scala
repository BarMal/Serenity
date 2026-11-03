package com.serenity.ui.renderer

import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage

import com.serenity.ReferenceFrames
import com.serenity.ui.color.RenderColor
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.fonts.{FontFallback, FontLoader}
import com.serenity.ui.layout.{CellMetrics, WrappedLineCache}
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2360: on the cell grid a bold run is drawn on the cells it takes. The bundled code font reaches the cell grid when
  * its advance is within a twelfth of half a pixel of the integer cell metrics give (14.5 px does: a 9 px cell, an
  * advance of 8.99) and its ligatures are off; its bold has to keep that advance, or the glyphs drift right off their
  * cells and the next piece's background fill (an emoji, a wide character) paints over the end of them.
  */
class BoldTextCellGridPaintSpec extends AnyFlatSpec with Matchers:

  private val white   = RenderColor.fromRgba(255, 255, 255)
  private val black   = RenderColor.fromRgba(0, 0, 0)
  private val primary = FontLoader.previewCodeFont(FontConfig(fontSize = 14.5f, enableLigatures = false))
  private val metrics = CellMetrics.fromFont(primary)
  private val cell    = metrics.charWidth
  private val label   = "bold run "
  private val heart   = String(Array(0x2764, 0xfe0f), 0, 2)

  private val fallback = Some(
    CellFallback(primary, FontFallback(Vector.empty, FontLoader.emojiFontFamily), WrappedLineCache.bounded())
  )

  private def painted(text: String, style: TextStyle): BufferedImage =
    val reference =
      ReferenceFrames.over(40 * cell, 3 * metrics.lineHeight, metrics, primary, 40 * cell, 3 * metrics.lineHeight)
    val surface = reference.surface
    surface.setForegroundColor(black)
    surface.setBackgroundColor(white)
    surface.clearViewport(white)
    CharacterRenderer.renderStyledString(
      surface,
      1,
      1,
      text,
      Theme.light,
      syntaxHighlightingEnabled = false,
      styledSegments = Some(List(StyledText(text, style, black, white))),
      fallback = fallback
    )
    reference.image

  private def inkPixels(image: BufferedImage, fromCell: Int, untilCell: Int): Int =
    (for
      y <- metrics.lineHeight until 2 * metrics.lineHeight
      x <- fromCell * cell until untilCell * cell
      if (image.getRGB(x, y) & 0xffffff) != 0xffffff
    yield 1).size

  // Row cell 1 holds the first character of the text, so the last letter of `label + "a"` is on this cell.
  private val lastCell = label.length + 1

  "The bundled code font at this size" should "be laid out on whole cells" in {
    val advance = primary.createGlyphVector(FontRenderContext(null, true, true), "M").getGlyphMetrics(0).getAdvance
    cell.toFloat shouldBe advance +- 0.05f
  }

  "A bold run on the cell grid" should "put no ink past the last cell it takes" in {
    val image = painted(label + "a", TextStyle.bold)
    inkPixels(image, lastCell, lastCell + 1) should be > 20
    inkPixels(image, lastCell + 1, 40) shouldBe 0
  }

  it should "keep the whole of its last glyph when an emoji follows" in {
    val alone  = painted(label + "a", TextStyle.bold)
    val before = painted(label + "a" + heart, TextStyle.bold)
    // The emoji is drawn on the two cells after it; the letter's own cell is the same with or without it.
    inkPixels(before, lastCell, lastCell + 1) shouldBe inkPixels(alone, lastCell, lastCell + 1)
    inkPixels(before, lastCell + 1, lastCell + 3) should be > 20
  }

  "A regular run on the cell grid" should "put no ink past the last cell it takes either" in {
    inkPixels(painted(label + "a", TextStyle.normal), lastCell + 1, 40) shouldBe 0
  }
