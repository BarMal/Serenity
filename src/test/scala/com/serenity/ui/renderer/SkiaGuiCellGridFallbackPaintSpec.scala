package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Font, GraphicsEnvironment}
import java.nio.file.Paths

import com.serenity.SkiaFrames
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.fonts.FontFallback
import com.serenity.ui.layout.{CellMetrics, WrappedLineCache}
import com.serenity.ui.skia.SkiaTestFonts
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866 (PR A2): text the primary font has no glyph for is drawn in a fallback font on the GUI cell grid, each wide
  * character on its two cells and each one-cell fallback symbol on its one. The fallback is a test subset font whose 中
  * is a solid box 0.9 em wide, so its ink says which font drew a glyph and where.
  */
class SkiaGuiCellGridFallbackPaintSpec extends AnyFlatSpec with Matchers:

  private val sizePx = 14.0f
  private val white  = RenderColor.fromRgba(255, 255, 255)
  private val black  = RenderColor.fromRgba(0, 0, 0)

  private def resource(path: String): Font =
    Font.createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(path).toURI).toFile)

  private val subset = resource("/fonts/SerenityFallbackTest.ttf")
  private val _      = GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(subset)

  private val primary  = resource("/fonts/MonaspaceNeon-Regular.otf").deriveFont(sizePx)
  private val metrics  = CellMetrics.fromFont(primary)
  private val cell     = metrics.charWidth
  private val fallback = FontFallback(Vector(subset.getFamily))

  private def painted(text: String, chain: Option[FontFallback]): BufferedImage =
    val caches = RenderCaches.create()
    require(SkiaTestFonts.install(caches.backendCaches, "/fonts/SerenityFallbackTest.ttf").isRight)
    val frame = SkiaFrames.newFrame(40 * cell, 3 * metrics.lineHeight)
    val surface =
      SkiaFrames.surfaceOver(frame, metrics, primary, 40 * cell, 3 * metrics.lineHeight, 1.0, persists = false, caches)
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
      styledSegments = Some(List(StyledText(text, TextStyle.normal, black, white))),
      fallback = chain.map(CellFallback(primary, _, WrappedLineCache.bounded()))
    )
    frame.toImage

  private def isInk(image: BufferedImage, x: Int, y: Int): Boolean = (image.getRGB(x, y) & 0xffffff) != 0xffffff

  private def inkColumns(image: BufferedImage, fromCell: Int, untilCell: Int): Vector[Int] =
    (fromCell * cell until untilCell * cell)
      .filter(x => (metrics.lineHeight until 2 * metrics.lineHeight).exists(isInk(image, x, _)))
      .toVector

  /** The share of the bounding box of the ink in `[fromCell, untilCell)` that is inked. */
  private def fillRatio(image: BufferedImage, fromCell: Int, untilCell: Int): Double =
    val inked = for
      y <- metrics.lineHeight until 2 * metrics.lineHeight
      x <- fromCell * cell until untilCell * cell
      if isInk(image, x, y)
    yield (x, y)
    if inked.isEmpty then 0.0
    else
      val width  = inked.map(_._1).max - inked.map(_._1).min + 1
      val height = inked.map(_._2).max - inked.map(_._2).min + 1
      inked.size.toDouble / (width * height)

  private def inkWidth(image: BufferedImage, fromCell: Int, untilCell: Int): Int =
    val columns = inkColumns(image, fromCell, untilCell)
    if columns.isEmpty then 0 else columns.last - columns.head + 1

  "A wide character the primary font lacks" should "be drawn in the fallback font, on its two cells" in {
    val image = painted("a中b", Some(fallback))
    // Row cell 1 is `a`, cells 2-3 are 中, cell 4 is `b`.
    fillRatio(image, 2, 4) should be > 0.95
    // The subset 中 is 0.9 em wide and starts 0.05 em in; the primary's own missing-glyph box is about a cell.
    inkWidth(image, 2, 4) should be >= (0.8f * sizePx).toInt
    (inkColumns(image, 2, 4).head - 2 * cell) should be <= 2
    inkColumns(image, 4, 5) should not be empty
  }

  // What the fallback font draws for 中 is a solid box 0.9 em wide, so a glyph that is both that wide and that solid was
  // drawn by it. Without a fallback, Skia draws whatever the system offers for the glyph, which differs by machine: a
  // real CJK glyph where a CJK font is installed (wide, not solid), the primary's own box where none is (a cell wide).
  it should "not be drawn in the fallback font when no fallback is passed" in {
    val image = painted("a中b", None)
    info(s"ink width ${inkWidth(image, 2, 4)}, fill ${fillRatio(image, 2, 4)}")
    val asWideAsTheFallbackBox  = inkWidth(image, 2, 4) >= (0.8f * sizePx).toInt
    val asSolidAsTheFallbackBox = fillRatio(image, 2, 4) > 0.95
    (asWideAsTheFallbackBox && asSolidAsTheFallbackBox) shouldBe false
  }

  "A one-cell symbol the primary font lacks" should "take exactly one cell, so the next character stays on the grid" in {
    val image  = painted("★ b", Some(fallback))
    val letter = inkColumns(image, 3, 4)
    letter should not be empty
    letter.head should be >= (3 * cell - 1)
  }
