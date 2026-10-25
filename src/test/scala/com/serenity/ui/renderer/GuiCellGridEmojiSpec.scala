package com.serenity.ui.renderer

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.Paths

import com.serenity.ui.color.RenderColor
import com.serenity.ui.fonts.{FontFallback, FontLoader}
import com.serenity.ui.layout.{CellMetrics, CharWidth, WrappedLineCache}
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866 (PR B): on the GUI cell grid an emoji takes two cells and is drawn in the bundled monochrome emoji face, kept
  * inside those cells, and a bold line draws it in the Bold face.
  */
class GuiCellGridEmojiSpec extends AnyFlatSpec with Matchers:

  private val sizePx = 14.0f
  private val white  = RenderColor.fromRgba(255, 255, 255)
  private val black  = RenderColor.fromRgba(0, 0, 0)
  private val grin   = Character.toString(0x1f600)

  private val primary =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile)
      .deriveFont(sizePx)

  private val metrics  = CellMetrics.fromFont(primary)
  private val cell     = metrics.charWidth
  private val fallback = FontFallback(Vector.empty, FontLoader.emojiFontFamily)

  private def painted(text: String, style: TextStyle): BufferedImage =
    val image   = new BufferedImage(40 * cell, 3 * metrics.lineHeight, BufferedImage.TYPE_INT_RGB)
    val surface = new Java2DRenderSurface(image, metrics, primary, _ => (), 40 * cell, 3 * metrics.lineHeight, 1.0, 1.0)
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
      fallback = Some(CellFallback(primary, fallback, WrappedLineCache.bounded()))
    )
    image

  private def isInk(image: BufferedImage, x: Int, y: Int): Boolean = (image.getRGB(x, y) & 0xffffff) != 0xffffff

  private def inkPixels(image: BufferedImage, fromCell: Int, untilCell: Int): Int =
    (for
      y <- metrics.lineHeight until 2 * metrics.lineHeight
      x <- fromCell * cell until untilCell * cell
      if isInk(image, x, y)
    yield 1).size

  "An emoji on the cell grid" should "be two cells wide" in {
    CharWidth.of(0x1f600) shouldBe 2
  }

  it should "be drawn on its two cells, with the next character on the third" in {
    val image = painted(s"a${grin}b", TextStyle.normal)
    // Row cell 1 is `a`, cells 2-3 are the emoji, cell 4 is `b`.
    inkPixels(image, 2, 4) should be > 20
    inkPixels(image, 4, 5) should be > 0
  }

  it should "not spill outside its two cells" in {
    val image = painted(s"$grin", TextStyle.normal)
    inkPixels(image, 0, 1) shouldBe 0
    inkPixels(image, 3, 40) shouldBe 0
  }

  it should "be drawn heavier in a bold line, in the Bold emoji face" in {
    val regular = inkPixels(painted(grin, TextStyle.normal), 1, 3)
    val bold    = inkPixels(painted(grin, TextStyle.bold), 1, 3)
    bold should be > regular
  }

  private def s(codePoints: Int*): String = String(codePoints.toArray, 0, codePoints.length)

  private val clusters = Vector(
    "variation selector 16" -> s(0x2764, 0xfe0f),
    "emoji modifier"        -> s(0x1f44d, 0x1f3fd),
    "joined family"         -> s(0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467),
    "flag"                  -> s(0x1f1fa, 0x1f1f8),
    "rainbow flag"          -> s(0x1f3f3, 0xfe0f, 0x200d, 0x1f308)
  )

  "An emoji cluster on the cell grid (#2365)" should "be drawn on its two cells, with the next character on the third" in
    clusters.foreach { (name, cluster) =>
      val image = painted("a" + cluster + "b", TextStyle.normal)
      // Row cell 1 is `a`, cells 2-3 are the cluster, cell 4 is `b`.
      withClue(name) {
        inkPixels(image, 2, 4) should be > 20
        inkPixels(image, 3, 4) should be > 0
        inkPixels(image, 4, 5) should be > 0
      }
    }

  it should "not put any ink past its two cells" in
    clusters.foreach { (name, cluster) =>
      val image = painted(cluster, TextStyle.normal)
      withClue(name) {
        inkPixels(image, 1, 3) should be > 20
        inkPixels(image, 3, 40) shouldBe 0
      }
    }
