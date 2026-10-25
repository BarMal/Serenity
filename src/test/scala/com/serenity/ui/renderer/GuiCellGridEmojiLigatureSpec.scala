package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.TextAttribute
import java.awt.image.BufferedImage
import java.nio.file.Paths

import com.serenity.ReferenceFrames
import com.serenity.ui.color.RenderColor
import com.serenity.ui.fonts.{FontFallback, FontLoader}
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot, WrappedLineCache}
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2372: the JDK shapes an emoji sequence (a flag, a skin tone, a tag flag) only when the font it draws with has
  * ligatures on, and the cell grid runs with the code font's ligatures off. An emoji cluster is drawn as if they were
  * on, whatever the user chose for the code font: the reference is the same line painted with a ligatures-on primary,
  * whose fallback font inherits the setting.
  */
class GuiCellGridEmojiLigatureSpec extends AnyFlatSpec with Matchers:

  private val sizePx = 14.0f
  private val white  = RenderColor.fromRgba(255, 255, 255)
  private val black  = RenderColor.fromRgba(0, 0, 0)

  private val ligaturesOff =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile)
      .deriveFont(sizePx)

  private val ligaturesOn =
    ligaturesOff.deriveFont(
      java.util.Map.of[TextAttribute, Integer](TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
    )

  private val metrics  = CellMetrics.fromFont(ligaturesOff)
  private val cell     = metrics.charWidth
  private val fallback = FontFallback(Vector.empty, FontLoader.emojiFontFamily)

  private def s(codePoints: Int*): String = String(codePoints.toArray, 0, codePoints.length)

  private def painted(text: String, style: TextStyle, primary: Font): BufferedImage =
    val reference = ReferenceFrames.over(40 * cell, 3 * metrics.lineHeight, metrics, primary, 40 * cell, 3 * metrics.lineHeight)
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
      fallback = Some(CellFallback(primary, fallback, WrappedLineCache.bounded()))
    )
    reference.image

  private def repainted(text: String, primary: Font): BufferedImage =
    val reference = ReferenceFrames.over(40 * cell, 3 * metrics.lineHeight, metrics, primary, 40 * cell, 3 * metrics.lineHeight)
    val surface = reference.surface
    surface.setForegroundColor(black)
    surface.setBackgroundColor(white)
    surface.clearViewport(white)
    val row = TextLayoutSnapshot
      .boundedVisualLinesForText(
        text,
        0,
        10_000,
        primary,
        TextLayoutSnapshot.defaultFontRenderContext(),
        forceCellLayout = true
      )
      .head
    CharacterRenderer.renderCellRange(
      surface,
      row,
      0,
      text.length,
      originCell = 1,
      y = 1,
      leftCell = 0,
      rightCell = 40,
      cellPx = cell,
      fallback = Some(CellFallback(primary, fallback, WrappedLineCache.bounded()))
    )
    reference.image

  private def inkPixels(image: BufferedImage, fromCell: Int, untilCell: Int): Int =
    (for
      y <- metrics.lineHeight until 2 * metrics.lineHeight
      x <- fromCell * cell until untilCell * cell
      if (image.getRGB(x, y) & 0xffffff) != 0xffffff
    yield 1).size

  private def sameInk(left: BufferedImage, right: BufferedImage): Boolean =
    (for y <- 0 until left.getHeight; x <- 0 until left.getWidth
    yield left.getRGB(x, y) == right.getRGB(x, y)).forall(identity)

  private val flag    = s(0x1f1fa, 0x1f1f8)
  private val tone    = s(0x1f44d, 0x1f3fd)
  private val england = s(0x1f3f4, 0xe0067, 0xe0062, 0xe0065, 0xe006e, 0xe0067, 0xe007f)

  private val sequencesTheShaperJoins =
    Vector("regional-indicator flag" -> flag, "skin-tone modifier sequence" -> tone, "England tag flag" -> england)

  private val sequencesDrawnAlready = Vector(
    "variation selector 16" -> s(0x2764, 0xfe0f),
    "joined family"         -> s(0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467),
    "rainbow flag"          -> s(0x1f3f3, 0xfe0f, 0x200d, 0x1f308),
    "keycap"                -> s('1'.toInt, 0xfe0f, 0x20e3)
  )

  private def lines(cluster: String): String = "a" + cluster + "b"

  sequencesTheShaperJoins.foreach { (name, cluster) =>
    s"The $name on the cell grid" should "be drawn as one glyph with the code font's ligatures off" in {
      val reference = painted(lines(cluster), TextStyle.normal, ligaturesOn)
      inkPixels(reference, 2, 3) should be > 0
      sameInk(painted(lines(cluster), TextStyle.normal, ligaturesOff), reference) shouldBe true
    }

    it should "be drawn as one glyph in a bold line" in {
      val reference = painted(lines(cluster), TextStyle.bold, ligaturesOn)
      sameInk(painted(lines(cluster), TextStyle.bold, ligaturesOff), reference) shouldBe true
    }
  }

  "A flag on the cell grid" should "draw as something other than its first regional indicator" in {
    val firstLetterOnly = painted(lines(s(0x1f1fa)), TextStyle.normal, ligaturesOff)
    sameInk(painted(lines(flag), TextStyle.normal, ligaturesOff), firstLetterOnly) shouldBe false
  }

  "An emoji sequence the cell grid already drew whole" should "be drawn exactly as before, whatever the ligature setting" in
    sequencesDrawnAlready.foreach { (name, cluster) =>
      withClue(name) {
        val off = painted(lines(cluster), TextStyle.normal, ligaturesOff)
        inkPixels(off, 2, 4) should be > 20
        sameInk(off, painted(lines(cluster), TextStyle.normal, ligaturesOn)) shouldBe true
      }
    }

  "An emoji line with the code font's ligatures on" should "still draw every sequence on its cells" in
    (sequencesTheShaperJoins ++ sequencesDrawnAlready).foreach { (name, cluster) =>
      withClue(name)(inkPixels(painted(lines(cluster), TextStyle.normal, ligaturesOn), 2, 4) should be > 20)
    }

  sequencesTheShaperJoins.foreach { (name, cluster) =>
    s"The $name in a selection repaint" should "be drawn as one glyph with the code font's ligatures off" in {
      val reference = repainted(lines(cluster), ligaturesOn)
      inkPixels(reference, 2, 3) should be > 0
      sameInk(repainted(lines(cluster), ligaturesOff), reference) shouldBe true
    }
  }
