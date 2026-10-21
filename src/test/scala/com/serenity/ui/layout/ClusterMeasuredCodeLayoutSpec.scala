package com.serenity.ui.layout

import java.awt.Font
import java.nio.file.Paths

import com.serenity.state.models.TextVisualLine
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2365: in a code line laid out by measured advances over a monospaced primary, an emoji cluster the emoji face draws
  * is set on exactly two cells of the primary's grid, whatever its glyph's own advance, so what follows it stays on the
  * grid and lines up with the cell grid and the terminal.
  */
class ClusterMeasuredCodeLayoutSpec extends AnyFlatSpec with Matchers:

  private def s(codePoints: Int*): String = String(codePoints.toArray, 0, codePoints.length)

  private val heart      = s(0x2764, 0xfe0f)
  private val thumbsTone = s(0x1f44d, 0x1f3fd)
  private val family     = s(0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467)
  private val flag       = s(0x1f1fa, 0x1f1f8)
  private val rainbow    = s(0x1f3f3, 0xfe0f, 0x200d, 0x1f308)

  private val frc = TextLayoutSnapshot.defaultFontRenderContext()

  private val codeFont =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile)
      .deriveFont(Font.PLAIN, 14.0f)

  private val cellPx = WideCells.cellAdvancePx(codeFont, frc)

  private def rowOf(text: String): TextVisualLine =
    val rows = TextLayoutSnapshot.boundedVisualLinesForText(
      text,
      0,
      10_000,
      codeFont,
      frc,
      forceCellLayout = false,
      richText = RichTextContext(None, 1.0f, dropCapsEnabled = true, fallback = FontConfig().fallback)
    )
    rows should have size 1
    rows.head

  private def landingCells(text: String): Vector[Float] =
    rowOf(text).row.caretXs.toVector.map(_ / cellPx)

  "A code line's emoji cluster" should "take two cells of the primary's grid, and the character after it the third" in
    Vector(heart, thumbsTone, family, flag, rainbow).foreach { cluster =>
      val cells = landingCells("a" + cluster + "b")
      withClue(cluster.codePoints().toArray.toVector.map(_.toHexString)) {
        cells.map(math.round) shouldBe Vector(0, 1, 3, 4)
        cells.zip(Vector(0.0f, 1.0f, 3.0f, 4.0f)).foreach((got, want) => got shouldBe want +- 0.01f)
      }
    }

  it should "leave a wide emoji by itself at two cells and a plain line on the grid" in {
    landingCells("a" + s(0x1f680) + "b").map(math.round) shouldBe Vector(0, 1, 3, 4)
    landingCells("abc").map(math.round) shouldBe Vector(0, 1, 2, 3)
  }
