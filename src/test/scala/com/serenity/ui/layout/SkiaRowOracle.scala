package com.serenity.ui.layout

import java.awt.Font

import com.serenity.state.models.TextVisualLine
import com.serenity.ui.renderer.FontSpec
import org.jetbrains.skia.{FontFeature, TextLine}
import org.jetbrains.skia.shaper.ShapingOptions
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers

/** What a row of emoji or CJK text must measure, checked against Skia's own shaper rather than AWT's per-row
  * measurement ([[LegacyLineMeasurement]]), which sized and cut the fallback fonts' glyphs differently.
  */
trait SkiaRowOracle:
  self: Matchers =>

  protected def skiaFaces: com.serenity.ui.skia.SkiaFontCache = com.serenity.testkit.TestMeasure.faces

  /** The width Skia's own shaper gives `text` in `font`: an oracle that is neither AWT nor the measurer under test, in
    * the faces the measurer lays out with (a logical family such as `SansSerif` can resolve to another physical font in
    * another font cache: a row measured in Inter and checked in IPAGothic came 3.4 px apart).
    */
  protected def skiaWidthOf(text: String, font: Font): Option[Float] =
    Some {
      val face = skiaFaces.faceFor(FontSpec.fromAwt(font)).font
      val options =
        if skiaFaces.ligaturesOf(face) then ShapingOptions.Companion.getDEFAULT()
        else
          ShapingOptions.Companion
            .getDEFAULT()
            .withFeatures(Array("liga", "clig", "calt").map(new FontFeature(_, false)))
      val line = TextLine.Companion.make(text, face, options)
      try line.getWidth
      finally line.close()
    }

  /** Emoji and CJK take their glyphs from fallback fonts that AWT's per-row measurement sized and cut differently, so
    * their rows are checked against Skia and the AWT oracle stays for every other line.
    */
  protected def fallsBackInSkia(line: String): Boolean =
    line.exists(char =>
      Character.isSurrogate(char) || Set(
        Character.UnicodeScript.HAN,
        Character.UnicodeScript.HIRAGANA,
        Character.UnicodeScript.KATAKANA
      ).contains(Character.UnicodeScript.of(char))
    )

  protected val SkiaWidthTolerancePx = 1.0f

  protected def isWideCluster(cluster: String): Boolean =
    cluster.codePoints().anyMatch(codePoint => Character.isEmojiPresentation(codePoint)) ||
      cluster.exists(char =>
        Set(Character.UnicodeScript.HAN, Character.UnicodeScript.HIRAGANA, Character.UnicodeScript.KATAKANA)
          .contains(Character.UnicodeScript.of(char))
      )

  /** A row of a monospaced code line sets each wide cluster (an emoji sequence, a CJK character) on exactly two cells
    * of the font's advance, as the terminal does ([[WideCells]]), whichever font draws its glyph; so the row's own
    * shaped width is not what is compared, but the caret distance across every wide cluster is.
    */
  protected def assertWideClustersTakeTwoCells(row: TextVisualLine, line: String, font: Font): Assertion =
    val twoCells = 2 * skiaWidthOf("M", font).getOrElse(0.0f)
    val breaks   = java.text.BreakIterator.getCharacterInstance
    breaks.setText(row.text)
    val bounds =
      Iterator.iterate(breaks.first())(_ => breaks.next()).takeWhile(_ != java.text.BreakIterator.DONE).toVector
    val xOfOffset = row.caretStops.map(stop => stop.column - row.startColumn -> stop.xPx).toMap
    bounds.zip(bounds.drop(1)).foreach { (from, until) =>
      if isWideCluster(row.text.substring(from, until)) then
        xOfOffset.get(from).zip(xOfOffset.get(until)).foreach((start, end) => (end - start) shouldBe twoCells +- 0.01f)
    }
    succeed

  protected def assertSkiaRows(rows: Vector[TextVisualLine], line: String, widthPx: Int, font: Font): Assertion =
    rows.map(_.text).mkString shouldBe line
    val boundaries = graphemeBoundaries(line)
    // A tab is a cell of the layout's own, which a shaped line has no notion of, so rows holding one are not compared.
    rows
      .filter(row =>
        boundaries.contains(row.startColumn) && boundaries.contains(row.endColumn) && !row.text.contains('\t')
      )
      .foreach { row =>
        withClue(s"row '${row.text}' of $font at $widthPx px: ") {
          if font.getFamily == Font.MONOSPACED then assertWideClustersTakeTwoCells(row, line, font)
          else skiaWidthOf(row.text, font).foreach(expected => row.widthPx shouldBe expected +- SkiaWidthTolerancePx)
          if widthPx >= 40 then row.widthPx should be <= widthPx + SkiaWidthTolerancePx
        }
      }
    succeed

  /** A row too narrow for an emoji sequence is cut inside it, so it holds half a cluster; shaped alone such text is not
    * what the row was measured as, which is why only rows that start and end on a grapheme boundary are compared.
    */
  protected def graphemeBoundaries(line: String): Set[Int] =
    val breaks = java.text.BreakIterator.getCharacterInstance
    breaks.setText(line)
    Iterator.iterate(breaks.first())(_ => breaks.next()).takeWhile(_ != java.text.BreakIterator.DONE).toSet

  protected def assertSkiaCarets(carets: Vector[Float], line: String, font: Font): Assertion =
    carets.length shouldBe line.length + 1
    carets.headOption shouldBe Some(0.0f)
    carets.zip(carets.drop(1)).foreach((from, to) => to should be >= from)
    skiaWidthOf(line, font).foreach(expected =>
      carets.lastOption.getOrElse(0.0f) shouldBe expected +- SkiaWidthTolerancePx
    )
    succeed
