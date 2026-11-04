package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.renderer.ShapedTextCache
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The per-font certificate for a font set with ligatures (#2447 B2). Shaping may give a character another glyph there,
  * so the certificate is granted only when shaping every pair of printable ASCII and every triple of symbols and space
  * leaves each run at the width of its characters' own advances and on the baseline, and when counting a battery of
  * code-like lines by the table agrees with wrapping them.
  */
class LigatureCertificateSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val config    = FontLoader.FontConfig()
  private val printable = (0x20 to 0x7e).map(_.toChar)
  private val sizes     = List(11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f, 18.0f)

  private def ligatureFont(sizePt: Float): Font =
    FontLoader.previewCodeFont(config.copy(fontSize = sizePt, enableLigatures = true))

  private val honest = LigatureCertificate.javaShaper

  "A code font set with ligatures" should "be granted a certificate at every size, including those the cell grid drifts from" in
    sizes.foreach(size => withClue(s"$size pt: ")(LigatureCertificate.of(ligatureFont(size), frc) shouldBe defined))

  it should "list, for each printable character, the advance the measured path gives it" in {
    val shapedText = ShapedTextCache()
    sizes.foreach { size =>
      val font     = ligatureFont(size)
      val resolver = TextCaretMeasurement.singleFontResolver(font)
      val table    = LigatureCertificate.of(font, frc).value
      printable.foreach { char =>
        val measured = GlyphAdvances.measure(char.toString, 0, 1, 0, resolver, frc, shapedText).caretsFrom(0, 1)(1)
        withClue(s"$size pt, '$char': ")(table.advanceOf(char) shouldBe measured)
      }
    }
  }

  it should "call a line one row exactly as the table of the same font without ligatures does" in {
    val font    = ligatureFont(12.0f)
    val plain   = FontLoader.previewCodeFont(config.copy(fontSize = 12.0f, enableLigatures = false))
    val granted = LigatureCertificate.of(font, frc).value
    val exact   = NominalAdvances.of(plain, frc).value
    List("a -> b => c", "x" * 90, "  val value = a |> f >>= g").foreach { text =>
      List(300, 800, 1472).foreach { width =>
        withClue(s"$width px, '$text': ")(granted.fitsOneRow(text, width) shouldBe exact.fitsOneRow(text, width))
      }
    }
  }

  it should "be refused when shaping a pair sets the run off the width of its characters" in {
    val widened: LigatureCertificate.Shaper = (font, frc, chars) =>
      val run = honest(font, frc, chars)
      if String(chars) == "=>" then run.copy(width = run.width + 0.5f) else run
    LigatureCertificate.of(ligatureFont(12.0f), frc, widened) shouldBe None
  }

  it should "be refused when shaping a triple of symbols puts a glyph off the baseline" in {
    val raised: LigatureCertificate.Shaper = (font, frc, chars) =>
      val run = honest(font, frc, chars)
      if String(chars) == "<$>" then run.copy(offBaseline = true) else run
    LigatureCertificate.of(ligatureFont(12.0f), frc, raised) shouldBe None
  }

  it should "be refused when a code-like line the table calls one row wraps to more" in {
    LigatureCertificate.of(ligatureFont(12.0f), frc, honest, (_, _) => 2) shouldBe None
  }

  it should "be checked against the wrap on lines either side of their own width" in {
    val asked = scala.collection.mutable.ListBuffer.empty[(String, Int)]
    LigatureCertificate.of(
      ligatureFont(12.0f),
      frc,
      honest,
      (text, width) =>
        asked += text -> width
        1
    )
    asked.map(_._1).distinct.size should be >= 150
    asked.map(_._2).distinct.size should be >= 300
  }

  "The proportional text font" should "have no certificate" in {
    LigatureCertificate.of(FontLoader.previewTextFont(config), frc) shouldBe None
  }
