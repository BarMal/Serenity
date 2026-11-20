package com.serenity.ui.layout

import com.serenity.testkit.TestMeasure
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.renderer.FontSpec
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The per-font certificate for a font set with ligatures (#2447 B2). Shaping may give a character another glyph there,
  * so the certificate is granted only when shaping every pair of printable ASCII and every triple of symbols and space
  * leaves each run at the width of its characters' own advances, and when counting a battery of code-like lines by the
  * table agrees with wrapping them. Shaping is the measurer's (#2660 item 2, step D2), which reports the vertical
  * offset of every glyph, so a run that sets a glyph off the baseline is refused as it was when AWT shaped it.
  */
class LigatureCertificateSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val measurer  = TestMeasure.measurer
  private val config    = FontLoader.FontConfig()
  private val printable = (0x20 to 0x7e).map(_.toChar)
  private val sizes     = List(11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f, 18.0f)

  private def ligatureFont(sizePt: Float): FontSpec =
    FontSpec.fromAwt(FontLoader.previewCodeFont(config.copy(fontSize = sizePt, enableLigatures = true)))

  private val honest = LigatureCertificate.measuredShaper(ligatureFont(12.0f), measurer)

  private def rows(font: FontSpec): LigatureCertificate.RowsOf =
    val cache = TestMeasure.wrapCache()
    (text, widthPx) =>
      TextLayoutSnapshot.boundedVisualLinesForText(text, 0, widthPx, font.toAwt, wrapCache = cache).length.max(1)

  "A code font set with ligatures" should "be granted a certificate at every size, including those the cell grid drifts from" in
    sizes.foreach(size =>
      withClue(s"$size pt: ")(LigatureCertificate.of(ligatureFont(size), measurer) shouldBe defined)
    )

  it should "list, for each printable character, the advance the measured path gives it" in
    sizes.foreach { size =>
      val font     = ligatureFont(size)
      val resolver = TextCaretMeasurement.singleFontResolver(font)
      val table    = LigatureCertificate.of(font, measurer).value
      printable.foreach { char =>
        val measured = GlyphAdvances.measure(char.toString, 0, 1, 0, resolver, TestMeasure.measurer).caretsFrom(0, 1)(1)
        withClue(s"$size pt, '$char': ")(table.advanceOf(char) shouldBe measured)
      }
    }

  it should "call a line one row exactly as the table of the same font without ligatures does" in {
    val font    = ligatureFont(12.0f)
    val plain   = FontSpec.fromAwt(FontLoader.previewCodeFont(config.copy(fontSize = 12.0f, enableLigatures = false)))
    val granted = LigatureCertificate.of(font, measurer).value
    val exact   = NominalAdvances.of(plain, measurer).value
    List("a -> b => c", "x" * 90, "  val value = a |> f >>= g").foreach { text =>
      List(300, 800, 1472).foreach { width =>
        withClue(s"$width px, '$text': ")(granted.fitsOneRow(text, width) shouldBe exact.fitsOneRow(text, width))
      }
    }
  }

  it should "be refused when shaping a pair sets the run off the width of its characters" in {
    val widened: LigatureCertificate.Shaper = text =>
      val run = honest(text)
      if text == "=>" then run.copy(width = run.width + 0.5f) else run
    LigatureCertificate.of(ligatureFont(12.0f), measurer, widened, rows(ligatureFont(12.0f))) shouldBe None
  }

  it should "be refused when shaping a triple of symbols sets the run off the width of its characters" in {
    val narrowed: LigatureCertificate.Shaper = text =>
      val run = honest(text)
      if text == "<$>" then run.copy(width = run.width - 0.5f) else run
    LigatureCertificate.of(ligatureFont(12.0f), measurer, narrowed, rows(ligatureFont(12.0f))) shouldBe None
  }

  it should "be refused when shaping a triple of symbols puts a glyph off the baseline" in {
    val raised: LigatureCertificate.Shaper = text =>
      val run = honest(text)
      if text == "<$>" then run.copy(offBaseline = true) else run
    LigatureCertificate.of(ligatureFont(12.0f), measurer, raised, rows(ligatureFont(12.0f))) shouldBe None
  }

  "The measurer" should "report a baseline offset for every glyph it shapes, and none after the last" in {
    val run = measurer.shape("e\u0323", 0, TextMeasurer.FontRun(0, 2, ligatureFont(12.0f)), 0)
    run.ys.length shouldBe run.glyphs.length + 1
    run.ys.last shouldBe 0.0f
  }

  it should "be refused when a code-like line the table calls one row wraps to more" in {
    LigatureCertificate.of(ligatureFont(12.0f), measurer, honest, (_, _) => 2) shouldBe None
  }

  it should "be checked against the wrap on lines either side of their own width" in {
    val asked = scala.collection.mutable.ListBuffer.empty[(String, Int)]
    LigatureCertificate.of(
      ligatureFont(12.0f),
      measurer,
      honest,
      (text, width) =>
        asked += text -> width
        1
    )
    asked.map(_._1).distinct.size should be >= 150
    asked.map(_._2).distinct.size should be >= 300
  }

  "The proportional text font" should "have no certificate" in {
    LigatureCertificate.of(FontSpec.fromAwt(FontLoader.previewTextFont(config)), measurer) shouldBe None
  }
