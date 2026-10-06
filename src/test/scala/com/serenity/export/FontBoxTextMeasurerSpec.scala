package com.serenity.`export`

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.manuscript.typography.{FaceStyle, FontFamily, FontSpec, MeasureError}
import org.apache.fontbox.ttf.TTFParser
import org.apache.pdfbox.io.RandomAccessReadBuffer
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FontBoxTextMeasurerSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val styles = FaceStyle.values.toList

  private def spec(size: Float): FontSpec = FontSpec(FontFamily.CourierPrime, FaceStyle.Regular, size)

  private def measured[A](measure: com.serenity.manuscript.typography.TextMeasurer => A): A =
    FontBoxTextMeasurer.resource().use(measurer => IO(measure(measurer))).unsafeRunSync()

  "the bundled fonts" should "load from the classpath, every face of every family" in {
    for
      family <- FontFamily.values.toList
      style  <- styles
    do withClue(family.resource(style))(BundledFonts.read(family, style).unsafeRunSync().length should be > 10000)
  }

  it should "fail with a named error when a face is not on the classpath" in {
    val result = BundledFonts.readResource("/fonts/NoSuchFace.ttf").attempt.unsafeRunSync()

    result.left.value shouldBe FontLoadError("/fonts/NoSuchFace.ttf", "not on the classpath")
  }

  "FontBoxTextMeasurer" should "measure each code point as its hmtx advance times size over unitsPerEm" in {
    val font = TTFParser().parse(
      RandomAccessReadBuffer(BundledFonts.read(FontFamily.CourierPrime, FaceStyle.Regular).unsafeRunSync())
    )
    val lookup = font.getUnicodeCmapLookup
    val upem   = font.getUnitsPerEm
    val text   = "Aiw. #"
    val expected =
      text.codePoints.toArray.toList.map(cp => font.getAdvanceWidth(lookup.getGlyphId(cp)).toFloat * 12f / upem)
    font.close()

    measured(_.advances(text, spec(12f))).value.toList shouldBe expected
  }

  it should "give Courier Prime a 0.6 em advance, so 12 pt is 7.2 pt a character" in {
    measured(_.advances("iW m", spec(12f))).value.toList.map(a => math.round(a * 100) / 100f) shouldBe
      List.fill(4)(7.2f)
  }

  it should "scale advances linearly with the size" in {
    val (small, large) = measured(m => (m.advances("Hello", spec(10f)), m.advances("Hello", spec(20f))))

    large.value.toList shouldBe small.value.toList.map(_ * 2f)
  }

  it should "report a missing glyph as an error instead of measuring zero" in {
    measured(_.advances("ab中", spec(12f))).left.value shouldBe
      MeasureError.MissingGlyph(0x4e2d, FontFamily.CourierPrime, FaceStyle.Regular)
  }

  it should "name a code point outside the BMP by its full value" in {
    measured(_.advances("a😀", spec(12f))).left.value shouldBe
      MeasureError.MissingGlyph(0x1f600, FontFamily.CourierPrime, FaceStyle.Regular)
  }

  it should "measure the empty string as no advances" in {
    measured(_.advances("", spec(12f))).value.length shouldBe 0
  }

  it should "report line metrics in points from the hhea table" in {
    val font = TTFParser().parse(
      RandomAccessReadBuffer(BundledFonts.read(FontFamily.CourierPrime, FaceStyle.Bold).unsafeRunSync())
    )
    val upem            = font.getUnitsPerEm.toFloat
    val hhea            = font.getHorizontalHeader
    val expectedAscent  = hhea.getAscender * 12f / upem
    val expectedDescent = -hhea.getDescender * 12f / upem
    val expectedGap     = hhea.getLineGap * 12f / upem
    font.close()

    val metrics = measured(_.lineMetrics(FontSpec(FontFamily.CourierPrime, FaceStyle.Bold, 12f))).value

    (metrics.ascent, metrics.descent, metrics.lineGap) shouldBe (expectedAscent, expectedDescent, expectedGap)
    metrics.height shouldBe expectedAscent + expectedDescent + expectedGap
    metrics.height should be > 12f
  }

  it should "measure every face, not just the regular one" in {
    val widths = measured(m => styles.map(style => m.advances("x", FontSpec(FontFamily.CourierPrime, style, 12f))))

    widths.map(_.value.toList.map(a => math.round(a * 100) / 100f)) shouldBe List.fill(4)(List(7.2f))
  }

  it should "refuse bytes that are not a font, naming the resource" in {
    BundledFonts.parse("/fonts/OFL.txt", "not a font".getBytes("UTF-8")).left.value.resource shouldBe "/fonts/OFL.txt"
  }
