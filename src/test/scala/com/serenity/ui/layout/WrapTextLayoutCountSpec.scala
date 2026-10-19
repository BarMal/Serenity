package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.TextAttribute

import scala.util.Random

import com.serenity.state.models.TextVisualLine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/** Wrapping a paragraph must not build a `TextLayout` per row: kerned prose is measured from one shaped stretch, and
  * wraps exactly where per-row measurement ([[LegacyLineMeasurement]]) did.
  */
class WrapTextLayoutCountSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val tolerance = 0.02f

  private def withAttribute(font: Font, attribute: TextAttribute, value: AnyRef): Font =
    font.deriveFont(java.util.Map.of(attribute, value))

  private val kernedSerif =
    withAttribute(Font(Font.SERIF, Font.PLAIN, 15), TextAttribute.KERNING, TextAttribute.KERNING_ON)
  private val kernedSans =
    withAttribute(Font(Font.SANS_SERIF, Font.PLAIN, 13), TextAttribute.KERNING, TextAttribute.KERNING_ON)

  private val kernedProse =
    ("Tales of AVAST waves: Ty and Yo went To Wayne, Va. for War and Peace, while Tom's Tyrannosaurus yawned. " +
      "Wavering voices sang Away, away, over Typhoon Tracks. ") * 6

  /** A row whose width lands within float rounding of the panel's is measured exactly: the search lays out a candidate
    * and doubles it until the row overflows (two layouts when the row is longer than the first candidate, as prose in a
    * narrow-averaged face is), then the chosen row's carets are one more. Which rows land that close depends on the
    * font's metrics, so a platform's face can have one where another has none; a row per paragraph is the cost, a
    * layout per row would be the regression.
    */
  private val MostLayoutsPerParagraph = 3L

  private val kernedFonts = Table("font", kernedSerif, kernedSans)
  private val widths      = Table("width", 97, 233, 480, 900)

  private def constructedWhile[A](body: => A): (A, Long) =
    val before = TextCaretMeasurement.textLayoutsConstructed
    val result = body
    (result, TextCaretMeasurement.textLayoutsConstructed - before)

  private def wrap(text: String, widthPx: Int, font: Font): Vector[TextVisualLine] =
    TextLayoutSnapshot.boundedVisualLinesForText(text, 3, widthPx, font, frc)

  "Wrapping kerned prose" should "build a constant number of TextLayouts however many rows wrap" in
    forAll(kernedFonts) { font =>
      TextLayoutSnapshot.shouldUseMeasuredLayout(font, frc) shouldBe true
      wrap("warm up the font probes", 200, font)
      forAll(widths) { widthPx =>
        val (rows, built) = constructedWhile(wrap(kernedProse, widthPx, font))
        withClue(s"$font at $widthPx px, ${rows.length} rows: ") {
          rows.length should be > 2
          built should be <= MostLayoutsPerParagraph
        }
      }
    }

  private val kernedInter = withAttribute(BundledInterFont.plain(13f), TextAttribute.KERNING, TextAttribute.KERNING_ON)

  it should "pay for the one row that lands on the panel's width, not for every row" in
    forAll(Table("width", 454, 551)) { widthPx =>
      wrap("warm up the font probes", 200, kernedInter)
      val (rows, built) = constructedWhile(wrap(kernedProse, widthPx, kernedInter))
      withClue(s"Inter 13 at $widthPx px, ${rows.length} rows: ") {
        rows.length should be > 10
        built should be <= MostLayoutsPerParagraph
      }
    }

  private def assertWrapsAsPerRowMeasurementDid(text: String, widthPx: Int, font: Font): Unit =
    val rows = wrap(text, widthPx, font)
    val legacy = LegacyLineMeasurement.wrap(
      text,
      3,
      widthPx,
      TextCaretMeasurement.singleFontResolver(font),
      frc,
      measuredLayout = true,
      CellMetrics.fromFont(font)
    )
    withClue(s"$font at $widthPx px, '$text': ") {
      rows.map(row => (row.startColumn, row.endColumn)) shouldBe legacy.map(row => (row.startColumn, row.endColumn))
      rows.zip(legacy).foreach { (row, expected) =>
        row.widthPx shouldBe expected.widthPx +- tolerance
        row.caretStops.map(_.column) shouldBe expected.caretStops.map(_.column)
        row.caretStops
          .zip(expected.caretStops)
          .foreach((stop, reference) => stop.xPx shouldBe reference.xPx +- tolerance)
      }
    }

  it should "wrap exactly where per-row measurement did, with the same caret stops" in
    forAll(kernedFonts) { font =>
      forAll(widths)(widthPx => assertWrapsAsPerRowMeasurementDid(kernedProse, widthPx, font))
    }

  private val vocabulary = Vector(
    "AVAST",
    "Wavering",
    "Tyrannosaurus",
    "To",
    "Yo-yo",
    "office",
    "affluent",
    "waffle",
    "Va.",
    "(Ty)",
    "p.m.",
    "x\ty",
    "日本語の",
    "テキスト、",
    "中文",
    "שלום",
    "مرحبا",
    "👍🏽",
    "🇬🇧",
    "naïve",
    "e\u0301",
    "a\u200bb",
    "https://example.com/Tavern/Yard?v=WAVE",
    "1234.5",
    "—"
  )

  private val allFonts = kernedFonts.toVector ++ Vector(
    withAttribute(Font(Font.SANS_SERIF, Font.PLAIN, 12), TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON),
    withAttribute(
      withAttribute(Font(Font.SERIF, Font.ITALIC, 16), TextAttribute.KERNING, TextAttribute.KERNING_ON),
      TextAttribute.LIGATURES,
      TextAttribute.LIGATURES_ON
    )
  )

  it should "wrap random prose, code, CJK and bidi text exactly as per-row measurement did" in {
    val random = Random(1888)
    (0 until 60).foreach { _ =>
      val words = Vector.fill(10 + random.nextInt(70))(vocabulary(random.nextInt(vocabulary.length)))
      val text  = words.mkString(if random.nextBoolean() then " " else "  ")
      val font  = allFonts(random.nextInt(allFonts.length))
      assertWrapsAsPerRowMeasurementDid(text, 12 + random.nextInt(600), font)
    }
  }
