package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.TextAttribute

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.TextCaretMeasurement.singleFontResolver
import com.serenity.ui.renderer.{FontProbes, ShapedTextCache}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** The bundled code font with ligatures on keeps the cell grid's widths while its contextual alternates swap glyphs, so
  * a line of it is measured from one shaped run and its rows and caret stops are the ones per-row `TextLayout`
  * measurement ([[LegacyLineMeasurement]]) finds.
  */
class LigatureFontMeasurementSpec extends AnyFlatSpec with Matchers:

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val tolerance = 0.02f

  private val codeFont = FontLoader.loadCodeFont(FontConfig(fontSize = 12.0f)).unsafeRunSync()

  private val codeLines = Vector(
    "val x = a => b != c // note",
    "def total(xs: List[Int]): Int = xs.map(_ + 1).filter(_ >= 2).sum",
    "if a == b && c <= d || e >= f then run(a, b) else stop()",
    "xs |> map(f) |> filter(g) :: rest -> next <- prev ... === !== ::: <=> |||",
    "for i <- 0 until 10 do report(s\"item $i of ${xs.length}\")",
    "https://www.example.com/path?query=value&other=1#fragment",
    "The quick brown fox jumps over the lazy dog, then does it all again until it wraps.",
    "    /* indented block comment */ val longName = anotherLongName.method(argumentOne, argumentTwo, argumentThree)"
  )

  private val widths = Seq(40, 97, 233, 480, 1200)

  private def resolver = singleFontResolver(codeFont)

  private def keyFor(text: String, widthPx: Int): WrappedLineKey =
    WrappedLineKey(
      text,
      widthPx,
      resolver,
      frc,
      measuredLayout = true,
      CellMetrics.fromFont(codeFont),
      0,
      com.serenity.richtext.ParagraphRole.Body,
      0.0f
    )

  private def assertSameStops(actual: Vector[TextCaretStop], expected: Vector[TextCaretStop]): Unit =
    actual.map(_.column) shouldBe expected.map(_.column)
    actual.zip(expected).foreach((stop, reference) => stop.xPx shouldBe reference.xPx +- tolerance)

  private def assertSameRows(actual: Vector[TextVisualLine], expected: Vector[TextVisualLine]): Unit =
    actual.map(row => (row.startColumn, row.endColumn, row.text)) shouldBe
      expected.map(row => (row.startColumn, row.endColumn, row.text))
    actual.zip(expected).foreach { (row, reference) =>
      withClue(s"row '${row.text}': ") {
        row.widthPx shouldBe reference.widthPx +- tolerance
        assertSameStops(row.caretStops, reference.caretStops)
        assertSameStops(row.xSortedCaretStops, reference.xSortedCaretStops)
      }
    }

  "The bundled code font with ligatures on" should "still be measured, not laid out on the cell grid" in {
    FontLoader.ligaturesEnabled(codeFont) shouldBe true
    TextLayoutSnapshot.shouldUseMeasuredLayout(codeFont, frc, FontProbes()) shouldBe true
  }

  it should "wrap code exactly where per-row TextLayout measurement did, with the same caret stops" in
    codeLines.foreach { line =>
      widths.foreach { widthPx =>
        withClue(s"'$line' at $widthPx px: ") {
          assertSameRows(
            TextLayoutSnapshot.boundedVisualLinesForText(line, 3, widthPx, codeFont, frc),
            LegacyLineMeasurement.wrap(
              line,
              3,
              widthPx,
              resolver,
              frc,
              measuredLayout = true,
              CellMetrics.fromFont(codeFont)
            )
          )
        }
      }
    }

  it should "give a single segment the carets TextLayout reports at every character" in
    codeLines.foreach { line =>
      val xs = TextLayoutSnapshot.caretXsForText(line, codeFont, ShapedTextCache(), frc)
      val reference =
        LegacyLineMeasurement.caretXs(line, 0, resolver, frc, true, CellMetrics.fromFont(codeFont))
      xs.length shouldBe reference.length
      xs.zip(reference).foreach((x, expected) => x shouldBe expected +- tolerance)
    }

  it should "keep every character of a code line context-free, so no row falls back to a TextLayout" in
    codeLines.foreach { line =>
      val advances = GlyphAdvances.measure(line, 0, line.length, 0, resolver, frc, ShapedTextCache())

      withClue(s"'$line': ") {
        advances.isContextFree(0, line.length) shouldBe true
      }
    }

  it should "fit a row of code from the measured advances rather than the per-row search" in
    codeLines.foreach { line =>
      val paragraph = ParagraphMeasurement(keyFor(line, 233), 0, Int.MaxValue, ShapedTextCache())

      withClue(s"'$line': ") {
        paragraph.measuredFit(0, 233) should not be empty
      }
    }

  it should "shape a line through the shaped-text cache once, however many windows measure it" in {
    val cache = ShapedTextCache()
    val line  = codeLines(1)

    val _ = GlyphAdvances.measure(line, 0, line.length, 0, resolver, frc, cache)
    val _ = GlyphAdvances.measure(line, 0, line.length, 0, resolver, frc, cache)

    cache.shapings shouldBe 1L
    cache.holds(line, codeFont, frc) shouldBe true
  }

  private def withFeatures(font: Font, features: (TextAttribute, AnyRef)*): Font =
    font.deriveFont(java.util.Map.copyOf(java.util.Map.ofEntries(features.map((k, v) => java.util.Map.entry(k, v))*)))

  "A kerned proportional font" should "keep the characters its kerning moves on the per-row path" in {
    val kerned = withFeatures(Font(Font.SERIF, Font.PLAIN, 15), TextAttribute.KERNING -> TextAttribute.KERNING_ON)
    val line   = "AVAVAVA WAWAWA To Ty Yo"

    val advances = GlyphAdvances.measure(line, 0, line.length, 0, singleFontResolver(kerned), frc, ShapedTextCache())

    GlyphAdvances.advancesAreUniform(kerned, frc, FontProbes()) shouldBe false
    advances.isContextFree(0, line.length) shouldBe false
  }
