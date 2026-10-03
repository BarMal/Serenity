package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute}

import com.serenity.richtext.{
  InlineMark,
  ParagraphAlignment,
  ParagraphRole,
  RichTextDocument,
  RichTextParagraph,
  RichTextRun,
  RichTextStyle
}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.TextCaretMeasurement.{resolverForLine, singleFontResolver}
import com.serenity.ui.theme.RichTextStyling
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/** Measuring a paragraph once and slicing rows out of it must wrap exactly where measuring every row with its own
  * `TextLayout` did ([[LegacyLineMeasurement]]), with caret positions equal up to float rounding.
  */
class ParagraphMeasurementSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:

  given Balance = Balance.default

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val tolerance = 0.02f

  private def withAttribute(font: Font, attribute: TextAttribute, value: AnyRef): Font =
    font.deriveFont(java.util.Map.of(attribute, value))

  private val sans = Font(Font.SANS_SERIF, Font.PLAIN, 13)
  private val ligatured =
    withAttribute(Font(Font.SANS_SERIF, Font.PLAIN, 12), TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
  private val kerned = withAttribute(Font(Font.SERIF, Font.PLAIN, 15), TextAttribute.KERNING, TextAttribute.KERNING_ON)
  private val mono   = Font(Font.MONOSPACED, Font.PLAIN, 12)

  private val measuredFonts = Table(
    "font",
    sans,
    ligatured,
    kerned,
    Font(Font.SERIF, Font.ITALIC, 17),
    Font(Font.DIALOG, Font.PLAIN, 1).deriveFont(14.95f),
    Font(Font.SERIF, Font.BOLD, 1).deriveFont(9.3f),
    withAttribute(mono, TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
  )

  private val prose =
    "The quick brown fox jumps over the lazy dog, again and again, until the row finally has to wrap somewhere."

  private val corpus = Table(
    "line",
    "",
    "a",
    "     ",
    prose,
    "AVAVAVA WAWAWA To Ty Yo -- kerning pairs repeated across the wrap point Tw Te Ta AV AW AY " * 3,
    "office affluent fluffy fjord efficient waffle shuffle ffi ffl -> => != <= " * 3,
    "Supercalifragilisticexpialidocious" * 5,
    "tab\tseparated\tcolumns\t\tdouble\t\t\ttriple\ttabs\tthat\twrap\tacross\tthe\tpanel\twidth",
    "zero​width​spaces​between​every​word " * 3,
    "non breaking spaces hold these words together " * 3,
    "été café résumé naïve with combining accents " * 3,
    "Family 👨‍👩‍👧‍👦 flags 🇬🇧🇯🇵🇫🇷 thumbs 👍🏽 and more emoji 🎉🎉🎉 to wrap " * 3,
    "日本語のテキストは空白なしで折り返されるので、行の途中で改行できる必要があります。" * 2,
    "中文混合 English words 和标点符号，测试换行。" * 3,
    "مرحبا بالعالم هذا نص عربي طويل يلتف عبر عدة أسطر " * 2,
    "Mixed שלום עולם with Hebrew and English words in one paragraph " * 2,
    "1234567890 !@#$%^&*() [braces] {curly} <angle> 'quotes' \"double\" " * 3
  )

  private val widths = Seq(7, 40, 97, 160, 233, 480, 1200)

  private def assertSameRows(actual: Vector[TextVisualLine], expected: Vector[TextVisualLine]): Assertion =
    withClue(s"rows ${actual.map(_.text)} vs ${expected.map(_.text)}: ") {
      actual.map(row => (row.bufferLine, row.startColumn, row.endColumn, row.text, row.heightPx, row.ascentPx)) shouldBe
        expected.map(row => (row.bufferLine, row.startColumn, row.endColumn, row.text, row.heightPx, row.ascentPx))
    }
    actual.zip(expected).foreach { (row, legacy) =>
      withClue(s"row '${row.text}': ") {
        row.widthPx shouldBe legacy.widthPx +- tolerance
        row.xOffsetPx shouldBe legacy.xOffsetPx +- tolerance
        assertSameStops(row.caretStops, legacy.caretStops)
        assertSameStops(row.xSortedCaretStops, legacy.xSortedCaretStops)
      }
    }
    succeed

  private def assertSameStops(actual: Vector[TextCaretStop], expected: Vector[TextCaretStop]): Unit =
    actual.map(_.column) shouldBe expected.map(_.column)
    actual.zip(expected).foreach((stop, legacy) => stop.xPx shouldBe legacy.xPx +- tolerance)

  private def legacyLine(
    text: String,
    widthPx: Int,
    font: Font,
    cellMetrics: CellMetrics,
    measured: Boolean,
    maxVisualLines: Int = Int.MaxValue
  ): Vector[TextVisualLine] =
    LegacyLineMeasurement.wrap(
      text,
      3,
      widthPx,
      singleFontResolver(font),
      frc,
      measured,
      cellMetrics,
      maxVisualLines = maxVisualLines
    )

  "Per-paragraph measurement" should "wrap measured plain text exactly where per-row measurement did" in
    forAll(measuredFonts) { font =>
      TextLayoutSnapshot.shouldUseMeasuredLayout(font, frc) shouldBe true
      forAll(corpus) { line =>
        widths.foreach { widthPx =>
          withClue(s"$font at $widthPx px: ") {
            assertSameRows(
              TextLayoutSnapshot.boundedVisualLinesForText(line, 3, widthPx, font, frc),
              legacyLine(line, widthPx, font, CellMetrics.fromFont(font), measured = true)
            )
          }
        }
      }
    }

  it should "measure single segments as per-row measurement did" in
    forAll(measuredFonts) { font =>
      forAll(corpus) { line =>
        val xs = TextLayoutSnapshot.caretXsForText(line, font, frc)
        val legacy =
          LegacyLineMeasurement.caretXs(line, 0, singleFontResolver(font), frc, true, CellMetrics.fromFont(font))
        xs.length shouldBe legacy.length
        xs.zip(legacy).foreach((x, expected) => x shouldBe expected +- tolerance)
        assertSameRows(
          Vector(TextLayoutSnapshot.visualLineForText(line, 2, font, frc, startColumn = 5)),
          Vector(
            LegacyLineMeasurement.shapeSegment(
              line,
              2,
              5,
              5 + line.length,
              singleFontResolver(font),
              frc,
              true,
              CellMetrics.fromFont(font)
            )
          )
        )
      }
    }

  it should "wrap cell layouts, uniform and display-width aware, exactly as before" in {
    val cellLayouts =
      Seq(CellMetrics.fromFont(mono), CellMetrics(1, 1, 0), CellMetrics(1, 1, 0, displayWidthAware = true))
    cellLayouts.foreach { cellMetrics =>
      forAll(corpus) { line =>
        Seq(1, 7, 40, 97, 480).foreach { widthPx =>
          withClue(s"$cellMetrics at $widthPx: ") {
            val rows = TextLayoutSnapshot.boundedVisualLinesForText(
              line,
              3,
              widthPx,
              mono,
              frc,
              cellMetricsOverride = Some(cellMetrics),
              forceCellLayout = true
            )
            rows shouldBe legacyLine(line, widthPx, mono, cellMetrics, measured = false)
          }
        }
      }
    }
  }

  it should "stop at a row limit with the same leading rows, however long the paragraph" in {
    val long = (prose + " office affluent 👍🏽 日本語 ") * 220
    long.length should be > 20000
    Seq(sans, ligatured, kerned).foreach { font =>
      Seq(97, 480).foreach { widthPx =>
        Seq(1, 3, 40).foreach { limit =>
          assertSameRows(
            TextLayoutSnapshot.boundedVisualLinesForText(long, 3, widthPx, font, frc, maxVisualLines = limit),
            legacyLine(long, widthPx, font, CellMetrics.fromFont(font), measured = true, maxVisualLines = limit)
          )
        }
      }
      assertSameRows(
        TextLayoutSnapshot.boundedVisualLinesForText(long, 3, 1200, font, frc),
        legacyLine(long, 1200, font, CellMetrics.fromFont(font), measured = true)
      )
    }
  }

  private def run(text: String, style: RichTextStyle = RichTextStyle.empty): RichTextRun = RichTextRun(text, style)

  private val richParagraphs = List(
    RichTextParagraph(
      List(
        run("Plain start then "),
        run("bold words ", RichTextStyle.empty.withMark(InlineMark.Bold)),
        run("and a much larger run of text ", RichTextStyle.empty.withFontSize(22.0f)),
        run("tiny tail that keeps going and going", RichTextStyle.empty.withFontSize(9.0f))
      )
    ),
    RichTextParagraph(List(run(prose, RichTextStyle.empty.withMark(InlineMark.Italic))), ParagraphAlignment.Center),
    RichTextParagraph(
      List(run("Drop cap paragraph "), run(prose + " " + prose, RichTextStyle.empty.withMark(InlineMark.Bold))),
      role = ParagraphRole.dropCap(lines = 3)
    ),
    RichTextParagraph(
      List(run("Serif family run " * 4, RichTextStyle(fontFamily = Some(Font.SERIF)))),
      ParagraphAlignment.Right
    ),
    RichTextParagraph(
      List(run("office affluent "), run("emoji 👍🏽 日本語 עברית", RichTextStyle.empty.withFontSize(17.0f)))
    ),
    RichTextParagraph(List(run("")))
  )

  private def richBuffer(visibleLines: Int = 400): Buffer =
    val document = RichTextDocument(richParagraphs)
    Buffer
      .fromString(BufferId(9), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = visibleLines)
      )

  private def legacyBufferRows(
    buffer: Buffer,
    widthPx: Int,
    font: Font,
    proseScale: Float,
    dropCapsEnabled: Boolean
  ): Vector[TextVisualLine] =
    val document = buffer.richText.richTextDocument
    buffer.document.content.linesIteratorFrom(0).toVector.flatMap { (lineIndex, line) =>
      val paragraph = document.flatMap(_.paragraphAt(lineIndex))
      val role    = RichTextStyling.effectiveRole(paragraph.map(_.role).getOrElse(ParagraphRole.Body), dropCapsEnabled)
      val glyphPx = DropCapLayout.measuredGlyphWidthPx(font, frc, document, lineIndex, role, proseScale)
      val rows = LegacyLineMeasurement.wrap(
        line,
        lineIndex,
        widthPx,
        resolverForLine(font, document, lineIndex, line.length, proseScale),
        frc,
        measuredLayout = true,
        CellMetrics.fromFont(font),
        paragraphRole = role,
        dropCapGlyphWidthPx = glyphPx
      )
      rows.map(row => aligned(row, paragraph.map(_.alignment).getOrElse(ParagraphAlignment.Left), widthPx))
    }

  private def aligned(row: TextVisualLine, alignment: ParagraphAlignment, widthPx: Int): TextVisualLine =
    val available = math.max(0.0f, widthPx.toFloat - row.widthPx)
    val offset = alignment match
      case ParagraphAlignment.Center => available / 2.0f
      case ParagraphAlignment.Right  => available
      case _                         => 0.0f
    if offset <= 0.0f then row
    else
      row.copy(
        caretStops = row.caretStops.map(stop => stop.copy(xPx = stop.xPx + offset)),
        xSortedCaretStops = row.xSortedCaretStops.map(stop => stop.copy(xPx = stop.xPx + offset)),
        xOffsetPx = offset
      )

  it should "wrap rich text with per-run fonts, prose zoom, alignment and drop caps exactly as before" in
    Seq(sans, ligatured, kerned).foreach { font =>
      Seq(1.0f, 1.37f).foreach { proseScale =>
        Seq(true, false).foreach { dropCaps =>
          Seq(97, 233, 480).foreach { widthPx =>
            withClue(s"$font zoom $proseScale drop caps $dropCaps at $widthPx: ") {
              val snapshot =
                TextLayoutSnapshot.fromBuffer(
                  richBuffer(),
                  widthPx,
                  font,
                  frc,
                  proseScale = proseScale,
                  dropCapsEnabled = dropCaps
                )
              assertSameRows(snapshot.visualLines, legacyBufferRows(richBuffer(), widthPx, font, proseScale, dropCaps))
            }
          }
        }
      }
    }

  it should "split rich text into the same columns as before" in
    Seq(sans, ligatured).foreach { font =>
      val buffer = richBuffer(visibleLines = 4)
      val chunks = TextLayoutSnapshot.columnChunksForBuffer(buffer, 160, font, frc, columnCount = 3)
      chunks.map(_.length).sum shouldBe 12
      assertSameRows(chunks.flatten, legacyBufferRows(buffer, 160, font, 1.0f, dropCapsEnabled = true).take(12))
    }

  private def contextFree(text: String, font: Font): Vector[Boolean] =
    val advances = GlyphAdvances.measure(text, 0, text.length, 0, singleFontResolver(font), frc)
    text.indices.toVector.map(index => advances.isContextFree(index, index + 1))

  "Glyph advances" should "treat plain text and tabs as context-free" in {
    contextFree("plain words\tand\t\ttabs, digits 123 and punctuation!", sans).forall(identity) shouldBe true
    contextFree("plain words\tand\t\ttabs", ligatured).forall(identity) shouldBe true
  }

  it should "need per-row measurement for kerning pairs, complex scripts, surrogates and format characters" in {
    contextFree("WAVE", sans).forall(identity) shouldBe true
    contextFree("WAVE", kerned).contains(false) shouldBe true
    contextFree("abc مرحبا", sans).drop(4).contains(true) shouldBe false
    contextFree("a👍🏽b", sans) shouldBe Vector(true, false, false, false, false, true)
    contextFree("a\u200bb", sans) shouldBe Vector(true, false, true)
  }

  it should "need per-row measurement wherever a ligature forms" in {
    val text    = "fi fl"
    val chars   = text.toCharArray
    val nominal = ligatured.createGlyphVector(frc, chars).getNumGlyphs
    val shaped  = ligatured.layoutGlyphVector(frc, chars, 0, chars.length, Font.LAYOUT_LEFT_TO_RIGHT).getNumGlyphs
    val flags   = contextFree(text, ligatured)
    if shaped < nominal then flags.contains(false) shouldBe true else flags.forall(identity) shouldBe true
    contextFree(text, sans).forall(identity) shouldBe true
  }

  "Grapheme boundaries" should "come from one sweep, matching a scan cluster by cluster" in
    forAll(corpus) { line =>
      ParagraphMeasurement.graphemeBoundaryOffsets(line).toVector shouldBe LegacyLineMeasurement
        .graphemeBoundaryOffsets(line)
    }

  "The default font render context" should "be built once and shared" in {
    TextLayoutSnapshot.defaultFontRenderContext() should be theSameInstanceAs TextLayoutSnapshot
      .defaultFontRenderContext()
  }

  it should "keep antialiased fractional metrics" in {
    val context: FontRenderContext = TextLayoutSnapshot.defaultFontRenderContext()
    context.isAntiAliased shouldBe true
    context.usesFractionalMetrics shouldBe true
  }
