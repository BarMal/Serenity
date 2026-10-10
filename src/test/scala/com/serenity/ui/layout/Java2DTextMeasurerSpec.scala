package com.serenity.ui.layout

import java.awt.font.{FontRenderContext, TextAttribute}
import java.awt.geom.AffineTransform
import java.awt.{Font, RenderingHints}

import com.serenity.richtext.ParagraphRole
import com.serenity.ui.layout.TextCaretMeasurement.{ColumnFontRun, LineFontResolver}
import com.serenity.ui.layout.TextMeasurer.{FontRun, LineFonts, LineMetrics, MetricsKey, ParagraphDirection}
import com.serenity.ui.renderer.FontSpec
import org.scalacheck.Gen

/** [[Java2DTextMeasurer]] keeps the measurer contract and answers every measurement bit for bit as the Java2D functions
  * it stands in for, at 1x and 2x.
  */
class Java2DTextMeasurerSpec extends TextMeasurerContract:

  private def withAttributes(font: Font, attributes: (TextAttribute, AnyRef)*): Font =
    font.deriveFont(java.util.Map.ofEntries(attributes.map((key, value) => java.util.Map.entry(key, value))*))

  private val awtFonts: Vector[Font] = Vector(
    Font(Font.SANS_SERIF, Font.PLAIN, 13),
    Font(Font.SERIF, Font.ITALIC, 17),
    Font(Font.MONOSPACED, Font.PLAIN, 12),
    Font(Font.DIALOG, Font.BOLD, 1).deriveFont(14.95f),
    withAttributes(Font("Liberation Serif", Font.PLAIN, 14), TextAttribute.KERNING -> TextAttribute.KERNING_ON),
    withAttributes(
      Font(Font.SERIF, Font.PLAIN, 15),
      TextAttribute.KERNING   -> TextAttribute.KERNING_ON,
      TextAttribute.LIGATURES -> TextAttribute.LIGATURES_ON
    ),
    withAttributes(Font(Font.MONOSPACED, Font.PLAIN, 11), TextAttribute.LIGATURES -> TextAttribute.LIGATURES_ON)
  )

  private val oneX = TextLayoutSnapshot.defaultFontRenderContext()

  private val twoX = FontRenderContext(
    AffineTransform.getScaleInstance(2.0, 2.0),
    RenderingHints.VALUE_TEXT_ANTIALIAS_ON,
    RenderingHints.VALUE_FRACTIONALMETRICS_ON
  )

  private val integerMetrics = FontRenderContext(
    AffineTransform(),
    RenderingHints.VALUE_TEXT_ANTIALIAS_ON,
    RenderingHints.VALUE_FRACTIONALMETRICS_OFF
  )

  protected def measurers: Vector[TextMeasurer] = Vector(Java2DTextMeasurer(oneX), Java2DTextMeasurer(twoX))

  protected def fonts: Vector[FontSpec] = awtFonts.map(FontSpec.fromAwt)

  /** A line measured by Java2D, with the render context the direct calls are made at. */
  private val genJava2DLine: Gen[(Line, FontRenderContext)] =
    for
      line <- genLine
      frc  <- Gen.oneOf(oneX, twoX)
    yield (line.copy(measurer = Java2DTextMeasurer(frc)), frc)

  private val genJava2DSpan: Gen[(Line, FontRenderContext, Int, Int)] =
    for
      (line, frc) <- genJava2DLine
      from        <- Gen.choose(0, line.text.length)
      until       <- Gen.choose(from, line.text.length)
    yield (line, frc, from, until)

  private def resolverFor(lineFonts: LineFonts): LineFontResolver =
    LineFontResolver(
      lineFonts.base.toAwt,
      lineFonts.runs.map(run => ColumnFontRun(run.startColumn, run.endColumn, run.font.toAwt))
    )

  /** Everything a [[GlyphAdvances]] answers, floats as raw bits. */
  private def observed(advances: GlyphAdvances): (Vector[Int], Vector[(Int, Boolean)], Boolean) =
    (bits(advances.caretsFrom(0, advances.length)), perCharacter(advances), advances.isContextFree(0, advances.length))

  property("raw caret positions equal TextCaretMeasurement.rawMeasuredCaretXs") {
    forAll(genJava2DLine) { (line, frc) =>
      val expected = TextCaretMeasurement.rawMeasuredCaretXs(line.text, line.startColumn, resolverFor(line.fonts), frc)
      bits(line.measurer.rawCaretXs(line.text, line.startColumn, line.fonts)) shouldBe bits(expected)
    }
  }

  property("glyph advances over any span equal GlyphAdvances.measure") {
    forAll(genJava2DSpan) { (line, frc, from, until) =>
      val expected = GlyphAdvances.measure(line.text, from, until, line.startColumn, resolverFor(line.fonts), frc)
      observed(line.measurer.advances(line.text, from, until, line.startColumn, line.fonts)) shouldBe observed(expected)
    }
  }

  property("itemised runs equal LineFontResolver.fontRuns") {
    forAll(genJava2DSpan) { (line, _, from, until) =>
      val start    = line.startColumn
      val expected = resolverFor(line.fonts).fontRuns(start + from, start + until)
      line.measurer.itemise(line.text, from, until, start, line.fonts) shouldBe
        expected.map(span => FontRun(span.startColumn, span.endColumn, FontSpec.fromAwt(span.font)))
    }
  }

  property("whether text may be measured by advances equals GlyphAdvances.hasContextFreeCharacters") {
    forAll(genText) { text =>
      Java2DTextMeasurer(oneX).mayMeasureByAdvances(text) shouldBe GlyphAdvances.hasContextFreeCharacters(text)
    }
  }

  property("shaping cuts equal ShapingBarriers") {
    forAll(genText, Gen.oneOf(awtFonts), Gen.oneOf(oneX, twoX), Gen.choose(-1, 32)) { (text, font, frc, position) =>
      val measurer = Java2DTextMeasurer(frc)
      val spec     = FontSpec.fromAwt(font)
      measurer.shapingCutAtOrAfter(text, position, spec) shouldBe
        ShapingBarriers.cutAtOrAfter(text, position, font, frc)
      measurer.shapingCutAtOrBefore(text, position, spec) shouldBe
        ShapingBarriers.cutAtOrBefore(text, position, font, frc)
    }
  }

  property("line metrics over any column span equal LineFontResolver.lineMetrics") {
    forAll(genJava2DSpan) { (line, frc, from, until) =>
      val (startColumn, endColumn) = (line.startColumn + from, line.startColumn + until + 1)
      val (heightPx, ascentPx)     = resolverFor(line.fonts).lineMetrics(frc, startColumn, endColumn)
      line.measurer.lineMetrics(line.fonts, startColumn, endColumn) shouldBe LineMetrics(heightPx, ascentPx)
    }
  }

  property("the measured-layout decision equals TextLayoutSnapshot.shouldUseMeasuredLayout") {
    forAll(Gen.oneOf(awtFonts), Gen.oneOf(oneX, twoX)) { (font, frc) =>
      Java2DTextMeasurer(frc).measuresProportionally(FontSpec.fromAwt(font)) shouldBe
        TextLayoutSnapshot.shouldUseMeasuredLayout(font, frc)
    }
  }

  property("cell metrics equal CellMetrics.fromFont") {
    forAll(Gen.oneOf(awtFonts), Gen.oneOf(oneX, twoX)) { (font, frc) =>
      Java2DTextMeasurer(frc).cellMetrics(FontSpec.fromAwt(font)) shouldBe CellMetrics.fromFont(font)
    }
  }

  property("a glyph's width equals DropCapLayout.glyphWidthPx") {
    forAll(genText, Gen.oneOf(awtFonts), Gen.oneOf(oneX, twoX)) { (text, font, frc) =>
      val width = Java2DTextMeasurer(frc).glyphWidthPx(FontSpec.fromAwt(font), text)
      java.lang.Float.floatToRawIntBits(width) shouldBe
        java.lang.Float.floatToRawIntBits(DropCapLayout.glyphWidthPx(font, frc, text))
    }
  }

  property("measurers share a metrics key exactly when their render contexts are equal") {
    val renderContexts = Vector(oneX, twoX, integerMetrics)
    for
      left  <- renderContexts
      right <- renderContexts
    do
      val sameKey = Java2DTextMeasurer(left).metricsKey == Java2DTextMeasurer(right).metricsKey
      withClue(s"$left vs $right: ")(sameKey shouldBe (left == right))
    val rebuilt = FontRenderContext(oneX.getTransform, oneX.getAntiAliasingHint, oneX.getFractionalMetricsHint)
    Java2DTextMeasurer(rebuilt).metricsKey shouldBe Java2DTextMeasurer(oneX).metricsKey
    Java2DTextMeasurer(rebuilt).metricsKey.hashCode shouldBe Java2DTextMeasurer(oneX).metricsKey.hashCode
  }

  property("metrics keys differ by fallback chain and by paragraph direction") {
    val key = Java2DTextMeasurer(oneX).metricsKey
    key.copy(fallbackChain = Vector("Noto Sans CJK")) should not be key
    key.copy(direction = ParagraphDirection.RightToLeft) should not be key
    key.copy(fallbackChain = Vector("a")) shouldBe key.copy(fallbackChain = Vector("a"))
    key shouldBe MetricsKey("java2d", Vector(oneX))
  }

  property("shaped runs equal Font.layoutGlyphVector at the level's direction") {
    forAll(genJava2DSpan, Gen.choose(0, 3)) {
      case ((line, frc, from, until), level) =>
        val runs = line.measurer.itemise(line.text, from, until, line.startColumn, line.fonts)
        runs.foreach { run =>
          val chars     = line.text.substring(run.startColumn - line.startColumn, run.endColumn - line.startColumn)
          val flags     = if level % 2 == 0 then Font.LAYOUT_LEFT_TO_RIGHT else Font.LAYOUT_RIGHT_TO_LEFT
          val direct    = run.font.toAwt.layoutGlyphVector(frc, chars.toCharArray, 0, chars.length, flags)
          val count     = direct.getNumGlyphs
          val positions = direct.getGlyphPositions(0, count + 1, new Array[Float](2 * (count + 1)))
          val shaped    = line.measurer.shape(line.text, line.startColumn, run, level.toByte)
          shaped.glyphs.toVector shouldBe direct.getGlyphCodes(0, count, new Array[Int](count)).toVector
          shaped.clusters.toVector shouldBe direct.getGlyphCharIndices(0, count, new Array[Int](count)).toVector
          bits(shaped.xs) shouldBe Vector.tabulate(count + 1)(i => java.lang.Float.floatToRawIntBits(positions(2 * i)))
          bits(shaped.ys) shouldBe
            Vector.tabulate(count + 1)(i => java.lang.Float.floatToRawIntBits(positions(2 * i + 1)))
        }
    }
  }

  private def keyFor(line: Line, frc: FontRenderContext, measured: Boolean, cell: CellMetrics, width: Int) =
    WrappedLineKey(
      line.text,
      width,
      resolverFor(line.fonts),
      frc,
      measured,
      cell,
      line.startColumn,
      ParagraphRole.Body,
      0.0f
    )

  private val genCell: Gen[CellMetrics] =
    for
      charWidth <- Gen.choose(1, 20)
      wide      <- Gen.oneOf(true, false)
    yield CellMetrics(charWidth, 16, 12, wide)

  property("caret stops equal TextCaretMeasurement.caretXs on the grid and on the measured path") {
    forAll(genJava2DLine, Gen.oneOf(true, false), genCell) { (pair, measured, cell) =>
      val (line, frc) = pair
      val expected = TextCaretMeasurement.caretXs(
        line.text,
        line.startColumn,
        resolverFor(line.fonts),
        frc,
        measured,
        cell
      )
      bits(line.measurer.caretXs(line.text, line.startColumn, line.fonts, measured, cell)) shouldBe bits(expected)
    }
  }

  property("exact carets equal the layout carets raw measurement falls back to") {
    forAll(genJava2DLine) { (line, frc) =>
      whenever(!GlyphAdvances.hasContextFreeCharacters(line.text)) {
        bits(line.measurer.exactCaretXs(line.text, line.startColumn, line.fonts)) shouldBe
          bits(TextCaretMeasurement.rawMeasuredCaretXs(line.text, line.startColumn, resolverFor(line.fonts), frc))
      }
    }
  }

  property("an exact fit equals the length of TextCaretMeasurement.fittingSegment's exact search") {
    forAll(genJava2DLine, Gen.choose(0, 400), genCell) { (pair, width, cell) =>
      val (line, frc) = pair
      whenever(line.text.nonEmpty) {
        val from = line.text.length / 2
        val key  = keyFor(line, frc, measured = true, cell, width)
        val expected = TextCaretMeasurement
          .fittingSegment(ParagraphMeasurement.assembled(key, None), line.text, from, width, line.startColumn, key)
          .length
        line.measurer.exactFit(line.text, from, width, line.startColumn, line.fonts, cell) shouldBe expected
      }
    }
  }
