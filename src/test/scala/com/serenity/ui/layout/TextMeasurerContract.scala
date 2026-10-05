package com.serenity.ui.layout

import com.serenity.ui.layout.TextMeasurer.{FontRun, LineFonts}
import com.serenity.ui.renderer.FontSpec
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** What wrapping and caret placement rely on from any [[TextMeasurer]], whichever backend measures. A backend's spec
  * extends this and names its measurers and fonts.
  */
trait TextMeasurerContract extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 300)

  protected def measurers: Vector[TextMeasurer]

  protected def fonts: Vector[FontSpec]

  protected val genCluster: Gen[String] =
    Gen.frequency(
      30 -> Gen.alphaChar.map(_.toString),
      12 -> Gen.const(" "),
      3  -> Gen.const("\t"),
      4  -> Gen.oneOf("fi", "ffl", "fl", "AV", "To", "WA", "Ty", "->", "=>", "!=", "<="),
      3  -> Gen.oneOf("é", "ä", "ṇ̃", "é"),
      3  -> Gen.oneOf("😀", "👍🏽", "🇬🇧", "👨‍👩‍👧", "👩‍💻"),
      3  -> Gen.oneOf("مرحبا", "بالعالم", "שלום", "עולם"),
      1  -> Gen.oneOf("日本語", "中文", "​", " ", ".", ",", "1")
    )

  protected val genText: Gen[String] =
    Gen.frequency(1 -> Gen.const(""), 12 -> Gen.choose(1, 30).flatMap(Gen.listOfN(_, genCluster)).map(_.mkString))

  protected def genFont: Gen[FontSpec] = Gen.oneOf(fonts)

  protected def genMeasurer: Gen[TextMeasurer] = Gen.oneOf(measurers)

  /** Font runs over `[startColumn, startColumn + length)`, sometimes none, sometimes leaving gaps on the base font. */
  protected def genLineFonts(startColumn: Int, length: Int): Gen[LineFonts] =
    def runsFrom(column: Int): Gen[List[FontRun]] =
      if column >= startColumn + length then Gen.const(Nil)
      else
        for
          width <- Gen.choose(1, startColumn + length - column)
          gap   <- Gen.choose(0, 2)
          font  <- genFont
          rest  <- runsFrom(column + width + gap)
        yield FontRun(column, column + width, font) :: rest
    for
      base <- genFont
      runs <- Gen.frequency(2 -> Gen.const(Nil), 3 -> runsFrom(startColumn))
    yield LineFonts(base, runs.toVector)

  final protected case class Line(text: String, startColumn: Int, fonts: LineFonts, measurer: TextMeasurer)

  protected def genLine: Gen[Line] =
    for
      text        <- genText
      startColumn <- Gen.choose(0, 5)
      lineFonts   <- genLineFonts(startColumn, text.length)
      measurer    <- genMeasurer
    yield Line(text, startColumn, lineFonts, measurer)

  protected def genSpan: Gen[(Line, Int, Int)] =
    for
      line  <- genLine
      from  <- Gen.choose(0, line.text.length)
      until <- Gen.choose(from, line.text.length)
    yield (line, from, until)

  protected def bits(xs: IArray[Float]): Vector[Int] = xs.toVector.map(java.lang.Float.floatToRawIntBits)

  /** Each character's advance and whether it is context-free, floats as raw bits. */
  protected def perCharacter(advances: GlyphAdvances): Vector[(Int, Boolean)] =
    (0 until advances.length).toVector.map { index =>
      (java.lang.Float.floatToRawIntBits(advances.caretsFrom(index, 1)(1)), advances.isContextFree(index, index + 1))
    }

  property("contract: measurers with equal metrics keys measure alike") {
    forAll(genLine, genMeasurer) { (line, other) =>
      whenever(other.metricsKey == line.measurer.metricsKey) {
        bits(other.rawCaretXs(line.text, line.startColumn, line.fonts)) shouldBe
          bits(line.measurer.rawCaretXs(line.text, line.startColumn, line.fonts))
      }
    }
  }

  property("contract: raw carets hold a stop per character plus the trailing edge") {
    forAll(genLine) { line =>
      line.measurer.rawCaretXs(line.text, line.startColumn, line.fonts).length shouldBe line.text.length + 1
    }
  }

  property("contract: itemised runs tile the span, each non-empty and in a different font from the last") {
    forAll(genSpan) { (line, from, until) =>
      val runs  = line.measurer.itemise(line.text, from, until, line.startColumn, line.fonts)
      val start = line.startColumn + from
      runs.map(_.startColumn) shouldBe (start +: runs.map(_.endColumn)).dropRight(1)
      runs.lastOption.fold(start)(_.endColumn) shouldBe line.startColumn + until
      runs.forall(run => run.endColumn > run.startColumn) shouldBe true
      runs.zip(runs.drop(1)).forall((left, right) => left.font != right.font) shouldBe true
    }
  }

  property("contract: advances cover the span, and a context-free span's carets are its summed advances") {
    forAll(genSpan) { (line, from, until) =>
      val advances = line.measurer.advances(line.text, from, until, line.startColumn, line.fonts)
      advances.length shouldBe until - from
      val span = line.text.substring(from, until)
      if line.measurer.mayMeasureByAdvances(span) && advances.isContextFree(0, advances.length) then
        bits(line.measurer.rawCaretXs(span, line.startColumn + from, line.fonts)) shouldBe
          bits(advances.caretsFrom(0, advances.length))
    }
  }

  property("contract: shaping cuts stay in range and are themselves cuts") {
    forAll(genText, genFont, genMeasurer, Gen.choose(-1, 32)) { (text, font, measurer, position) =>
      val after = measurer.shapingCutAtOrAfter(text, position, font)
      after should (be >= math.min(text.length, math.max(1, position)) and be <= text.length)
      if after < text.length then measurer.shapingCutAtOrAfter(text, after, font) shouldBe after
      val before = measurer.shapingCutAtOrBefore(text, position, font)
      before should (be >= 0 and be <= math.max(0, math.min(position, text.length - 1)))
      if before > 0 then measurer.shapingCutAtOrBefore(text, before, font) shouldBe before
    }
  }

  property("contract: cutting text at a shaping cut leaves every character's advance and context as measured whole") {
    val genCut =
      for
        text     <- genText.suchThat(_.nonEmpty)
        font     <- genFont
        measurer <- genMeasurer
        position <- Gen.choose(0, text.length)
      yield (text, LineFonts(font, Vector.empty), measurer, measurer.shapingCutAtOrAfter(text, position, font))
    forAll(genCut) { (text, lineFonts, measurer, cut) =>
      whenever(measurer.mayMeasureByAdvances(text)) {
        val head = measurer.advances(text, 0, cut, 0, lineFonts)
        val tail = measurer.advances(text, cut, text.length, 0, lineFonts)
        perCharacter(head) ++ perCharacter(tail) shouldBe perCharacter(
          measurer.advances(text, 0, text.length, 0, lineFonts)
        )
      }
    }
  }

  property("contract: line metrics are at least one pixel") {
    forAll(genSpan) { (line, from, until) =>
      val metrics = line.measurer.lineMetrics(line.fonts, line.startColumn + from, line.startColumn + until)
      metrics.heightPx should be >= 1
      metrics.ascentPx should be >= 1
    }
  }

  property("contract: a shaped run holds a position per glyph plus the pen end, and clusters inside the run") {
    forAll(genSpan, Gen.choose(0, 3)) {
      case ((line, from, until), level) =>
        val runs = line.measurer.itemise(line.text, from, until, line.startColumn, line.fonts)
        runs.foreach { run =>
          val shaped = line.measurer.shape(line.text, line.startColumn, run, level.toByte)
          shaped.level shouldBe level.toByte
          shaped.font shouldBe run.font
          shaped.xs.length shouldBe shaped.glyphs.length + 1
          shaped.ys.length shouldBe shaped.glyphs.length + 1
          shaped.clusters.length shouldBe shaped.glyphs.length
          shaped.clusters.forall(cluster => cluster >= 0 && cluster < run.endColumn - run.startColumn) shouldBe true
        }
    }
  }

  property("contract: a shaped run is shaped alike by measurers with equal metrics keys") {
    forAll(genSpan, genMeasurer, Gen.choose(0, 1)) {
      case ((line, from, until), other, level) =>
        whenever(other.metricsKey == line.measurer.metricsKey) {
          line.measurer.itemise(line.text, from, until, line.startColumn, line.fonts).foreach { run =>
            val left  = line.measurer.shape(line.text, line.startColumn, run, level.toByte)
            val right = other.shape(line.text, line.startColumn, run, level.toByte)
            (left.glyphs.toVector, bits(left.xs), bits(left.ys), left.clusters.toVector) shouldBe
              ((right.glyphs.toVector, bits(right.xs), bits(right.ys), right.clusters.toVector))
          }
        }
    }
  }

  property("contract: exact carets hold a stop per character plus the trailing edge") {
    forAll(genLine) { line =>
      line.measurer.exactCaretXs(line.text, line.startColumn, line.fonts).length shouldBe line.text.length + 1
    }
  }

  property("contract: an exact fit takes at least one character and at most the rest of the line") {
    forAll(genLine, Gen.choose(0, 300), Gen.choose(1, 20)) { (line, width, charWidth) =>
      whenever(line.text.nonEmpty) {
        val cell = CellMetrics(charWidth, 16, 12)
        val fit  = line.measurer.exactFit(line.text, 0, width, line.startColumn, line.fonts, cell)
        fit should (be >= 1 and be <= line.text.length)
      }
    }
  }

  property("contract: an empty glyph has no width") {
    forAll(genFont, genMeasurer)((font, measurer) => measurer.glyphWidthPx(font, "") shouldBe 0.0f)
  }
