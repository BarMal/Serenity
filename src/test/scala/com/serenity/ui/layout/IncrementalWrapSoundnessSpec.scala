package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.TextAttribute

import scala.util.Random

import com.serenity.state.models.TextVisualLine
import com.serenity.ui.layout.TextCaretMeasurement.singleFontResolver
import com.serenity.ui.layout.WrapFixtures.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The assumptions [[IncrementalWrap]]'s exactness rests on, each checked against the thing it assumes about. */
class IncrementalWrapSoundnessSpec extends AnyFlatSpec with Matchers:

  private val measuredLayouts =
    layouts.filter(layout => !layout.forceCellLayout && TextLayoutSnapshot.shouldUseMeasuredLayout(layout.font, frc))

  private def cold(text: String, layout: Layout, widthPx: Int): RowWrap =
    TextLayoutSnapshot.wrapRows(keyFor(text, layout, widthPx), 0, Int.MaxValue)

  // -- ICU's lookahead ---------------------------------------------------------------------------------------------

  private val breakAlphabet: Vector[String] =
    ("abcXYZ019 $%()[]{}\"'.,;:!?-/&@#*+=<>_~|^\t —–“”‘’…éЖα" +
      "、。「」­​").map(_.toString).toVector ++
      Vector("🇬🇧", "🇯🇵", "👍🏽", "👨‍👩‍👧", "1,2", "3.5", "$(5)", "(5)", "US$", "http://x.y/z", "--", "́")

  private def breakText(rnd: Random, length: Int): String =
    Iterator.continually(breakAlphabet(rnd.nextInt(breakAlphabet.length))).take(length).mkString

  "the row break search" should "depend only on the text up to its lookahead margin past the fit" in {
    info(s"$Cases pairs of texts that differ only past the margin")
    val rnd = Random(20260418L)
    (0 until Cases).foreach { _ =>
      val from   = rnd.nextInt(30)
      val fit    = 1 + rnd.nextInt(40)
      val margin = from + fit + 1 + IncrementalWrap.IcuLookaheadUnits
      val head   = breakText(rnd, margin + 10).take(margin)
      val first  = head + breakText(rnd, 1 + rnd.nextInt(40))
      val second = head + breakText(rnd, 1 + rnd.nextInt(40))
      withClue(s"from=$from fit=$fit head=${head.map(c => f"\\u${c.toInt}%04x").mkString}: ")(
        TextLayoutSnapshot.wordBoundarySegmentLength(first, from, fit) shouldBe
          TextLayoutSnapshot.wordBoundarySegmentLength(second, from, fit)
      )
    }
  }

  it should "not be resumed for dictionary-segmented scripts, where ICU may read the whole run" in {
    Vector("日本語のテキスト", "ひらがなカタカナ", "ภาษาไทย", "ພາສາລາວ", "ភាសាខ្មែរ", "မြန်မာ").foreach { text =>
      withClue(s"$text: ")(IncrementalWrap.traceable(keyFor(text, layouts.head, 100)) shouldBe false)
    }
    IncrementalWrap.traceable(keyFor("한국어 plain text", layouts.head, 100)) shouldBe true
  }

  // -- shaping barriers ----------------------------------------------------------------------------------------------

  private def liberationSerifKerned: Option[Font] =
    Option(Font("Liberation Serif", Font.PLAIN, 14)).filter(_.getFamily == "Liberation Serif").map { font =>
      font.deriveFont(java.util.Map.of(TextAttribute.KERNING, TextAttribute.KERNING_ON))
    }

  "a cut between a space and the next character" should "be clean in the default prose font, whatever the character" in
    (' ' to '~').foreach(char =>
      withClue(s"'$char': ")(ShapingBarriers.cutsCleanly(defaultProse, frc, char) shouldBe true)
    )

  it should "be unclean where the font kerns against a space" in {
    assume(liberationSerifKerned.isDefined, "Liberation Serif is not installed")
    val font = liberationSerifKerned.get
    "ATVWY".foreach(char => withClue(s"'$char': ")(ShapingBarriers.cutsCleanly(font, frc, char) shouldBe false))
    "abcdefghij".foreach(char => withClue(s"'$char': ")(ShapingBarriers.cutsCleanly(font, frc, char) shouldBe true))
  }

  it should "have been worth probing: cutting at every space changes advances in such a font" in {
    assume(liberationSerifKerned.isDefined, "Liberation Serif is not installed")
    val font          = liberationSerifKerned.get
    val resolver      = singleFontResolver(font)
    val text          = "the To AV Wa Ty Yo end"
    val whole         = GlyphAdvances.measure(text, 0, text.length, 0, resolver, frc)
    val cutAt         = text.indexOf("To")
    val right         = GlyphAdvances.measure(text, cutAt, text.length, 0, resolver, frc)
    val left          = GlyphAdvances.measure(text, 0, cutAt, 0, resolver, frc)
    val spaceBeforeTo = cutAt - 1
    val differs = whole.isContextFree(spaceBeforeTo, cutAt) != left.isContextFree(spaceBeforeTo, cutAt) ||
      whole.caretsFrom(spaceBeforeTo, 1)(1) != left.caretsFrom(spaceBeforeTo, 1)(1) ||
      whole.isContextFree(cutAt, cutAt + 1) != right.isContextFree(0, 1)
    differs shouldBe true
  }

  private def sameAdvances(actual: GlyphAdvances, expected: GlyphAdvances, length: Int): Boolean =
    actual.length == length && expected.length == length && (0 until length).forall { index =>
      actual.isContextFree(index, index + 1) == expected.isContextFree(index, index + 1) &&
      java.lang.Float.floatToRawIntBits(actual.caretsFrom(index, 1)(1)) ==
        java.lang.Float.floatToRawIntBits(expected.caretsFrom(index, 1)(1))
    }

  "advances spliced around an edit" should "equal the advances of the whole new text, character by character" in {
    info(s"${Cases / 2} random edits")
    val rnd = Random(20260420L)
    (0 until Cases / 2).foreach { _ =>
      val layout = measuredLayouts(rnd.nextInt(measuredLayouts.length))
      val width  = widthPx(rnd, layout)
      val before = paragraphFor(rnd, layout, width, 1 + rnd.nextInt(20))
      val rows   = wrap(before, WrappedLineCache.Uncached, layout, width)
      val change = edit(rnd, before, rows)
      val after  = change.applyTo(before)
      val oldKey = keyFor(before, layout, width)
      val newKey = keyFor(after, layout, width)
      val old    = TextLayoutSnapshot.wrapRows(oldKey, 0, Int.MaxValue)
      (old.trace, Option.when(IncrementalWrap.traceable(newKey))(())) match
        case (Some(trace), Some(_)) =>
          val resumed = IncrementalWrap.rewrap(newKey, 0, Seq(Predecessor(oldKey, 0, old.rows, trace)))
          resumed.foreach { wrapped =>
            val whole = GlyphAdvances.measure(after, 0, after.length, 0, newKey.resolver, frc)
            withClue(s"${layout.name} $change in '${before.take(80)}': ") {
              wrapped.trace.flatMap(_.advances).exists(sameAdvances(_, whole, after.length)) shouldBe true
            }
          }
        case _ => ()
    }
  }

  private val kerningText =
    "To AV Wa Ty Yo the And All Tab Var War Yes ta ya wa fi fl ff A T V W Y Aa Tt Vv Ww Yy x A y TT VV".repeat(2)
  private val ligatureText =
    "fi fl ffi ffl office affluent waffle shuffle fifth fjord after fine flight offer ski fix ".repeat(3)

  it should "equal the advances of the whole new text at every offset, where the font kerns against spaces or ligates" in {
    assume(liberationSerifKerned.isDefined, "Liberation Serif is not installed")
    val fonts = Vector(
      (Layout("liberation", liberationSerifKerned.get), kerningText),
      (layouts.find(_.name == "default-prose").get, ligatureText),
      (layouts.find(_.name == "serif-kerned-ligatured").get, kerningText + ligatureText)
    )
    fonts.foreach { (layout, before) =>
      val width = 20 * layout.charWidthPx
      val edits = (0 to before.length).flatMap { offset =>
        val one = 1.min(before.length - offset)
        Vector("A", "T", "V", "W", "Y", "a", " ", "AV", "f", "i", "l").map(Edit(offset, 0, _)) ++
          Vector(Edit(offset, one, ""), Edit(offset, one, "A"), Edit(offset, one, " "), Edit(offset, one, "f"))
      }
      edits.filter(_.applyTo(before) != before).foreach { change =>
        val after  = change.applyTo(before)
        val oldKey = keyFor(before, layout, width)
        val newKey = keyFor(after, layout, width)
        val old    = TextLayoutSnapshot.wrapRows(oldKey, 0, Int.MaxValue)
        val resumed =
          old.trace.flatMap(trace => IncrementalWrap.rewrap(newKey, 0, Seq(Predecessor(oldKey, 0, old.rows, trace))))
        withClue(s"${layout.name} $change: ") {
          resumed.isDefined shouldBe true
          val whole = GlyphAdvances.measure(after, 0, after.length, 0, newKey.resolver, frc)
          resumed.flatMap(_.trace).flatMap(_.advances).exists(sameAdvances(_, whole, after.length)) shouldBe true
          bitwise(resumed.get.rows) shouldBe bitwise(TextLayoutSnapshot.wrapRows(newKey, 0, Int.MaxValue).rows)
        }
      }
    }
  }

  // -- the reach of a row's decision ---------------------------------------------------------------------------------

  "a row" should "be unchanged by any change to the text past its reach" in {
    info(s"${Cases / 4} random paragraphs, every row of each mutated past its reach")
    val rnd = Random(20260421L)
    (0 until Cases / 4).foreach { _ =>
      val layout = layouts(rnd.nextInt(layouts.length))
      val width  = widthPx(rnd, layout)
      val text   = paragraphFor(rnd, layout, width, 1 + rnd.nextInt(30))
      val key    = keyFor(text, layout, width)
      cold(text, layout, width).trace.foreach { trace =>
        val rows = wrap(text, WrappedLineCache.Uncached, layout, width)
        trace.reaches.zipWithIndex.filter((reach, _) => reach != IncrementalWrap.ToEndOfText).foreach {
          (reach, index) =>
            reach should be <= key.text.length
            val mutated     = text.take(reach) + snippet(rnd) + paragraph(rnd, rnd.nextInt(200))
            val mutatedRows = wrap(mutated, WrappedLineCache.Uncached, layout, width)
            withClue(s"${layout.name} width=$width row $index reach=$reach of '${text.take(120)}': ") {
              bitwise(mutatedRows.take(index + 1)) shouldBe bitwise(rows.take(index + 1))
            }
        }
      }
    }
  }

  it should "have a reach beyond its own end" in {
    val rnd = Random(20260422L)
    (0 until Cases / 10).foreach { _ =>
      val layout  = layouts(rnd.nextInt(layouts.length))
      val width   = widthPx(rnd, layout)
      val text    = paragraphFor(rnd, layout, width, 1 + rnd.nextInt(20))
      val wrapped = cold(text, layout, width)
      wrapped.trace.foreach { trace =>
        trace.reaches.length shouldBe wrapped.rows.length
        wrapped.rows.zip(trace.reaches).foreach((row: TextVisualLine, reach) => reach should be > row.endColumn)
      }
    }
  }
