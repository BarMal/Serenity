package com.serenity.ui.skia

import java.awt.Font
import java.lang.reflect.Modifier

import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The measurer keeps the shaping of every line a runtime lays out, so what an entry holds is what a drawn document
  * leaves resident (`footprint.prose_frame.*`): the arrays, and nothing that was used to build them.
  */
class SkiaShapingMemoSpec extends AnyFlatSpec with Matchers:

  private val faces     = new SkiaFontCache()
  private val stretches = new SkiaShapedTextCache(ligaturesOf = faces.ligaturesOf)
  private val memos     = new ShapingMemos()
  private val measurer  = new SkiaTextMeasurer(faces, stretches, memos = memos)
  private val fonts     = LineFonts(FontSpec.fromAwt(Font(Font.SANS_SERIF, Font.PLAIN, 14)), Vector.empty)

  private def memoOf(measurer: SkiaTextMeasurer): java.util.Map[?, ?] = memos.shapings

  "A remembered shaping" should "hold arrays and numbers only, not the builders that filled them" in {
    val _       = measurer.exactCaretXs("The quick brown fox jumps over the lazy dog, twice over.", 0, fonts)
    val entries = memoOf(measurer).values.toArray.toVector

    entries should not be empty
    entries.foreach { entry =>
      val held  = entry.getClass.getDeclaredFields.filterNot(field => Modifier.isStatic(field.getModifiers))
      val heavy = held.filterNot(field => field.getType.isArray || field.getType.isPrimitive).map(_.getName)
      withClue(s"${entry.getClass.getSimpleName} holds ${heavy.mkString(", ")}: ")(heavy shouldBe empty)
    }
  }

  it should "hold no y offsets or levels per glyph for left-to-right text, and its levels for mixed text" in {
    val plain = "Left to right only, with nothing to tell apart."
    val mixed = "abc \u05e9\u05dc\u05d5\u05dd def"
    val _     = (measurer.exactCaretXs(plain, 0, fonts), measurer.exactCaretXs(mixed, 0, fonts))

    def length(entry: AnyRef, name: String): Int =
      val field = entry.getClass.getDeclaredFields.find(_.getName == name).get
      field.setAccessible(true)
      java.lang.reflect.Array.getLength(field.get(entry))

    val byText = memoOf(measurer).entrySet.toArray.toVector.map { entry =>
      val pair = entry.asInstanceOf[java.util.Map.Entry[AnyRef, AnyRef]]
      val key  = pair.getKey.asInstanceOf[Product].productElement(0).asInstanceOf[String]
      key -> pair.getValue
    }.toMap

    length(byText(plain), "offsets") shouldBe 0
    length(byText(plain), "mixedLevels") shouldBe 0
    length(byText(mixed), "mixedLevels") should be > 0
  }
