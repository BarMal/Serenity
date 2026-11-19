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
  private val measurer  = new SkiaTextMeasurer(faces, stretches)
  private val fonts     = LineFonts(FontSpec.fromAwt(Font(Font.SANS_SERIF, Font.PLAIN, 14)), Vector.empty)

  private def memoOf(measurer: SkiaTextMeasurer): java.util.Map[?, ?] =
    val field = measurer.getClass.getDeclaredFields.find(_.getName.endsWith("shapings")).get
    field.setAccessible(true)
    field.get(measurer).asInstanceOf[java.util.Map[?, ?]]

  "A remembered shaping" should "hold arrays and numbers only, not the builders that filled them" in {
    val _       = measurer.exactCaretXs("The quick brown fox jumps over the lazy dog, twice over.", 0, fonts)
    val entries = memoOf(measurer).values.toArray.toVector

    entries should not be empty
    entries.foreach { entry =>
      val held = entry.getClass.getDeclaredFields.filterNot(field => Modifier.isStatic(field.getModifiers))
      val heavy = held.filterNot(field => field.getType.isArray || field.getType.isPrimitive).map(_.getName)
      withClue(s"${entry.getClass.getSimpleName} holds ${heavy.mkString(", ")}: ")(heavy shouldBe empty)
    }
  }
