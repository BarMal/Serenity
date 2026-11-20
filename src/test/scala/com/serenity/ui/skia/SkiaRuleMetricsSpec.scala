package com.serenity.ui.skia

import java.awt.Font

import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** An underline and a strikethrough are rules Skia draws, not font attributes, and where they sit is the font's own:
  * the offset and thickness its Skia metrics give, not what AWT's line metrics said of an AWT font the text no longer
  * draws with.
  */
class SkiaRuleMetricsSpec extends AnyFlatSpec with Matchers:

  private val faces = TestBundledTypefaces.cacheWithInter()

  private val specs = Vector(
    "bundled Inter" -> FontSpec.fromAwt(new Font("Inter", Font.PLAIN, 14)),
    "DejaVu Sans"   -> FontSpec.fromAwt(new Font("DejaVu Sans", Font.PLAIN, 12)),
    "Monospaced"    -> FontSpec.fromAwt(new Font(Font.MONOSPACED, Font.PLAIN, 13))
  )

  private def struck(spec: FontSpec): FontSpec = spec.copy(strikethrough = true)(spec.awtOrigin)

  "A face's underline" should "sit where its Skia metrics put it: the stroke's top below the baseline, its thickness" in
    specs.foreach { (name, spec) =>
      val face = faces.faceFor(spec)
      val rule = face.underline

      withClue(s"$name: ") {
        val (ascent, descent, _) = SkiaTextMeasurer.verticalMetrics(face.font)
        rule shouldBe TextRules.underlineOf(face.font)
        rule.thicknessPx should (be >= 0.2f and be <= face.font.getSize / 4f)
        rule.offsetPx should (be > 0f and be <= 2f * descent)
        ascent should be < 0f
      }
    }

  "A face's strikethrough" should "be a rule only for a struck font, sitting where Skia puts it above the baseline" in
    specs.foreach { (name, spec) =>
      withClue(s"$name: ") {
        faces.faceFor(spec).strikethrough shouldBe None

        val face = faces.faceFor(struck(spec))
        val rule = face.strikethrough.getOrElse(fail("a struck font has a strikethrough rule"))

        val (ascent, _, _) = SkiaTextMeasurer.verticalMetrics(face.font)
        rule shouldBe TextRules.strikethroughOf(face.font)
        rule.thicknessPx should (be >= 0.2f and be <= face.font.getSize / 4f)
        rule.offsetPx should (be < 0f and be >= ascent)
      }
    }

  "A font whose metrics give no rule" should "get an underline and a strikethrough in proportion to its size" in {
    val underline = TextRules.underline(position = None, thickness = None, sizePx = 20f)
    val strike    = TextRules.strikethrough(position = None, thickness = None, xHeightPx = None, sizePx = 20f)

    underline.thicknessPx should be >= 1f
    underline.offsetPx should be > 0f
    strike.thicknessPx should be >= 1f
    strike.offsetPx should be < 0f
  }
