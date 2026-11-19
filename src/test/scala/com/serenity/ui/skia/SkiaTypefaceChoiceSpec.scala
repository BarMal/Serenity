package com.serenity.ui.skia

import java.awt.Font

import com.serenity.ui.renderer.FontSpec
import org.jetbrains.skia.{Font as SkFont, FontMgr, FontStyleSet, Typeface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The face a family's font is drawn in is the one Skia's own style matching picks for the weight and slant asked for:
  * an independent oracle, not what any other font stack reports. Only real families are compared: the set a logical
  * alias such as `Serif` names mixes families, which Skia's matching ranks across.
  */
class SkiaTypefaceChoiceSpec extends AnyFlatSpec with Matchers:

  private val Width5 = 5 << 16
  private val Italic = 1 << 24

  private def skiaMatch(set: FontStyleSet, weight: Int, slant: Int): Typeface =
    val matchStyle = classOf[FontStyleSet].getMethods.find(_.getName.startsWith("matchStyle-"))
    matchStyle.fold(fail("Skiko has no FontStyleSet.matchStyle"))(
      _.invoke(set, Integer.valueOf(weight | Width5 | slant)).asInstanceOf[Typeface]
    )

  /** Skia hands out a new typeface object for each match, so two are the same face when they read the same. */
  private def describe(face: Typeface): (String, Int, Boolean, Float) =
    (face.getFamilyName, SkiaFontWeight.of(face), face.isItalic, new SkFont(face, 14f).measureTextWidth("Hamburgefonstiv"))

  private val families = List("Inter", "DejaVu Sans", "DejaVu Serif", "DejaVu Sans Mono", "Liberation Serif", "Noto Sans")

  for family <- families; (label, bold, italic) <- List(("plain", false, false), ("bold", true, false), ("italic", false, true))
  do
    s"A $label $family font" should "be drawn in the face Skia's own style matching picks, where the system has the family" in {
      val set = FontMgr.Companion.getDefault().matchFamily(family)
      assume(set != null && set.count() > 0, s"$family is not a family of this machine's font manager")
      assume(
        !italic || (0 until set.count()).exists(index => Option(set.getTypeface(index)).exists(_.isItalic)),
        s"$family lists no italic face, so it is drawn upright and skewed, where fontconfig would substitute one"
      )
      val spec   = FontSpec.fromAwt(new Font(family, (if bold then Font.BOLD else 0) | (if italic then Font.ITALIC else 0), 14))
      val chosen = SkiaTypefaces.fontFor(spec).getTypeface
      val oracle = skiaMatch(set, if bold then 700 else 400, if italic then Italic else 0)
      withClue(s"chosen ${describe(chosen)}, Skia matches ${describe(oracle)}: ")(describe(chosen) shouldBe describe(oracle))
    }
