package com.serenity.ui.skia

import java.awt.Font

import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Counting a long prose document asks, for every row it ends, whether the text can be cut at a space. The windows
  * around those spaces are all different, so what the answer costs must not follow the windows: it is what the pair of
  * characters across the cut, and the font, decide.
  */
class SkiaCutDecisionCostSpec extends AnyFlatSpec with Matchers:

  private val Windows = 400

  private def proseWindows(seed: Long): Vector[(String, Int)] =
    val random = new scala.util.Random(seed)
    Vector.fill(Windows) {
      val before = random.alphanumeric.take(7).mkString.toLowerCase
      val after  = random.alphanumeric.take(8).mkString.toLowerCase
      (s"${before.take(7)} $after", 8)
    }

  private def runsToDecide(fontName: String, ligatures: Boolean): (Long, Int) =
    val faces     = new SkiaFontCache()
    val stretches = new SkiaShapedTextCache(ligaturesOf = faces.ligaturesOf)
    val measurer  = new SkiaTextMeasurer(faces, stretches, memos = new ShapingMemos())
    val spec      = FontSpec.fromAwt(Font(fontName, Font.PLAIN, 14)).copy(ligatures = ligatures)(None)
    val windows   = proseWindows(7L)
    val before    = measurer.shaperRuns
    windows.foreach((text, cut) => measurer.isShapingCut(text, cut, spec))
    (measurer.shaperRuns - before, windows.map((text, cut) => text.charAt(cut)).distinct.size)

  "Deciding where prose may be cut" should "shape once per character after the space, not once per window, with ligatures off" in {
    val (runs, distinctNext) = runsToDecide("Inter", ligatures = false)

    runs should be <= (4L * distinctNext + 16L)
  }

  it should "shape once per character after the space, not once per window, for a font without substitutions" in {
    val (runs, distinctNext) = runsToDecide("DejaVu Sans", ligatures = false)

    runs should be <= (4L * distinctNext + 16L)
  }

  "The first cut decision in a font" should "shape a handful of times, not once for each character of a probe" in
    Vector("Inter", "DejaVu Sans").foreach { family =>
      val faces     = new SkiaFontCache()
      val stretches = new SkiaShapedTextCache(ligaturesOf = faces.ligaturesOf)
      val measurer  = new SkiaTextMeasurer(faces, stretches, memos = new ShapingMemos())
      val spec      = FontSpec.fromAwt(Font(family, Font.PLAIN, 14)).copy(ligatures = false)(None)

      val _ = measurer.isShapingCut("first cut here", 6, spec)

      withClue(s"$family: ")(measurer.shaperRuns should be <= 8L)
    }
