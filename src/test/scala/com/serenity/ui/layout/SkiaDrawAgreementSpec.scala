package com.serenity.ui.layout

import com.serenity.SkiaPixels
import com.serenity.ui.layout.TextMeasureCorpus.{Line, PinnedFont}
import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import com.serenity.ui.skia.{SkiaShapedTextCache, SkiaTextMeasurer, TestBundledTypefaces}
import org.jetbrains.skia.{Canvas, Paint, Surface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the measurer says and what Skia draws, compared as pixels. The line is drawn once through the stretch the
  * render surface paints with; then each cluster is drawn alone with its first glyph at the x the measurer gives it. If
  * every measured cluster origin is where Skia put the glyph, the pieces add up to the line.
  */
class SkiaDrawAgreementSpec extends AnyFlatSpec with Matchers:

  private val faces     = TestBundledTypefaces.cacheWithInter()
  private val measurer  = new SkiaTextMeasurer(faces)
  private val stretches = new SkiaShapedTextCache()

  private val Height       = 48
  private val Baseline     = 32f
  private val Margin       = 12
  private val AlphaSlack   = 2
  private val NotDrawnHere = Set("ltr_rtl", "tabs")

  private val lines = TextMeasureCorpus.lines.filterNot(line => NotDrawnHere.contains(line.id))

  private def clusterStarts(text: String, fonts: LineFonts): Vector[Int] =
    measurer
      .itemise(text, 0, text.length, 0, fonts)
      .flatMap(run => measurer.shape(text, 0, run, 0.toByte).clusters.map(_ + run.startColumn))
      .distinct
      .sorted

  private def alphaOf(width: Int)(draw: (Canvas, Paint) => Unit): Array[Int] =
    val surface = Surface.Companion.makeRasterN32Premul(width, Height)
    val paint   = new Paint()
    paint.setColor(0xff000000)
    draw(surface.getCanvas, paint)
    SkiaPixels.read(surface).map(_ >>> 24)

  private def widthOf(carets: IArray[Float]): Int = math.ceil(carets.lastOption.getOrElse(0.0f)).toInt + 2 * Margin

  /** The largest difference in alpha between the line and its clusters placed at the measured origins, the origins
    * moved by `shift` pixels. `None` when the line has no cluster to place.
    */
  private def worstAlphaDifference(pinned: PinnedFont, line: Line, shift: Float): Option[Int] =
    val fonts  = LineFonts(FontSpec.fromAwt(pinned.font), Vector.empty)
    val text   = line.text
    val carets = measurer.rawCaretXs(text, 0, fonts)
    val skia   = faces.faceFor(pinned.font).font
    val shaped = stretches.stretchOf(text, skia)
    val starts = clusterStarts(text, fonts)
    val ends   = starts.drop(1) :+ text.length
    val width  = widthOf(carets)
    val whole  = alphaOf(width)((canvas, paint) => shaped.draw(canvas, 0, text.length, Margin.toFloat, Baseline, paint))
    val pieces = alphaOf(width) { (canvas, paint) =>
      starts.zip(ends).foreach { (from, until) =>
        shaped.draw(canvas, from, until, Margin + carets(from) + shift, Baseline, paint)
      }
    }
    Option.when(starts.nonEmpty)(
      whole.zip(pieces).foldLeft(0)((worst, pair) => math.max(worst, math.abs(pair._1 - pair._2)))
    )

  /** Lines whose drawing depends on which emoji font the host has, so their result is reported, not asserted: with a
    * colour emoji font installed Skiko's `TextLine` draws a skin-toned thumbs-up as one glyph where the measurer's
    * shaper cuts it into two (a measured-versus-drawn gap); with none, both sides draw the same missing-glyph boxes.
    */
  private val HostDependent = Set("emoji_clusters")

  private def agreement(hostDependent: Boolean): Vector[(String, Int)] =
    for
      pinned <- TextMeasureCorpus.fonts
      line   <- lines.filter(line => HostDependent.contains(line.id) == hostDependent)
      worst  <- worstAlphaDifference(pinned, line, 0.0f)
    yield (s"${pinned.label}.${line.id}", worst)

  "Each cluster drawn at its measured origin" should "land where the line draws it, for every bundled font and line" in {
    val results = agreement(hostDependent = false)
    results should not be empty
    info(
      s"largest alpha difference over ${results.size} lines: ${results.foldLeft(0)((largest, result) => math.max(largest, result._2))} of 255"
    )
    val off = results.filter(_._2 > AlphaSlack)
    withClue(off.map((key, worst) => s"$key differs by $worst").mkString("; ")) {
      off shouldBe empty
    }
  }

  "The lines that depend on the host's emoji font" should "be reported, whatever they show" in {
    val results = agreement(hostDependent = true)
    results should not be empty
    info(s"host dependent lines differ by ${results.map(_._2).mkString(", ")} of 255")
  }

  "The same comparison" should "tell a cluster half a pixel from its origin" in {
    val pinned = TextMeasureCorpus.fonts.filter(_.label == "courier_prime").headOption.getOrElse(fail("no font"))
    val line   = lines.find(_.id == "ascii_code").getOrElse(fail("no line"))
    worstAlphaDifference(pinned, line, 0.5f).getOrElse(0) should be > AlphaSlack
  }
