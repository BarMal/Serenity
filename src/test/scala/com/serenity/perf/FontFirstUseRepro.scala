package com.serenity.perf

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute}
import java.awt.geom.AffineTransform
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.{DropCapWidths, FontFirstUseProbes, FontLineExtents}
import com.serenity.ui.renderer.{FontSpec, ShapedStretch, ShapedTextCache, StyledFontCache}

/** Standalone reproduction, run in a fresh JVM per trial (it is a test-scope main, not a spec): many threads make the
  * first use of one font at once through the measuring paths whose results live for the life of the process, then each
  * cached value is compared with the same value recomputed on one thread once the font has settled. Prints `OK` or
  * `BAD <what disagreed>`; the caller counts lines over many JVMs.
  *
  * Arguments: `<family> <threads> <size>`.
  */
object FontFirstUseRepro:

  private val shapedText    = ShapedTextCache()
  private val styledFonts   = StyledFontCache()
  private val lineExtents   = FontLineExtents(TestMeasure.measurer.cellMetrics)
  private val dropCapWidths = DropCapWidths(TestMeasure.measurer.glyphWidthPx)

  private val Texts = Vector(
    "Hoxi AVATAR To Wa Yo",
    "office fi fl ffi affluent waffle",
    "The quick brown fox jumps over the lazy dog, kerning: AV VA LT P. Y, r.",
    "Typography Tf Te Ty Wo Vo Ve"
  )

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      val family  = args.lift(0).getOrElse("Liberation Serif")
      val threads = args.lift(1).flatMap(_.toIntOption).getOrElse(32)
      val size    = args.lift(2).flatMap(_.toIntOption).getOrElse(17)
      val font = Font(family, Font.PLAIN, size).deriveFont(
        java.util.Map.of[TextAttribute, Object](
          TextAttribute.KERNING,
          TextAttribute.KERNING_ON,
          TextAttribute.LIGATURES,
          TextAttribute.LIGATURES_ON
        )
      )
      val frc      = FontRenderContext(AffineTransform(), true, true)
      val gate     = CountDownLatch(1)
      val pool     = Executors.newFixedThreadPool(threads)
      val finished = CountDownLatch(threads)
      (0 until threads).foreach { _ =>
        pool.execute { () =>
          gate.await()
          try cachedValues(font, frc)
          finally finished.countDown()
        }
      }
      gate.countDown()
      val _   = finished.await(5, TimeUnit.MINUTES)
      val bad = disagreements(font, frc)
      pool.shutdown()
      println(if bad.isEmpty then s"OK ${settledDigest(font, frc)}" else s"BAD ${bad.mkString(",")}")

  private def cachedValues(font: Font, frc: FontRenderContext): Unit =
    Texts.foreach { text =>
      val _ = shapedText.stretchOf(text, font, frc)
    }
    val _ = lineExtents.of(FontSpec.fromAwt(font))
    val _ = styledFonts.underlineOf(font)
    val _ = dropCapWidths(FontSpec.fromAwt(font), "A")
    val _ = FontFirstUseProbes.advanceChecks(font, frc)
    Texts.foreach(text => FontFirstUseProbes.measure(text, font, frc, shapedText))

  private def stretchDigest(stretch: ShapedStretch): String =
    (0 until stretch.glyphCount)
      .map(glyph => s"${stretch.glyphCodeAt(glyph)}@${stretch.xAt(glyph)},${stretch.yAt(glyph)}")
      .mkString(";") + s"|${stretch.advance}"

  /** What the process ends up believing, as a hash: a single-threaded run is the reference every racing run must match.
    */
  private def settledDigest(font: Font, frc: FontRenderContext): String =
    val beliefs = Texts.map { text =>
      stretchDigest(shapedText.stretchOf(text, font, frc)) +
        FontFirstUseProbes.advancesDigest(text, font, frc, shapedText)
    } :+ lineExtents.of(FontSpec.fromAwt(font)).toString :+
      styledFonts.underlineOf(font).toString :+
      dropCapWidths(FontSpec.fromAwt(font), "A").toString :+
      FontFirstUseProbes.advanceChecks(font, frc).toString
    Integer.toHexString(beliefs.mkString("\n").hashCode)

  private def disagreements(font: Font, frc: FontRenderContext): Vector[String] =
    val shapes = Texts
      .filter { text =>
        stretchDigest(shapedText.stretchOf(text, font, frc)) != stretchDigest(
          ShapedStretch.shape(text, font, frc)
        )
      }
      .map(_ => "shaped-text")
    // Layout's extents and drop cap widths come from the Skia measurer, so a fresh measurement of the settled font is
    // what the raced one must equal.
    val extents =
      val settled = FontLineExtents(TestMeasure.measurer.cellMetrics).of(FontSpec.fromAwt(font))
      Option.when(lineExtents.of(FontSpec.fromAwt(font)) != settled)("line-extents")
    val underline =
      val metrics = font.getLineMetrics("", FontRenderContext(AffineTransform(), true, true))
      Option.when(styledFonts.underlineOf(font) != StyledFontCache.Underline.of(metrics))("underline")
    val dropCap =
      Option.when(
        dropCapWidths(FontSpec.fromAwt(font), "A") != TestMeasure.measurer.glyphWidthPx(FontSpec.fromAwt(font), "A")
      )(
        "drop-cap-width"
      )
    val advances = Texts
      .filter { text =>
        FontFirstUseProbes.advancesDigest(text, font, frc, shapedText) !=
          FontFirstUseProbes.advancesDigest(text, font, frc, new ShapedTextCache())
      }
      .map(_ => "advances")
    (shapes ++ extents ++ underline ++ dropCap ++ advances).distinct
