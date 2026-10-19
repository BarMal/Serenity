package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute}
import java.awt.geom.AffineTransform
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.renderer.{ShapedStretch, ShapedTextCache, StyledFontCache}

/** Standalone reproduction, run in a fresh JVM per trial (it is a test-scope main, not a spec): many threads make the
  * first use of one font at once through the measuring paths whose results live for the life of the process, then each
  * cached value is compared with the same value recomputed on one thread once the font has settled. Prints `OK` or
  * `BAD <what disagreed>`; the caller counts lines over many JVMs.
  *
  * Arguments: `<family> <threads> <size>`.
  */
object FontFirstUseRepro:

  private val Texts = Vector(
    "Hoxi AVATAR To Wa Yo",
    "office fi fl ffi affluent waffle",
    "The quick brown fox jumps over the lazy dog, kerning: AV VA LT P. Y, r.",
    "Typography Tf Te Ty Wo Vo Ve"
  )

  def main(args: Array[String]): Unit =
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
      val _ = ShapedTextCache.shared.stretchOf(text, font, frc)
    }
    val _ = FontLineExtents.shared(FontLineExtents.Key(font, frc))
    val _ = StyledFontCache.shared.underlineOf(font)
    val _ = DropCapLayout.glyphWidthPx(font, frc, "A")
    val _ = GlyphAdvances.layoutAdvanceMatchesSum(font, frc)
    val _ = GlyphAdvances.advancesAreUniform(font, frc)
    Texts.foreach(text => measure(text, font, frc, ShapedTextCache.shared))

  private def measure(text: String, font: Font, frc: FontRenderContext, shaped: ShapedTextCache): GlyphAdvances =
    GlyphAdvances.measure(text, 0, text.length, 0, LineFontResolver(font, Vector.empty), frc, shaped)

  private def stretchDigest(stretch: ShapedStretch): String =
    (0 until stretch.glyphCount)
      .map(glyph => s"${stretch.glyphCodeAt(glyph)}@${stretch.xAt(glyph)},${stretch.yAt(glyph)}")
      .mkString(";") + s"|${stretch.advance}"

  private def advancesDigest(advances: GlyphAdvances): String =
    advances.caretsFrom(0, advances.length).mkString(",") + "|" +
      (0 until advances.length).map(index => if advances.isContextFree(index, index + 1) then "c" else "-").mkString

  /** What the process ends up believing, as a hash: a single-threaded run is the reference every racing run must match.
    */
  private def settledDigest(font: Font, frc: FontRenderContext): String =
    val beliefs = Texts.map { text =>
      stretchDigest(ShapedTextCache.shared.stretchOf(text, font, frc)) +
        advancesDigest(measure(text, font, frc, ShapedTextCache.shared))
    } :+ FontLineExtents.shared(FontLineExtents.Key(font, frc)).toString :+
      StyledFontCache.shared.underlineOf(font).toString :+
      DropCapLayout.glyphWidthPx(font, frc, "A").toString :+
      GlyphAdvances.layoutAdvanceMatchesSum(font, frc).toString :+ GlyphAdvances.advancesAreUniform(font, frc).toString
    Integer.toHexString(beliefs.mkString("\n").hashCode)

  private def disagreements(font: Font, frc: FontRenderContext): Vector[String] =
    val shapes = Texts
      .filter { text =>
        stretchDigest(ShapedTextCache.shared.stretchOf(text, font, frc)) != stretchDigest(
          ShapedStretch.shape(text, font, frc)
        )
      }
      .map(_ => "shaped-text")
    val extents =
      val metrics = font.getLineMetrics("Mg", frc)
      val settled =
        FontLineExtents.Extent(
          math.ceil(metrics.getAscent.toDouble).toInt,
          math.ceil((metrics.getDescent + metrics.getLeading).toDouble).toInt
        )
      Option.when(FontLineExtents.shared(FontLineExtents.Key(font, frc)) != settled)("line-extents")
    val underline =
      val metrics = font.getLineMetrics("", FontRenderContext(AffineTransform(), true, true))
      Option.when(StyledFontCache.shared.underlineOf(font) != StyledFontCache.Underline.of(metrics))("underline")
    val dropCap =
      Option.when(DropCapLayout.glyphWidthPx(font, frc, "A") != font.getStringBounds("A", frc).getWidth.toFloat)(
        "drop-cap-width"
      )
    val advances = Texts
      .filter { text =>
        advancesDigest(measure(text, font, frc, ShapedTextCache.shared)) !=
          advancesDigest(measure(text, font, frc, new ShapedTextCache()))
      }
      .map(_ => "advances")
    (shapes ++ extents ++ underline ++ dropCap ++ advances).distinct
