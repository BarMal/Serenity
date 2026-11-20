package com.serenity.ui.skia

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import com.serenity.ui.layout.GlyphAdvances
import com.serenity.ui.layout.TextMeasurer.FontRun
import com.serenity.ui.renderer.BackendCacheKey
import org.jetbrains.skia.Font

/** What the shaping of a text in a font comes to, kept for every owner of one set of shared slots. It is the same
  * wherever it is asked for, so a window's surfaces, its state and its next window shape a text once between them, as
  * they resolve a typeface once ([[SharedTypefaces]]). Everything held is JVM arrays, bounded by the measurer that
  * fills it, and the advances of a paragraph go when nothing holds them.
  */
final private[skia] class ShapingMemos:
  val shapings    = new ConcurrentHashMap[ShapingMemos.ShapedKey, ShapedGlyphs]()
  val shapedChars = new AtomicLong(0L)
  val drawn       = new ConcurrentHashMap[ShapingMemos.ShapedKey, (Array[Float], Float)]()
  val pairs       = new ConcurrentHashMap[ShapingMemos.PairKey, java.lang.Float]()
  val cuts        = new ConcurrentHashMap[ShapingMemos.CutKey, java.lang.Boolean]()
  val advances    = new WeakMemo[ShapingMemos.AdvancesKey, GlyphAdvances]()

private[skia] object ShapingMemos:

  val Key = BackendCacheKey(classOf[ShapingMemos])

  /** What a shaping reads of a font, by value: Skiko compares two fonts by value but hashes them by address, so a font
    * made by another owner for the same face would never be found. Ligatures are the font's spec, which the font does
    * not say.
    */
  final case class FontKey(
      typeface: Int,
      size: Float,
      skewX: Float,
      scaleX: Float,
      emboldened: Boolean,
      ligatures: Boolean
  )

  object FontKey:
    def of(font: Font, ligatures: Boolean): FontKey =
      FontKey(font.getTypeface.getUniqueId, font.getSize, font.getSkewX, font.getScaleX, font.isEmboldened, ligatures)

  final case class ShapedKey(text: String, font: FontKey, level: Byte)

  final case class PairKey(font: FontKey, first: Char, second: Char)

  final case class CutKey(text: String, cut: Int, font: FontKey)

  final case class AdvancesKey(text: String, runs: Vector[FontRun])
