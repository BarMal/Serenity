package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.TextAttribute

import com.serenity.ui.fonts.{FontFallback, FontLoader}

/** The fonts and lines whose AWT measurement [[TextMeasureBaseline]] pins. Every font is a bundled file, so the glyphs
  * and their metrics are the same on every host that loads them; a glyph none of them holds still falls back to the
  * system's `Dialog`, which the corpus keeps to a few combining marks.
  */
private[layout] object TextMeasureCorpus:

  final case class PinnedFont(label: String, font: Font, fallback: FontFallback)

  final case class Line(id: String, text: String)

  private val SizePt = 14f

  private def bundled(resource: String): Font =
    val stream = getClass.getResourceAsStream(resource)
    try Font.createFont(Font.TRUETYPE_FONT, stream).deriveFont(SizePt)
    finally stream.close()

  private def ligatured(font: Font): Font =
    font.deriveFont(java.util.Map.of(TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON))

  private lazy val fallback = FontFallback(emojiFamily = FontLoader.emojiFontFamily)

  lazy val fonts: Vector[PinnedFont] =
    val courier   = bundled("/fonts/CourierPrime-Regular.ttf")
    val bold      = bundled("/fonts/CourierPrime-Bold.ttf")
    val monaspace = bundled("/fonts/MonaspaceNeon-Regular.otf")
    val inter     = bundled("/fonts/Inter-Regular-Subset.ttf")
    Vector(
      PinnedFont("courier_prime", courier, fallback),
      PinnedFont("courier_prime_bold", bold, fallback),
      PinnedFont("monaspace_neon", monaspace, fallback),
      PinnedFont("monaspace_neon_ligatures", ligatured(monaspace), fallback),
      PinnedFont("inter_subset", inter, fallback),
      PinnedFont("inter_subset_ligatures", ligatured(inter), fallback)
    )

  private val Paragraph =
    "The quick brown fox jumps over the lazy dog. Typography affects every Wave and Vase; Tomorrow, " +
      "after fifteen office coffees, ffi and fl and fi join. A very long word follows: " +
      "antidisestablishmentarianism, then more words to fill the rows until the wrap points are many."

  val lines: Vector[Line] = Vector(
    Line("ascii_code", "val value = compute(input, 42) // plain ASCII code line"),
    Line("kerning_prose", "AV To Tj Wa Yo fi ffi Th: kerning pairs and ligatures"),
    Line("ligature_code", "a => b -> c // d != e === f <= g >= h && i || j"),
    Line("tabs", "\tindented\tcolumns\there"),
    Line("latin_marks", "café café naïve naïve"),
    Line("cjk", "你好，世界 こんにちは 안녕하세요"),
    Line(
      "emoji_clusters",
      "a❤️b 👍🏽 c 👨‍👩‍👧 d 🇺🇸 e 1️⃣"
    ),
    Line("paragraph", Paragraph),
    Line("ltr_rtl", "abc שלום def مرحبا ghi")
  )

  /** The line whose characters no bundled font holds, so the system's fallback measures them: only how many caret stops
    * it has is pinned, not where they are.
    */
  val structuralOnly: Set[String] = Set("ltr_rtl")
