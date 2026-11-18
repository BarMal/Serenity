package com.serenity.ui.skia

import java.awt.font.TextAttribute

import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.TextMeasurer.{FontRun, LineFonts}
import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `ligatures` of a [[FontSpec]] is what decides whether Skia sets the font's ligatures and contextual alternates,
  * for the glyphs the surface draws and the glyphs the layout measures alike. Monaspace sets `...` as one cluster.
  */
final class SkiaLigatureFeaturesSpec extends AnyFlatSpec with Matchers:

  private val Dots = "..."

  private def monaspace(ligatures: Boolean): FontSpec =
    FontSpec.fromAwt(FontLoader.previewCodeFont(FontLoader.FontConfig(fontSize = 14.0f, enableLigatures = ligatures)))

  private def measuredClusters(spec: FontSpec): Int =
    val measurer = new SkiaTextMeasurer(new SkiaFontCache())
    measurer.shape(Dots, 0, FontRun(0, Dots.length, spec), 0).clusters.distinct.length

  private def drawnClusters(spec: FontSpec): Int =
    val caches = SkiaTextCaches.fresh()
    val drawn  = caches.shapedText.stretchOf(Dots, caches.fonts.faceFor(spec).font)
    Dots.indices.count(index => drawn.leftEdgeOf(index).isDefined)

  "A font with ligatures off" should "measure one cluster for each character" in {
    measuredClusters(monaspace(ligatures = false)) shouldBe Dots.length
  }

  it should "draw one cluster for each character" in {
    drawnClusters(monaspace(ligatures = false)) shouldBe Dots.length
  }

  "A font with ligatures on" should "measure fewer clusters than characters" in {
    measuredClusters(monaspace(ligatures = true)) should be < Dots.length
  }

  it should "draw fewer clusters than characters" in {
    drawnClusters(monaspace(ligatures = true)) should be < Dots.length
  }

  it should "set the letters of text that Skiko's line shaper sets, flags included, as it sets them beside Latin text" in {
    val serif  = FontSpec.fromAwt(java.awt.Font(java.awt.Font.SERIF, java.awt.Font.ITALIC, 17))
    val caches = SkiaTextCaches.fresh()
    val face   = caches.fonts.faceFor(serif).font
    val edge   = (text: String) => caches.shapedText.stretchOf(text, face).leftEdgeOf(5)
    edge("flags \uD83C\uDDEC\uD83C\uDDE7").zip(edge("flags xx")).foreach((flag, latin) => flag shouldBe latin +- 0.01f)
  }

  "An emoji sequence in a font with ligatures off" should "be set as wide as with them on, as emoji composition is not a typographic ligature" in {
    val plain = java.awt.Font(java.awt.Font.SERIF, java.awt.Font.PLAIN, 15)
    val off   = FontSpec.fromAwt(plain)
    val on = FontSpec.fromAwt(plain.deriveFont(java.util.Map.of(TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)))
    val measurer = new SkiaTextMeasurer(new SkiaFontCache())
    def widthOf(spec: FontSpec, text: String): Float =
      measurer.rawCaretXs(text, 0, LineFonts(spec, Vector.empty)).last
    Seq(
      "flag"      -> "\uD83C\uDDEC\uD83C\uDDE7",
      "family"    -> "\uD83D\uDC68\u200D\uD83D\uDC69\u200D\uD83D\uDC67\u200D\uD83D\uDC66",
      "skin tone" -> "\uD83D\uDC4D\uD83C\uDFFD"
    ).foreach((name, sequence) => withClue(s"$name: ")(widthOf(off, sequence) shouldBe widthOf(on, sequence) +- 0.01f))
  }
