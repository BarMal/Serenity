package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute}
import java.awt.geom.AffineTransform

import com.serenity.ui.layout.{BundledInterFont, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What [[ShapedTextPaintSpec]] asks of shaped text that needs no surface: the shaped-text cache serves what it has
  * shaped and stays bounded, a line is shaped into as many glyphs however it is split, and a caret stop sits where the
  * shaped line draws its character.
  */
class ShapedTextCacheSpec extends AnyFlatSpec with Matchers:

  private def kerned(face: Font): Font = face.deriveFont(
    java.util.Map.of[TextAttribute, Object](
      TextAttribute.LIGATURES,
      TextAttribute.LIGATURES_ON,
      TextAttribute.KERNING,
      TextAttribute.KERNING_ON
    )
  )

  private val font = kerned(new Font(Font.SERIF, Font.PLAIN, 14))

  private val frc = TextLayoutSnapshot.defaultFontRenderContext()

  private val representativeLines = List(
    "Latin"          -> "AVAWAY To the office, fi ffl waffle",
    "code"           -> "val x = a => b != c // note",
    "CJK"            -> "日本語のテキスト mixed 漢字 text",
    "combining"      -> "é ạ̈ ô plain",
    "Devanagari"     -> "क्षि कि नमस्ते",
    "emoji"          -> "hello 😀 👨‍👩‍👧 x",
    "Hebrew"         -> "abc שלום עולם def",
    "Arabic"         -> "مرحبا بالعالم 123 abc",
    "mirrored pairs" -> "(שלום) [abc] {def}"
  )

  private def graphemeStarts(text: String): Vector[Int] =
    CharacterRenderer.computeGraphemeSpans(text).map(_.startLocalIndex)

  it should "draw every glyph of the line exactly once however the line is split into runs" in
    representativeLines.foreach {
      case (name, text) =>
        val stretch = ShapedTextCache().stretchOf(text, font, frc)
        val glyphs  = stretch.glyphsOf(0, text.length).map(_.getNumGlyphs).getOrElse(0)

        withClue(s"$name: ") {
          glyphs should be > 0
          graphemeStarts(text).drop(1).foreach { split =>
            val before = stretch.glyphsOf(0, split).map(_.getNumGlyphs).getOrElse(0)
            val after  = stretch.glyphsOf(split, text.length).map(_.getNumGlyphs).getOrElse(0)
            withClue(s"split at $split: ")(before + after shouldBe glyphs)
          }
        }
    }

  private val kernedInter    = kerned(BundledInterFont.plain(14f))
  private val ligaturedInter = kerned(BundledInterFont.ligatured(14f))

  private def stretchAndCarets(text: String, face: Font) =
    (
      ShapedTextCache().stretchOf(text, face, frc),
      TextLayoutSnapshot.caretXsForText(text, face, ShapedTextCache(), frc)
    )

  "A caret stop" should "sit at the x of the glyph the line draws for its character, kerned or not" in
    List("platform serif" -> font, "bundled Inter" -> kernedInter).foreach {
      case (name, face) =>
        val text                = "Hello, wide world: AVAWAY To iiii WWWW"
        val (stretch, carets)   = stretchAndCarets(text, face)
        def glyphX(column: Int) = stretch.leftEdgeOf(column).getOrElse(Float.NaN)

        stretch.glyphCount shouldBe text.length
        graphemeStarts(text).foreach { column =>
          withClue(s"$name, column $column: ")(carets(column) shouldBe glyphX(column) +- 0.01f)
        }
    }

  it should "start the line at zero and end it at the shaped advance, which no neighbour pulls" in
    List(font, kernedInter, ligaturedInter).foreach { face =>
      val text              = "AVAWAY To WWWW"
      val (stretch, carets) = stretchAndCarets(text, face)

      carets.head shouldBe 0.0f +- 0.01f
      carets.last shouldBe stretch.advance +- 0.01f
    }

  // The platform's faces form whatever ligatures their fonts and the platform's shaper give them -- none for a
  // monospace face, and none for Windows' serif -- so only the bundled face is required to ligate.
  it should "divide a ligature's span evenly between the characters it stands for" in
    List(
      ("platform serif", font, false),
      ("platform monospace", kerned(new Font(Font.MONOSPACED, Font.PLAIN, 14)), false),
      ("ligatured Inter", ligaturedInter, true)
    ).foreach {
      case (name, face, mustLigate) =>
        val text              = "office waffle ffl fi -> => AV"
        val (stretch, carets) = stretchAndCarets(text, face)
        val glyphStarts       = (0 until stretch.glyphCount).map(stretch.charIndexAt)
        val ligatures = glyphStarts.zip(glyphStarts.drop(1) :+ text.length).filter((from, until) => until - from > 1)

        if mustLigate then withClue(s"$name: ")(ligatures should not be empty)
        ligatures.foreach { (from, until) =>
          val startX = stretch.leftEdgeOf(from).getOrElse(Float.NaN)
          val endX   = stretch.leftEdgeOf(until).getOrElse(stretch.advance)
          (from to until).foreach { column =>
            withClue(s"$name, ligature $from-$until, column $column: ") {
              carets(column) shouldBe startX + (endX - startX) * (column - from) / (until - from) +- 0.01f
            }
          }
        }
    }

  it should "sit at the leading edge of the glyph in a right-to-left run, and at the glyph's x in a left-to-right one" in {
    val text = "AVAWAY To \u05e9\u05dc\u05d5\u05dd \u05e2\u05d5\u05dc\u05dd WWWW \u0645\u0631\u062d\u0628\u0627 Ty"
    val (stretch, carets) = stretchAndCarets(text, font)
    val glyphs            = stretch.glyphsOf(0, text.length).getOrElse(fail("no glyphs"))
    val bidi              = java.text.Bidi(text, java.text.Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT)

    bidi.isMixed shouldBe true
    graphemeStarts(text).foreach { column =>
      val own = (0 until stretch.glyphCount).filter(stretch.charIndexAt(_) == column)
      withClue(s"column $column: ") {
        own should not be empty
        val expected =
          if bidi.getLevelAt(column) % 2 == 1 then
            own.map(glyph => stretch.xAt(glyph) + glyphs.getGlyphMetrics(glyph).getAdvance).max
          else own.map(stretch.xAt).min
        carets(column) shouldBe expected +- 0.01f
      }
    }
  }

  "ShapedTextCache" should "serve a repeated text, font and render context without shaping it again" in {
    val cache = ShapedTextCache()

    val first = cache.stretchOf("Open file...", font, frc)
    val again = cache.stretchOf("Open file...", font, frc)

    again should be theSameInstanceAs first
    cache.shapings shouldBe 1L
  }

  it should "shape again for a different font or render context, so a change of either cannot serve stale glyphs" in {
    val cache = ShapedTextCache()
    val _     = cache.stretchOf("Open file...", font, frc)

    val _ = cache.stretchOf("Open file...", font.deriveFont(20.0f), frc)
    val _ = cache.stretchOf("Open file...", font, FontRenderContext(AffineTransform(), false, false))

    cache.shapings shouldBe 3L
  }

  it should "keep what the last frames painted and forget older text, so it stays bounded" in {
    val cache = ShapedTextCache(2)
    val texts = (1 to 6).map(index => s"row $index").toList
    texts.foreach(cache.stretchOf(_, font, frc))
    val shapedSoFar = cache.shapings

    val _ = cache.stretchOf(texts.lastOption.getOrElse(""), font, frc)
    cache.shapings shouldBe shapedSoFar

    val _ = cache.stretchOf(texts.headOption.getOrElse(""), font, frc)
    cache.shapings shouldBe shapedSoFar + 1
  }

  it should "promote a stretch from the older generation instead of shaping it again" in {
    val cache = ShapedTextCache(2)
    List("a", "b", "c").foreach(cache.stretchOf(_, font, frc))
    val shapedSoFar = cache.shapings

    val _ = cache.stretchOf("a", font, frc)

    cache.shapings shouldBe shapedSoFar
  }
