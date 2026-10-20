package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage

import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{BundledInterFont, CellMetrics, TextLayoutSnapshot}
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A frame paints text from glyphs shaped once, not by shaping every colour run again: the shaping happens when a line
  * is first seen, a repaint finds it, and what is drawn is what `drawString` draws.
  */
class ShapedTextPaintSpec extends AnyFlatSpec with Matchers:

  private val theme   = Theme.light
  private val metrics = CellMetrics(charWidth = 8, lineHeight = 24, ascent = 18)

  private def kerned(face: Font): Font = face.deriveFont(
    java.util.Map.of[TextAttribute, Object](
      TextAttribute.LIGATURES,
      TextAttribute.LIGATURES_ON,
      TextAttribute.KERNING,
      TextAttribute.KERNING_ON
    )
  )

  private val font = kerned(new Font(Font.SERIF, Font.PLAIN, 14))

  private val frc         = TextLayoutSnapshot.defaultFontRenderContext()
  private val widthPx     = 600
  private val heightPx    = 60
  private val deviceScale = 2.0

  private val red   = RenderColor.fromRgba(220, 20, 20)
  private val blue  = RenderColor.fromRgba(20, 20, 220)
  private val black = RenderColor.fromRgba(0, 0, 0)
  private val white = RenderColor.fromRgba(255, 255, 255)

  private val blankArgb = 0
  private val whiteArgb = 0xffffffff

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

  private val leftToRightLines =
    representativeLines.filterNot((name, _) => List("Hebrew", "Arabic", "mirrored pairs").contains(name))

  private def freshImage() = new BufferedImage(widthPx * 2, heightPx * 2, BufferedImage.TYPE_INT_ARGB)

  private def surfaceOver(image: BufferedImage, cache: ShapedTextCache): Java2DRenderSurface =
    val surface = new Java2DRenderSurface(
      image,
      metrics,
      font,
      _ => (),
      widthPx,
      heightPx,
      deviceScale,
      deviceScale,
      textCaches = SurfaceTextCaches.fresh().copy(shapedText = cache)
    )
    surface.setForegroundColor(black)
    surface.setBackgroundColor(white)
    surface

  private def paintedWith(cache: ShapedTextCache)(paint: Java2DRenderSurface => Unit): BufferedImage =
    val image   = freshImage()
    val surface = surfaceOver(image, cache)
    paint(surface)
    image

  private def inkOf(image: BufferedImage): Int =
    (for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
      if image.getRGB(x, y) != blankArgb && image.getRGB(x, y) != whiteArgb
    yield 1).sum

  private def differingPixels(a: BufferedImage, b: BufferedImage): List[(Int, Int)] =
    (for
      y <- 0 until a.getHeight
      x <- 0 until a.getWidth
      if a.getRGB(x, y) != b.getRGB(x, y)
    yield (x, y)).toList

  private def graphemeStarts(text: String): Vector[Int] =
    CharacterRenderer.computeGraphemeSpans(text).map(_.startLocalIndex)

  private def lineOf(text: String): TextVisualLine =
    val carets = TextLayoutSnapshot.caretXsForText(text, font, ShapedTextCache(), frc)
    val stops =
      (graphemeStarts(text) :+ text.length).map(column => TextCaretStop(column, carets.lift(column).getOrElse(0.0f)))
    TextVisualLine(0, 0, text.length, text, carets.lastOption.getOrElse(0.0f), stops)

  private def paintLine(surface: Java2DRenderSurface, text: String, segments: List[StyledText]): Unit =
    CharacterRenderer.renderMeasuredLine(
      surface,
      4.0f,
      4,
      metrics.lineHeight,
      metrics.ascent,
      lineOf(text),
      theme,
      styledSegments = Some(segments)
    )

  private val codeLine = "val answer = 42 // note"

  private val threeColours = List(
    StyledText("val ", TextStyle.normal, red, white),
    StyledText("answer", TextStyle.normal, blue, white),
    StyledText(" = 42 // note", TextStyle.normal, black, white)
  )

  "Painting a measured line" should "shape it once, however many colour runs it has" in {
    val cache = ShapedTextCache()

    val _ = paintedWith(cache)(paintLine(_, codeLine, threeColours))

    cache.shapings shouldBe 1L
  }

  it should "shape nothing when the same line is painted again, on a fresh surface or the same one" in {
    val cache = ShapedTextCache()
    val first = paintedWith(cache)(paintLine(_, codeLine, threeColours))

    val again = paintedWith(cache)(paintLine(_, codeLine, threeColours))
    val _ = paintedWith(cache) { surface =>
      paintLine(surface, codeLine, threeColours)
      paintLine(surface, codeLine, threeColours)
    }

    cache.shapings shouldBe 1L
    differingPixels(again, first).take(6) shouldBe Nil
  }

  it should "shape each stretch of one style once, since styles draw in different fonts" in {
    val cache = ShapedTextCache()
    val mixed = List(
      StyledText("val ", TextStyle.normal, black, white),
      StyledText("answer", TextStyle.bold, red, white),
      StyledText(" = 42 // note", TextStyle.normal, blue, white)
    )

    val _ = paintedWith(cache)(paintLine(_, codeLine, mixed))
    val _ = paintedWith(cache)(paintLine(_, codeLine, mixed))

    cache.shapings shouldBe 3L
  }

  it should "shape a line again only when its text or font changes" in {
    val cache = ShapedTextCache()
    val _     = paintedWith(cache)(paintLine(_, codeLine, threeColours))

    val _ =
      paintedWith(cache)(paintLine(_, codeLine + "!", List(StyledText(codeLine + "!", TextStyle.normal, black, white))))

    cache.shapings shouldBe 2L
  }

  it should "shape a ligature that spans a colour boundary as part of the one line" in {
    val cache = ShapedTextCache()
    val split = List(
      StyledText("of", TextStyle.normal, red, white),
      StyledText("fi", TextStyle.normal, blue, white),
      StyledText("ce waffle", TextStyle.normal, black, white)
    )

    val _ = paintedWith(cache)(paintLine(_, "office waffle", split))

    cache.shapings shouldBe 1L
  }

  "A run drawn from shaped text" should "paint the pixels drawString paints, for every representative line" in
    representativeLines.foreach {
      case (name, text) =>
        val expected = paintedWith(ShapedTextCache()) { surface =>
          surface.drawRunPx(4.0f, 4, 560.0f, metrics.lineHeight, metrics.ascent, text)
        }
        val actual = paintedWith(ShapedTextCache()) { surface =>
          surface.drawShapedRunPx(4.0f, 4, 560.0f, metrics.lineHeight, metrics.ascent, text, 0, text.length)
        }

        withClue(s"$name: ") {
          inkOf(expected) should be > 0
          differingPixels(actual, expected).take(6) shouldBe Nil
        }
    }

  it should "continue its neighbour exactly where one shaping put it when a line is split into runs" in
    leftToRightLines.foreach {
      case (name, text) =>
        val cache    = ShapedTextCache()
        val split    = text.indexOf(' ', text.length / 2) + 1
        val nextEdge = cache.stretchOf(text, font, surfaceFrc(cache)).leftEdgeOf(split)
        val expected = paintedWith(ShapedTextCache()) { surface =>
          surface.drawRunPx(4.0f, 4, 560.0f, metrics.lineHeight, metrics.ascent, text)
        }
        val actual = paintedWith(cache) { surface =>
          surface.drawShapedRunPx(4.0f, 4, 560.0f, metrics.lineHeight, metrics.ascent, text, 0, split)
          surface.drawShapedRunPx(
            4.0f + nextEdge.getOrElse(0.0f),
            4,
            560.0f - nextEdge.getOrElse(0.0f),
            metrics.lineHeight,
            metrics.ascent,
            text,
            split,
            text.length
          )
        }

        withClue(s"$name: ") {
          nextEdge should not be empty
          differingPixels(actual, expected).take(6) shouldBe Nil
        }
    }

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

  it should "leave a colour boundary's second run where one shaping puts it, even between a kerned pair" in {
    val text    = "AVAWAY To the office"
    val split   = 1
    val cache   = ShapedTextCache()
    val stretch = cache.stretchOf(text, font, surfaceFrc(cache))
    val glyphX  = stretch.leftEdgeOf(split).getOrElse(Float.NaN)
    val colours = List(
      StyledText("A", TextStyle.normal, red, white),
      StyledText("VAWAY To the office", TextStyle.normal, blue, white)
    )
    val expected = paintedWith(cache) { surface =>
      surface.setForegroundColor(red)
      surface.drawShapedRunPx(4.0f, 4, glyphX, metrics.lineHeight, metrics.ascent, text, 0, split)
      surface.setForegroundColor(blue)
      surface.drawShapedRunPx(
        4.0f + glyphX,
        4,
        stretch.advance - glyphX,
        metrics.lineHeight,
        metrics.ascent,
        text,
        split,
        text.length
      )
    }
    val actual = paintedWith(ShapedTextCache())(paintLine(_, text, colours))

    glyphX should not be 0.0f
    inkOf(expected) should be > 0
    differingPixels(actual, expected).take(6) shouldBe Nil
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

  "A plain font over simple text" should "draw with drawString, which shapes nothing there, and leave the cache alone" in {
    val plain = new Font(Font.SERIF, Font.PLAIN, 14)
    val cache = ShapedTextCache()
    val image = freshImage()
    val surface = new Java2DRenderSurface(
      image,
      metrics,
      plain,
      _ => (),
      widthPx,
      heightPx,
      deviceScale,
      deviceScale,
      textCaches = SurfaceTextCaches.fresh().copy(shapedText = cache)
    )
    surface.setForegroundColor(black)
    surface.setBackgroundColor(white)

    paintLine(surface, codeLine, threeColours)

    cache.shapings shouldBe 0L
    inkOf(image) should be > 0
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

  private def surfaceFrc(cache: ShapedTextCache): FontRenderContext =
    surfaceOver(freshImage(), cache).fontRenderContext.getOrElse(frc)
