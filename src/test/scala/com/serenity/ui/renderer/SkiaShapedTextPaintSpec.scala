package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.TextAttribute

import com.serenity.SkiaFrames
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import com.serenity.ui.skia.SkiaTextCaches
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What [[ShapedTextPaintSpec]] asks of the shaping a frame does when it paints a measured line, asked of a Skia
  * surface and its owner's Skia shaped-text cache: a line is shaped once however many colour runs it has, a repaint
  * finds it, and only a change of text or style shapes it again.
  */
class SkiaShapedTextPaintSpec extends AnyFlatSpec with Matchers:

  private val theme   = Theme.light
  private val metrics = CellMetrics(charWidth = 8, lineHeight = 24, ascent = 18)

  private val font = new Font(Font.SERIF, Font.PLAIN, 14).deriveFont(
    java.util.Map.of[TextAttribute, Object](
      TextAttribute.LIGATURES,
      TextAttribute.LIGATURES_ON,
      TextAttribute.KERNING,
      TextAttribute.KERNING_ON
    )
  )

  private val frc         = TextLayoutSnapshot.defaultFontRenderContext()
  private val widthPx     = 600
  private val heightPx    = 60
  private val deviceScale = 2.0

  private val red   = RenderColor.fromRgba(220, 20, 20)
  private val blue  = RenderColor.fromRgba(20, 20, 220)
  private val black = RenderColor.fromRgba(0, 0, 0)
  private val white = RenderColor.fromRgba(255, 255, 255)

  private def shapings(caches: RenderCaches): Long = SkiaTextCaches.of(caches.backendCaches).shapedText.shapings

  private def surfaceOver(caches: RenderCaches): RenderSurface =
    val frame = SkiaFrames.newFrame(
      SurfaceSizing.deviceImageDimension(widthPx, deviceScale),
      SurfaceSizing.deviceImageDimension(heightPx, deviceScale)
    )
    val surface =
      SkiaFrames.surfaceOver(frame, metrics, font, widthPx, heightPx, deviceScale, persists = false, caches)
    surface.setForegroundColor(black)
    surface.setBackgroundColor(white)
    surface

  private def graphemeStarts(text: String): Vector[Int] =
    CharacterRenderer.computeGraphemeSpans(text).map(_.startLocalIndex)

  private def lineOf(text: String): TextVisualLine =
    val carets = TextLayoutSnapshot.caretXsForText(text, font, com.serenity.ui.renderer.ShapedTextCache(), frc)
    val stops =
      (graphemeStarts(text) :+ text.length).map(column => TextCaretStop(column, carets.lift(column).getOrElse(0.0f)))
    TextVisualLine(0, 0, text.length, text, carets.lastOption.getOrElse(0.0f), stops)

  private def paintLine(surface: RenderSurface, text: String, segments: List[StyledText]): Unit =
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
    val caches = RenderCaches.create()

    paintLine(surfaceOver(caches), codeLine, threeColours)

    shapings(caches) shouldBe 1L
  }

  it should "shape nothing when the same line is painted again, on a fresh surface or the same one" in {
    val caches = RenderCaches.create()
    paintLine(surfaceOver(caches), codeLine, threeColours)

    paintLine(surfaceOver(caches), codeLine, threeColours)
    val surface = surfaceOver(caches)
    paintLine(surface, codeLine, threeColours)
    paintLine(surface, codeLine, threeColours)

    shapings(caches) shouldBe 1L
  }

  it should "shape each stretch of one style once, since styles draw in different fonts" in {
    val caches = RenderCaches.create()
    val mixed = List(
      StyledText("val ", TextStyle.normal, black, white),
      StyledText("answer", TextStyle.bold, red, white),
      StyledText(" = 42 // note", TextStyle.normal, blue, white)
    )

    paintLine(surfaceOver(caches), codeLine, mixed)
    paintLine(surfaceOver(caches), codeLine, mixed)

    shapings(caches) shouldBe 3L
  }

  it should "shape a line again only when its text or font changes" in {
    val caches = RenderCaches.create()
    paintLine(surfaceOver(caches), codeLine, threeColours)

    paintLine(
      surfaceOver(caches),
      codeLine + "!",
      List(StyledText(codeLine + "!", TextStyle.normal, black, white))
    )

    shapings(caches) shouldBe 2L
  }

  it should "shape a ligature that spans a colour boundary as part of the one line" in {
    val caches = RenderCaches.create()
    val split = List(
      StyledText("of", TextStyle.normal, red, white),
      StyledText("fi", TextStyle.normal, blue, white),
      StyledText("ce waffle", TextStyle.normal, black, white)
    )

    paintLine(surfaceOver(caches), "office waffle", split)

    shapings(caches) shouldBe 1L
  }
