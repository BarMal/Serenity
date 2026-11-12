package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.SkiaFrames
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What [[StyledFontCacheSpec]] asks of a painting surface, asked of a Skia surface: the styled fonts it paints with
  * come from the owner's cache, and are derived once, however many frames paint them.
  */
class SkiaStyledFontCacheSpec extends AnyFlatSpec with Matchers:

  private val base = new Font(Font.MONOSPACED, Font.PLAIN, 12)

  private val styles = List(
    TextStyle.bold,
    TextStyle.italic,
    TextStyle.underlined,
    TextStyle.boldItalic,
    TextStyle(fontSize = Some(20.0f)),
    TextStyle(fontFamily = Some(Font.SERIF), isStrikethrough = true)
  )

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)

  private def surfaceOver(caches: RenderCaches): RenderSurface =
    SkiaFrames.surfaceOver(SkiaFrames.newFrame(210, 130), metrics, base, 210, 130, 1.0, persists = false, caches)

  "Painting the same styled runs on a second frame" should "derive no new font" in {
    val caches    = RenderCaches.create()
    val cache     = caches.styledFonts
    val runStyles = TextStyle.normal :: styles

    def paintFrame(): Unit =
      val surface = surfaceOver(caches)
      surface.clearViewport(RenderColor.fromAwt(Color.BLACK))
      surface.setForegroundColor(RenderColor.fromAwt(Color.WHITE))
      runStyles.zipWithIndex.foreach { (style, row) =>
        surface.enableStyle(style)
        surface.text.drawRunPx(0f, row * 13, 70f, 13, 10, "styled")
        surface.disableStyle(style)
      }
      surface.flush()

    paintFrame()
    val afterFirstFrame = cache.derivations
    paintFrame()

    afterFirstFrame shouldBe styles.size.toLong
    cache.derivations shouldBe afterFirstFrame
  }

  "Painting normal-style runs" should "derive no font at all" in {
    val caches  = RenderCaches.create()
    val surface = surfaceOver(caches)

    (0 until 5).foreach { row =>
      surface.enableStyle(TextStyle.normal)
      surface.text.drawRunPx(0f, row * 13, 70f, 13, 10, "plain")
      surface.disableStyle(TextStyle.normal)
    }

    caches.styledFonts.derivations shouldBe 0L
  }
