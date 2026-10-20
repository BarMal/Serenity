package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Color, Font}

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StyledFontCacheSpec extends AnyFlatSpec with Matchers:

  private val base = new Font(Font.MONOSPACED, Font.PLAIN, 12)

  private val styles = List(
    TextStyle.bold,
    TextStyle.italic,
    TextStyle.underlined,
    TextStyle.boldItalic,
    TextStyle(fontSize = Some(20.0f)),
    TextStyle(fontFamily = Some(Font.SERIF), isStrikethrough = true)
  )

  "StyledFontCache.fontFor" should "hand back the same font, without deriving again, for a repeated style" in {
    val cache       = StyledFontCache()
    val first       = styles.map(cache.fontFor(base, _))
    val derivedOnce = cache.derivations

    val second = styles.map(cache.fontFor(base, _))

    second.zip(first).foreach((again, original) => again should be theSameInstanceAs original)
    cache.derivations shouldBe derivedOnce
    derivedOnce shouldBe styles.size.toLong
  }

  it should "give the font the style derivation gives" in {
    val cache = StyledFontCache()
    styles.foreach(style => cache.fontFor(base, style) shouldBe TextStyle.styledFont(base, style))
  }

  it should "use a plain base font as it is for a normal style, deriving nothing" in {
    val cache = StyledFontCache()

    cache.fontFor(base, TextStyle.normal) should be theSameInstanceAs base
    cache.derivations shouldBe 0L
  }

  it should "still derive a normal style over a base font that is not plain" in {
    val cache    = StyledFontCache()
    val boldBase = base.deriveFont(Font.BOLD)

    cache.fontFor(boldBase, TextStyle.normal).isBold shouldBe false
    cache.derivations shouldBe 1L
  }

  it should "derive again for a different base font" in {
    val cache = StyledFontCache()
    val _     = cache.fontFor(base, TextStyle.bold)

    cache.fontFor(base.deriveFont(18.0f), TextStyle.bold).getSize2D shouldBe 18.0f
    cache.derivations shouldBe 2L
  }

  it should "stay within its bounds however many styles it is asked for" in {
    val cache = StyledFontCache(maxBaseFonts = 2, maxStylesPerBaseFont = 3)

    (1 to 40).foreach(size => cache.fontFor(base, TextStyle(fontSize = Some(size.toFloat + 1.0f))).getSize2D)
    cache.fontFor(base, TextStyle(fontSize = Some(7.0f))).getSize2D shouldBe 7.0f
  }

  "StyledFontCache.underlineOf" should "give a visible rule below the baseline" in {
    val underline = StyledFontCache().underlineOf(base)

    underline.thicknessPx should be > 0.0f
    underline.offsetPx should be > 0.0f
  }

  "Painting the same styled runs on a second frame" should "derive no new font" in {
    val cache     = StyledFontCache()
    val metrics   = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
    val runStyles = TextStyle.normal :: styles

    def paintFrame(): Unit =
      val image = new BufferedImage(210, 130, BufferedImage.TYPE_INT_ARGB)
      val surface = new Java2DRenderSurface(
        image,
        metrics,
        base,
        _ => (),
        210,
        130,
        1.0,
        1.0,
        textCaches = SurfaceTextCaches.fresh().copy(styledFonts = cache)
      )
      surface.clearViewport(RenderColor.fromAwt(Color.BLACK))
      surface.setForegroundColor(RenderColor.fromAwt(Color.WHITE))
      runStyles.zipWithIndex.foreach { (style, row) =>
        surface.enableStyle(style)
        surface.drawRunPx(0f, row * 13, 70f, 13, 10, "styled")
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
    val cache   = StyledFontCache()
    val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
    val image   = new BufferedImage(210, 130, BufferedImage.TYPE_INT_ARGB)
    val surface = new Java2DRenderSurface(
      image,
      metrics,
      base,
      _ => (),
      210,
      130,
      1.0,
      1.0,
      textCaches = SurfaceTextCaches.fresh().copy(styledFonts = cache)
    )

    (0 until 5).foreach { row =>
      surface.enableStyle(TextStyle.normal)
      surface.drawRunPx(0f, row * 13, 70f, 13, 10, "plain")
      surface.disableStyle(TextStyle.normal)
    }

    cache.derivations shouldBe 0L
  }
