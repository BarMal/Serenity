package com.serenity.ui.skia

import java.awt.Font
import java.awt.font.TextAttribute
import java.util.concurrent.atomic.AtomicInteger

import org.jetbrains.skia.Typeface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SkiaFontCacheSpec extends AnyFlatSpec with Matchers:

  final private class CountingResolver:
    val calls = new AtomicInteger(0)

    def apply(key: SkiaTypefaces.TypefaceKey): Typeface =
      val _ = calls.incrementAndGet()
      SkiaTypefaces.typefaceFor(key)

  private def serif(style: Int, size: Int): Font = new Font(Font.SERIF, style, size)

  /** A different font to AWT that measures every string the same, so it chooses its face from the same inputs. */
  private def struckThrough(font: Font): Font =
    font.deriveFont(java.util.Map.of(TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON))

  "SkiaFontCache" should "look a typeface up once for fonts that choose their face from the same inputs" in {
    val resolver = new CountingResolver
    val cache    = new SkiaFontCache(resolveTypeface = resolver(_))
    val plain    = serif(Font.PLAIN, 14)
    val struck   = struckThrough(plain)
    struck should not be plain

    cache.faceFor(plain)
    cache.faceFor(struck)

    resolver.calls.get shouldBe 1
    cache.size shouldBe 2
  }

  it should "look a typeface up again for another size, which can choose another face" in {
    val resolver = new CountingResolver
    val cache    = new SkiaFontCache(resolveTypeface = resolver(_))

    List(14, 16, 18).foreach(size => cache.faceFor(serif(Font.PLAIN, size)))

    resolver.calls.get shouldBe 3
  }

  it should "look a typeface up for each style of a family" in {
    val resolver = new CountingResolver
    val cache    = new SkiaFontCache(resolveTypeface = resolver(_))

    List(Font.PLAIN, Font.BOLD, Font.ITALIC, Font.BOLD | Font.ITALIC).foreach(style => cache.faceFor(serif(style, 14)))

    resolver.calls.get shouldBe 4
  }

  it should "build the same Skia font from a remembered typeface as from a fresh lookup" in {
    val cache  = new SkiaFontCache()
    val plain  = serif(Font.PLAIN, 14)
    val struck = struckThrough(plain)

    cache.faceFor(plain)
    val remembered = cache.faceFor(struck).font
    val fresh      = SkiaTypefaces.fontFor(struck)

    remembered.getTypeface.getUniqueId shouldBe fresh.getTypeface.getUniqueId
    remembered.getSize shouldBe fresh.getSize
    remembered.getSkewX shouldBe fresh.getSkewX
    remembered.isEmboldened shouldBe fresh.isEmboldened
    remembered.measureTextWidth("Hoxi AVATAR fi To") shouldBe fresh.measureTextWidth("Hoxi AVATAR fi To")
  }

  it should "forget remembered typefaces with the faces it drops" in {
    val resolver = new CountingResolver
    val cache    = new SkiaFontCache(maxFaces = 1, resolveTypeface = resolver(_))
    val first    = serif(Font.PLAIN, 14)

    cache.faceFor(first)
    cache.faceFor(serif(Font.PLAIN, 16))
    cache.faceFor(first)

    resolver.calls.get shouldBe 3
  }
