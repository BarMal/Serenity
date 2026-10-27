package com.serenity.ui.skia

import java.awt.Font
import java.util.concurrent.atomic.AtomicInteger

import org.jetbrains.skia.Typeface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SkiaFontCacheSpec extends AnyFlatSpec with Matchers:

  final private class CountingResolver:
    val calls = new AtomicInteger(0)

    def apply(family: String, bold: Boolean, italic: Boolean): Typeface =
      val _ = calls.incrementAndGet()
      SkiaTypefaces.typefaceFor(family, bold, italic)

  "SkiaFontCache" should "look a typeface up once for every size of a family and style" in {
    val resolver = new CountingResolver
    val cache    = new SkiaFontCache(resolveTypeface = resolver(_, _, _))

    List(14, 16, 18).foreach(size => cache.faceFor(new Font(Font.SERIF, Font.PLAIN, size)))

    resolver.calls.get shouldBe 1
    cache.size shouldBe 3
  }

  it should "look a typeface up for each family and style it is asked to draw" in {
    val resolver = new CountingResolver
    val cache    = new SkiaFontCache(resolveTypeface = resolver(_, _, _))

    List(
      new Font(Font.SERIF, Font.PLAIN, 14),
      new Font(Font.SERIF, Font.BOLD, 14),
      new Font(Font.SERIF, Font.ITALIC, 14),
      new Font(Font.SANS_SERIF, Font.PLAIN, 14)
    ).foreach(cache.faceFor)

    resolver.calls.get shouldBe 4
  }

  it should "size each face from its own font when they share a typeface" in {
    val cache = new SkiaFontCache()

    val small = cache.faceFor(new Font(Font.SERIF, Font.PLAIN, 14)).font
    val large = cache.faceFor(new Font(Font.SERIF, Font.PLAIN, 28)).font

    small.getSize shouldBe 14f
    large.getSize shouldBe 28f
    large.measureTextWidth("Hoxi") should be > small.measureTextWidth("Hoxi")
  }
