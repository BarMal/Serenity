package com.serenity.ui.skia

import java.awt.Font
import java.awt.font.TextAttribute
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.renderer.FontSpec
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

  "SkiaFontCache" should "give two AWT fonts that describe the same spec one face" in {
    val cache  = new SkiaFontCache()
    val first  = FontSpec.fromAwt(serif(Font.PLAIN, 14))
    val second = FontSpec.fromAwt(serif(Font.PLAIN, 14).deriveFont(14.0f))
    (cache.faceFor(first) eq cache.faceFor(second)) shouldBe true
    cache.size shouldBe 1
  }

  it should "choose a face from the family and style alone, so a spec made from data and one read from AWT share it" in {
    val resolver = new CountingResolver
    val cache    = new SkiaFontCache(resolveTypeface = resolver(_))
    val data     = FontSpec(Font.SERIF, "Serif", 14f, false, false, false, false)()
    val read     = FontSpec.fromAwt(serif(Font.PLAIN, 14))

    cache.faceFor(data).font.getTypeface.getUniqueId shouldBe cache.faceFor(read).font.getTypeface.getUniqueId
    resolver.calls.get shouldBe 1
  }

  it should "look a typeface up once for fonts that choose their face from the same inputs" in {
    val resolver = new CountingResolver
    val cache    = new SkiaFontCache(resolveTypeface = resolver(_))
    val plain    = serif(Font.PLAIN, 14)
    val struck   = struckThrough(plain)
    struck should not be plain

    cache.faceFor(FontSpec.fromAwt(plain))
    cache.faceFor(FontSpec.fromAwt(struck))

    resolver.calls.get shouldBe 1
    cache.size shouldBe 2
  }

  it should "look a typeface up once for a family and style however many sizes it is used at" in {
    val resolver = new CountingResolver
    val cache    = new SkiaFontCache(resolveTypeface = resolver(_))

    val faces = List(14, 16, 18).map(size => cache.faceFor(FontSpec.fromAwt(serif(Font.PLAIN, size))))

    resolver.calls.get shouldBe 1
    faces.map(_.font.getSize) shouldBe List(14.0f, 16.0f, 18.0f)
    faces.map(_.font.getTypeface.getUniqueId).distinct should have size 1
  }

  it should "look a typeface up for each style of a family" in {
    val resolver = new CountingResolver
    val cache    = new SkiaFontCache(resolveTypeface = resolver(_))

    List(Font.PLAIN, Font.BOLD, Font.ITALIC, Font.BOLD | Font.ITALIC).foreach(style =>
      cache.faceFor(FontSpec.fromAwt(serif(style, 14)))
    )

    resolver.calls.get shouldBe 4
  }

  it should "build the same Skia font from a remembered typeface as from a fresh lookup" in {
    val cache  = new SkiaFontCache()
    val plain  = serif(Font.PLAIN, 14)
    val struck = struckThrough(plain)

    cache.faceFor(FontSpec.fromAwt(plain))
    val remembered = cache.faceFor(FontSpec.fromAwt(struck)).font
    val fresh      = SkiaTypefaces.fontFor(FontSpec.fromAwt(struck))

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

    cache.faceFor(FontSpec.fromAwt(first))
    cache.faceFor(FontSpec.fromAwt(serif(Font.PLAIN, 16)))
    cache.faceFor(FontSpec.fromAwt(first))

    resolver.calls.get shouldBe 3
  }
