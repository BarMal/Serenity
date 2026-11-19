package com.serenity.ui.skia

import java.awt.Font
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.renderer.{BackendCaches, CacheSlots, FontSpec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SharedTypefacesSpec extends AnyFlatSpec with Matchers:

  private val font = FontSpec.fromAwt(new Font(Font.SERIF, Font.PLAIN, 14))

  private def countingHolder(resolved: AtomicInteger): SharedTypefaces =
    SharedTypefaces(key =>
      resolved.incrementAndGet()
      SkiaTypefaces.typefaceFor(key)
    )

  "Two font caches built over one holder" should "resolve a typeface once between them" in {
    val resolved = AtomicInteger(0)
    val holder   = countingHolder(resolved)
    val one      = new SkiaFontCache(resolveTypeface = holder.typefaceFor)
    val two      = new SkiaFontCache(resolveTypeface = holder.typefaceFor)

    val first  = one.faceFor(font).font.getTypeface
    val second = two.faceFor(font).font.getTypeface

    second.getUniqueId shouldBe first.getUniqueId
    resolved.get shouldBe 1
  }

  it should "resolve every logical family to the same typeface" in {
    val holder = SharedTypefaces()
    val one    = new SkiaFontCache(resolveTypeface = holder.typefaceFor)
    val two    = new SkiaFontCache(resolveTypeface = holder.typefaceFor)

    List(Font.SANS_SERIF, Font.SERIF, Font.MONOSPACED, Font.DIALOG, Font.DIALOG_INPUT).foreach { family =>
      val spec = FontSpec.fromAwt(new Font(family, Font.PLAIN, 14))
      withClue(family)(
        two.faceFor(spec).font.getTypeface.getUniqueId shouldBe one.faceFor(spec).font.getTypeface.getUniqueId
      )
    }
  }

  "Font caches over separate holders" should "each resolve their own" in {
    val resolved = AtomicInteger(0)
    val one      = new SkiaFontCache(resolveTypeface = countingHolder(resolved).typefaceFor)
    val two      = new SkiaFontCache(resolveTypeface = countingHolder(resolved).typefaceFor)

    val _ = (one.faceFor(font), two.faceFor(font))

    resolved.get shouldBe 2
  }

  "Owners built over the same shared slots" should "share one holder, and owners of different slots do not" in {
    val slots = CacheSlots()
    val one   = BackendCaches(slots)
    val two   = BackendCaches(slots)

    (SkiaTextCaches.typefacesOf(one) eq SkiaTextCaches.typefacesOf(two)) shouldBe true
    (SkiaTextCaches.typefacesOf(BackendCaches()) eq SkiaTextCaches.typefacesOf(BackendCaches())) shouldBe false
  }

  "An owner's font cache" should "resolve through the holder of its shared slots" in {
    val slots = CacheSlots()
    val one   = SkiaTextCaches.of(BackendCaches(slots))
    val two   = SkiaTextCaches.of(BackendCaches(slots))

    two.fonts.faceFor(font).font.getTypeface.getUniqueId shouldBe one.fonts.faceFor(font).font.getTypeface.getUniqueId
  }

  "A holder" should "start again once it holds its limit, and resolve again what it let go" in {
    val resolved = AtomicInteger(0)
    val holder = SharedTypefaces(
      key =>
        resolved.incrementAndGet()
        SkiaTypefaces.typefaceFor(key)
      ,
      limit = 1
    )
    val styles = Vector(Font.PLAIN, Font.BOLD, Font.ITALIC).map(style => FontSpec.fromAwt(new Font(Font.SERIF, style, 14)))
    val faces =
      styles.map(spec => new SkiaFontCache(resolveTypeface = holder.typefaceFor).faceFor(spec).font.getTypeface)

    faces should have size 3
    resolved.get should be >= 3
  }
