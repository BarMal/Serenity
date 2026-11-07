package com.serenity.ui.skia

import java.awt.Font

import com.serenity.ui.layout.BundledInterFont
import org.jetbrains.skia.Typeface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class PinnedInterTypefaceSpec extends AnyFlatSpec with Matchers:

  private def refuse(key: SkiaTypefaces.TypefaceKey): Typeface =
    fail(s"${key.family} went to the system lookup")

  "The pinned Inter resolver" should "answer Inter from the bundled subset, never from what the machine has installed" in {
    val cache = new SkiaFontCache(resolveTypeface = PinnedInterTypeface.resolver(refuse))

    cache.faceFor(BundledInterFont.plain(14f)).font.getTypeface.getFamilyName shouldBe "Inter"
  }

  it should "hand every other family to the lookup it was given" in {
    val asked = new java.util.concurrent.atomic.AtomicReference[String]("")
    val cache = new SkiaFontCache(resolveTypeface = PinnedInterTypeface.resolver { key =>
      asked.set(key.family)
      SkiaTypefaces.typefaceFor(key)
    })

    val _ = cache.faceFor(new Font(Font.SERIF, Font.PLAIN, 14))

    asked.get shouldBe Font.SERIF
  }
