package com.serenity.ui.skia

import java.awt.Font

import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.{BackendCaches, FontSpec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SkiaTextMeasurerOwnerSpec extends AnyFlatSpec with Matchers:

  private val font = FontSpec.fromAwt(new Font(Font.SERIF, Font.PLAIN, 14))

  "SkiaTextMeasurer.of" should "give an owner one measurer however often it is asked" in {
    val owner = new BackendCaches
    (SkiaTextMeasurer.of(owner) eq SkiaTextMeasurer.of(owner)) shouldBe true
  }

  it should "give two owners two measurers" in {
    (SkiaTextMeasurer.of(new BackendCaches) eq SkiaTextMeasurer.of(new BackendCaches)) shouldBe false
  }

  it should "measure with the faces the owner's surface draws with" in {
    val owner = new BackendCaches
    val _     = SkiaTextMeasurer.of(owner).glyphWidthPx(font, "Hello")
    SkiaTextCaches.of(owner).fonts.size shouldBe 1
    SkiaTextCaches.of(owner).fonts.faceFor(font).font.measureTextWidth("Hello") shouldBe
      SkiaTextMeasurer.of(owner).glyphWidthPx(font, "Hello")
  }

  it should "measure the stretches the owner's surface draws" in {
    val owner  = new BackendCaches
    val before = SkiaTextCaches.of(owner).shapedText.lookups
    val _      = SkiaTextMeasurer.of(owner).exactCaretXs("café 😀", 0, LineFonts(font, Vector.empty))
    SkiaTextCaches.of(owner).shapedText.lookups should be > before
  }
