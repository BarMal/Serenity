package com.serenity.ui.skia

import java.awt.Font
import java.awt.font.TextAttribute

import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.{BackendCaches, CacheSlots, FontSpec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Shaping a text in a font gives the same glyphs wherever it is asked for, so the owners built over one set of shared
  * slots (a window's surfaces, its state, its next window) shape it once between them, as they resolve a typeface once.
  */
class SharedShapingsSpec extends AnyFlatSpec with Matchers:

  private val text  = "The quick brown fox jumps over the lazy dog, twice over and then once again."
  private val plain = LineFonts(FontSpec.fromAwt(Font(Font.SERIF, Font.PLAIN, 14)), Vector.empty)

  private def measurerOver(slots: CacheSlots): SkiaTextMeasurer = SkiaTextMeasurer.of(BackendCaches(slots))

  "Owners over the same shared slots" should "shape a text once between them" in {
    val slots  = CacheSlots()
    val first  = measurerOver(slots)
    val second = measurerOver(slots)

    val _ = first.exactCaretXs(text, 0, plain)
    first.shaperRuns should be > 0L

    val again = second.exactCaretXs(text, 0, plain)
    second.shaperRuns shouldBe 0L
    again.toVector shouldBe first.exactCaretXs(text, 0, plain).toVector
  }

  it should "measure the advances of a text once between them too" in {
    val slots  = CacheSlots()
    val first  = measurerOver(slots)
    val second = measurerOver(slots)

    val held = first.advances(text, 0, text.length, 0, plain)
    second.advances(text, 0, text.length, 0, plain).caretsFrom(0, text.length).toVector shouldBe held.caretsFrom(0, text.length).toVector
    second.shaperRuns shouldBe 0L
  }

  "Owners over different slots" should "each shape for themselves" in {
    val first  = measurerOver(CacheSlots())
    val second = measurerOver(CacheSlots())

    val _ = first.exactCaretXs(text, 0, plain)
    val __ = second.exactCaretXs(text, 0, plain)

    second.shaperRuns should be > 0L
  }

  "A shared memo" should "keep the shaping with ligatures apart from the shaping without them" in {
    val ligated   = Font(Font.SERIF, Font.PLAIN, 14).deriveFont(java.util.Map.of(TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON))
    val unligated = Font(Font.SERIF, Font.PLAIN, 14)
    val on        = LineFonts(FontSpec.fromAwt(ligated), Vector.empty)
    val off       = LineFonts(FontSpec.fromAwt(unligated), Vector.empty)
    val ffi       = "office affinity, difficult files"
    val slots     = CacheSlots()

    val withLigatures = measurerOver(slots).exactCaretXs(ffi, 0, on).toVector
    val shared        = measurerOver(slots).exactCaretXs(ffi, 0, off).toVector
    val alone         = measurerOver(CacheSlots()).exactCaretXs(ffi, 0, off).toVector

    shared shouldBe alone
    withLigatures.length shouldBe shared.length
  }
