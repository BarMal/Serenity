package com.serenity.ui.layout

import java.awt.Font
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.renderer.{FontProbes, ShapedTextCache}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a font is probed for (does shaping substitute glyphs, do its advances agree, may a row be cut here) is
  * remembered by the owner that asked, not by the JVM: a font one owner has probed is still unprobed for another.
  */
class FontProbesOwnershipSpec extends AnyFlatSpec with Matchers:

  private val frc            = TextLayoutSnapshot.defaultFontRenderContext()
  private val sizes          = AtomicInteger(0)
  private val base           = Font(Font.SERIF, Font.PLAIN, 14)
  private def unseen(): Font = base.deriveFont(14.0f + sizes.incrementAndGet() * 0.0013f)

  private def twice[A](probe: => A): A =
    val first = probe
    probe shouldBe first
    first

  "A layout-advance probe" should "run once per owner and font" in {
    val (a, b) = (FontProbes(), FontProbes())
    val font   = unseen()

    twice(GlyphAdvances.layoutAdvanceMatchesSum(font, frc, a))

    a.probesRun shouldBe 1L
    b.probesRun shouldBe 0L
  }

  "A substitution probe" should "run once per owner and font" in {
    val (a, b) = (FontProbes(), FontProbes())
    val font   = unseen()

    twice(GlyphAdvances.shapingMaySubstitute(font, frc, a))

    a.probesRun shouldBe 1L
    b.probesRun shouldBe 0L
  }

  "A uniform-advance probe" should "run once per owner and font" in {
    val (a, b) = (FontProbes(), FontProbes())
    val font   = unseen()

    twice(GlyphAdvances.advancesAreUniform(font, frc, a))

    a.probesRun shouldBe 1L
    b.probesRun shouldBe 0L
  }

  "A shaping-barrier probe" should "run in the owner's cache and no other's" in {
    val (a, b) = (ShapedTextCache(), ShapedTextCache())
    val font   = unseen()

    twice(ShapingBarriers.cutsCleanly(font, frc, 'T', a))

    a.probes.probesRun should be > 0L
    b.probes.probesRun shouldBe 0L
  }

  "The measured-layout decision" should "be taken once per owner and font" in {
    val (a, b) = (FontProbes(), FontProbes())
    val font   = unseen()

    twice(TextLayoutSnapshot.shouldUseMeasuredLayout(font, frc, a))

    a.probesRun shouldBe 1L
    b.probesRun shouldBe 0L
  }

  "A ShapedTextCache" should "carry its own probes" in {
    (ShapedTextCache().probes `ne` ShapedTextCache().probes) shouldBe true
  }

  "A wrapped-line cache that remembers" should "probe through the cache that shapes for it" in {
    val owner = WrappedLineCache.bounded()
    val font  = unseen()

    TextLayoutSnapshot.boundedVisualLinesForText("a few words to lay out", 0, 300, font, frc, wrapCache = owner)

    owner.shapedText.probes.probesRun should be > 0L
  }

  "An owner's probes" should "stay within their bound however many fonts it meets" in {
    val probes = FontProbes(capacityPerGeneration = 2)
    val fonts  = Vector.fill(10)(unseen())

    fonts.foreach(GlyphAdvances.layoutAdvanceMatchesSum(_, frc, probes))
    probes.probesRun shouldBe 10L

    GlyphAdvances.layoutAdvanceMatchesSum(fonts.head, frc, probes)
    probes.probesRun shouldBe 11L
  }

  it should "keep answering from memory while the fonts fit" in {
    val probes = FontProbes(capacityPerGeneration = 2)
    val fonts  = Vector.fill(2)(unseen())

    fonts.foreach(GlyphAdvances.layoutAdvanceMatchesSum(_, frc, probes))
    fonts.foreach(GlyphAdvances.layoutAdvanceMatchesSum(_, frc, probes))

    probes.probesRun shouldBe 2L
  }
