package com.serenity.ui.layout

import java.awt.Font
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.testkit.TestMeasure
import com.serenity.ui.renderer.FontProbes.{BarrierProbe, Probe}
import com.serenity.ui.renderer.{FontProbes, ShapedTextCache}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a font is probed for (does shaping substitute glyphs, do its advances agree, may a row be cut here) is
  * remembered by the owner that asked, not by the JVM: a font one owner has probed is still unprobed for another.
  * Layout no longer probes AWT fonts at all (#2660 item 2, step D2): it asks its owner's measurer, so a wrap leaves the
  * shaped-text cache's probes untouched.
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

    twice(a.layoutAgreement(Probe(font, frc))(_ => true))

    a.probesRun shouldBe 1L
    b.probesRun shouldBe 0L
  }

  "A substitution probe" should "run once per owner and font" in {
    val (a, b) = (FontProbes(), FontProbes())
    val font   = unseen()

    twice(a.substitution(Probe(font, frc))(_ => false))

    a.probesRun shouldBe 1L
    b.probesRun shouldBe 0L
  }

  "A uniform-advance probe" should "run once per owner and font" in {
    val (a, b) = (FontProbes(), FontProbes())
    val font   = unseen()

    twice(a.uniformAdvance(Probe(font, frc))(_ => true))

    a.probesRun shouldBe 1L
    b.probesRun shouldBe 0L
  }

  "A shaping-barrier probe" should "run in the owner's cache and no other's" in {
    val (a, b) = (ShapedTextCache(), ShapedTextCache())
    val font   = unseen()

    twice(a.probes.barrier(BarrierProbe(font, frc, 'T'))(_ => true))

    a.probes.probesRun should be > 0L
    b.probes.probesRun shouldBe 0L
  }

  "The measured-layout decision" should "be taken once per owner and font" in {
    val (a, b) = (FontProbes(), FontProbes())
    val font   = unseen()

    twice(a.measuredLayout(Probe(font, frc))(_ => true))

    a.probesRun shouldBe 1L
    b.probesRun shouldBe 0L
  }

  "A ShapedTextCache" should "carry its own probes" in {
    (ShapedTextCache().probes `ne` ShapedTextCache().probes) shouldBe true
  }

  "A wrapped-line cache that remembers" should "measure through its owner's measurer and probe nothing" in {
    val owner = TestMeasure.wrapCache()
    val font  = unseen()

    val rows = TextLayoutSnapshot.boundedVisualLinesForText("a few words to lay out", 0, 300, font, wrapCache = owner)

    rows should not be empty
    owner.shapedText.probes.probesRun shouldBe 0L
  }

  "An owner's probes" should "stay within their bound however many fonts it meets" in {
    val probes = FontProbes(capacityPerGeneration = 2)
    val fonts  = Vector.fill(10)(unseen())

    fonts.foreach(font => probes.layoutAgreement(Probe(font, frc))(_ => true))
    probes.probesRun shouldBe 10L

    probes.layoutAgreement(Probe(fonts.head, frc))(_ => true)
    probes.probesRun shouldBe 11L
  }

  it should "keep answering from memory while the fonts fit" in {
    val probes = FontProbes(capacityPerGeneration = 2)
    val fonts  = Vector.fill(2)(unseen())

    fonts.foreach(font => probes.layoutAgreement(Probe(font, frc))(_ => true))
    fonts.foreach(font => probes.layoutAgreement(Probe(font, frc))(_ => true))

    probes.probesRun shouldBe 2L
  }
