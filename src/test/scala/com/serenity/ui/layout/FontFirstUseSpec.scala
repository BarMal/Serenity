package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.renderer.{FontFirstUse, FontSpec, ShapedStretch, StyledFontCache}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FontFirstUseSpec extends AnyFlatSpec with Matchers:

  private val frc = FontRenderContext(AffineTransform(), true, true)

  // The process-wide settled set is shared with other specs, so every example takes a size none of them uses.
  private val nextSize              = AtomicInteger(901)
  private def unsettledFont(): Font = Font(Font.SERIF, Font.PLAIN, nextSize.getAndIncrement())

  private def discard(value: Any): Unit = ()

  private def onThreads(count: Int)(work: Int => Unit): Unit =
    val pool     = Executors.newFixedThreadPool(count)
    val gate     = CountDownLatch(1)
    val finished = CountDownLatch(count)
    val failure  = AtomicReference[Option[Throwable]](None)
    (0 until count).foreach { index =>
      pool.execute { () =>
        gate.await()
        try work(index)
        catch case error: Throwable => failure.set(Some(error))
        finally finished.countDown()
      }
    }
    gate.countDown()
    val _ = finished.await(1, TimeUnit.MINUTES)
    pool.shutdown()
    failure.get.foreach(throw _)

  "settling a font" should "run its first use once however many threads ask" in {
    val settling   = FontFirstUse()
    val font       = unsettledFont()
    val firstUses  = AtomicInteger(0)
    val finishedAt = AtomicInteger(0)
    val seenEarly  = AtomicInteger(0)
    onThreads(16) { _ =>
      settling.settle(
        font,
        frc,
        (_, _) =>
          discard(firstUses.incrementAndGet())
          Thread.sleep(20)
          finishedAt.set(1)
      )
      if finishedAt.get != 1 then seenEarly.set(1)
    }
    firstUses.get shouldBe 1
    seenEarly.get shouldBe 0
    settling.isSettled(font, frc) shouldBe true
  }

  it should "never let the first uses of two fonts overlap" in {
    val settling = FontFirstUse()
    val fonts    = Vector.fill(8)(unsettledFont())
    val inside   = AtomicInteger(0)
    val deepest  = AtomicInteger(0)
    onThreads(fonts.length) { index =>
      settling.settle(
        fonts(index),
        frc,
        (_, _) =>
          discard(deepest.accumulateAndGet(inside.incrementAndGet(), (a, b) => math.max(a, b)))
          Thread.sleep(10)
          discard(inside.decrementAndGet())
      )
    }
    deepest.get shouldBe 1
  }

  it should "treat the same font at another render context as a font not yet used" in {
    val settling = FontFirstUse()
    val font     = unsettledFont()
    val scaled   = FontRenderContext(AffineTransform.getScaleInstance(2.0, 2.0), true, true)
    settling.settle(font, frc)
    settling.isSettled(font, frc) shouldBe true
    settling.isSettled(font, scaled) shouldBe false
  }

  "every path that measures or shapes text" should "settle the font before reading it" in {
    val shaped = unsettledFont()
    ShapedStretch.shape("office", shaped, frc)
    FontFirstUse.isSettled(shaped, frc) shouldBe true

    val extent = unsettledFont()
    FontLineExtents.shared(FontLineExtents.Key(extent, frc))
    FontFirstUse.isSettled(extent, frc) shouldBe true

    val underlined = unsettledFont()
    StyledFontCache.shared.underlineOf(underlined)
    FontFirstUse.isSettled(underlined, frc) shouldBe true

    val dropCap = unsettledFont()
    DropCapLayout.glyphWidthPx(dropCap, frc, "A")
    FontFirstUse.isSettled(dropCap, frc) shouldBe true

    val advanced = unsettledFont()
    GlyphAdvances.measure("Hoxi", 0, 4, 0, LineFontResolver(advanced, Vector.empty), frc)
    FontFirstUse.isSettled(advanced, frc) shouldBe true

    val measured = unsettledFont()
    GlyphAdvances.layoutAdvanceMatchesSum(measured, frc)
    FontFirstUse.isSettled(measured, frc) shouldBe true

    val caret = unsettledFont()
    TextCaretMeasurement.rawMeasuredCaretXs("To fi", 0, LineFontResolver(caret, Vector.empty), frc)
    FontFirstUse.isSettled(caret, frc) shouldBe true

    val glyphs = unsettledFont()
    Java2DTextMeasurer(frc).shape(
      "To",
      0,
      TextMeasurer.FontRun(0, 2, FontSpec.fromAwt(glyphs)),
      0.toByte
    )
    FontFirstUse.isSettled(glyphs, frc) shouldBe true
  }
