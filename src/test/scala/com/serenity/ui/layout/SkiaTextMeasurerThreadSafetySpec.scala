package com.serenity.ui.layout

import java.util.concurrent.{Callable, CountDownLatch, Executors}

import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import com.serenity.ui.skia.SkiaTextMeasurer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A Skia font's first use, and a shaper's state, are not documented as safe across threads. Threads that all start
  * cold on the same fonts and text must measure what one thread does.
  */
class SkiaTextMeasurerThreadSafetySpec extends AnyFlatSpec with Matchers:

  private val threads = 4

  private def measure(measurer: SkiaTextMeasurer): Vector[Vector[Int]] =
    for
      pinned <- TextMeasureCorpus.fonts
      line   <- TextMeasureCorpus.lines
    yield
      val spec = FontSpec.fromAwt(pinned.font)
      val xs   = measurer.rawCaretXs(line.text, 0, LineFonts(spec, Vector.empty))
      val cell = measurer.cellMetrics(spec)
      xs.toVector.map(java.lang.Float.floatToRawIntBits) :+ cell.charWidth :+ cell.lineHeight :+ cell.ascent

  "Threads measuring from cold" should "get the numbers a single thread gets" in {
    val reference = measure(new SkiaTextMeasurer())
    val pool      = Executors.newFixedThreadPool(threads)
    try
      val shared = new SkiaTextMeasurer()
      val start  = new CountDownLatch(1)
      val runs = (1 to threads).map { _ =>
        pool.submit(
          new Callable[Vector[Vector[Int]]]:
            def call(): Vector[Vector[Int]] =
              start.await()
              measure(shared)
        )
      }
      start.countDown()
      runs.foreach(run => run.get() shouldBe reference)
    finally pool.shutdown()
  }
