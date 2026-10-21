package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `FontFirstUse` makes a font's first use at a render context happen once, on one thread, and answers the settled
  * question without a lock afterwards.
  */
class FontFirstUseSpec extends AnyFlatSpec with Matchers:

  private val mono   = new Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val serif  = new Font(Font.SERIF, Font.PLAIN, 14)
  private val plain  = FontRenderContext(null, true, true)
  private val coarse = FontRenderContext(null, false, false)

  "FontFirstUse" should "know nothing about a font it has not been asked to settle" in {
    val firstUse = new FontFirstUse()

    (firstUse.isSettled(mono, plain), firstUse.isSettled(serif, coarse)) shouldBe (false, false)
  }

  it should "settle a font at one render context without settling it at another, or another font" in {
    val firstUse = new FontFirstUse()

    firstUse.settle(mono, plain, (_, _) => ())

    (firstUse.isSettled(mono, plain), firstUse.isSettled(mono, coarse), firstUse.isSettled(serif, plain)) shouldBe
      (true, false, false)
  }

  it should "run the first use once however often the font is settled" in {
    val firstUse = new FontFirstUse()
    val runs     = new AtomicInteger(0)

    (1 to 5).foreach(_ =>
      firstUse.settle(
        mono,
        plain,
        (_, _) =>
          val _ = runs.incrementAndGet()
      )
    )

    runs.get shouldBe 1
  }

  it should "run the first use once when many threads settle the same font together" in {
    val firstUse = new FontFirstUse()
    val runs     = new AtomicInteger(0)
    val start    = new CountDownLatch(1)
    val pool     = Executors.newFixedThreadPool(8)
    try
      val done = (1 to 8).map { _ =>
        pool.submit(
          new Runnable:
            def run(): Unit =
              start.await()
              firstUse.settle(
                mono,
                plain,
                (_, _) =>
                  val _ = runs.incrementAndGet()
              )
        )
      }
      start.countDown()
      done.foreach(_.get(10, TimeUnit.SECONDS))
    finally pool.shutdownNow()

    runs.get shouldBe 1
  }
