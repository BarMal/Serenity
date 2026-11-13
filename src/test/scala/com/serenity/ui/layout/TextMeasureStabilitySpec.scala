package com.serenity.ui.layout

import java.util.concurrent.{Callable, Executors}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** AWT's first use of a font is not safe across threads (`FontFirstUse`), so a baseline taken once could be one of
  * several. Measuring again in the same JVM, and from two threads at once, must give the numbers the first time did.
  */
class TextMeasureStabilitySpec extends AnyFlatSpec with Matchers:

  "Measuring twice in one JVM" should "give the same numbers" in {
    val first = TextMeasureBaseline.entries()

    TextMeasureBaseline.changes(first, TextMeasureBaseline.entries()) shouldBe Nil
  }

  "Measuring from two threads at once" should "give the same numbers as one thread" in {
    val reference = TextMeasureBaseline.entries()
    val pool      = Executors.newFixedThreadPool(2)
    try
      val runs = (1 to 2).map(_ =>
        pool.submit(
          new Callable[Vector[TextMeasureBaseline.Entry]]:
            def call(): Vector[TextMeasureBaseline.Entry] = TextMeasureBaseline.entries()
        )
      )
      runs.foreach(run => TextMeasureBaseline.changes(reference, run.get()) shouldBe Nil)
    finally pool.shutdown()
  }
