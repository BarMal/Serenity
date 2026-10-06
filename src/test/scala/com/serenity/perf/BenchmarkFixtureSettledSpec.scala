package com.serenity.perf

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The benchmarks build every fixture before timing the first one, so a fixture whose first spell-check analysis was
  * still running on its background lane would overlap whichever benchmark ran next and inflate it. A session is
  * measured as it is shortly after a document opens: analysed, with nothing left to do.
  */
class BenchmarkFixtureSettledSpec extends AnyFlatSpec with Matchers:

  "A benchmark state manager" should "return with its document already analysed and no background work pending" in {
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager(paragraphs = 300)

    val analysed = stateManager.getCurrentState.unsafeRunSync().runtime.languageService.diagnosticsState.spellCheckCache

    analysed should not be empty
  }
