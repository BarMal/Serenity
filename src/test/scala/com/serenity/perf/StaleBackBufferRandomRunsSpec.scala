package com.serenity.perf

import scala.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The random runs of [[StaleBackBufferFrameSpec]]'s scenario, a few seeds to a suite so that the suites run in
  * parallel instead of one after another (#2557). Each seed and scale is the same test it was in the one suite.
  */
abstract class StaleBackBufferRandomRunsSpec(seeds: Range)
    extends AnyFlatSpec
    with Matchers
    with StaleBackBufferScenarios:

  for
    scale <- scales
    seed  <- seeds
  do
    s"A random run of edits at seed $seed and ${scale}x" should "draw what a cold frame draws at every step" in {
      val random  = Random(seed)
      val session = Session(prose, scale)
      session.markModified()
      (1 to 40).foreach { round =>
        session.placeCursor(0, 20 + random.nextInt(260))
        (1 to 8).foreach { _ =>
          val (event, label) = randomEdit(random)
          session(event, s"round $round: $label")
        }
      }
      session.mismatches shouldBe empty
    }

final class StaleBackBufferSeeds1To2Spec extends StaleBackBufferRandomRunsSpec(1 to 2)

final class StaleBackBufferSeeds3To4Spec extends StaleBackBufferRandomRunsSpec(3 to 4)

final class StaleBackBufferSeeds5To6Spec extends StaleBackBufferRandomRunsSpec(5 to 6)
