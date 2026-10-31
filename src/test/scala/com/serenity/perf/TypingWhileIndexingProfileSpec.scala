package com.serenity.perf

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypingWhileIndexingProfileSpec extends AnyFlatSpec with Matchers:
  import TypingWhileIndexingProfile.*

  "the block order" should "be idle, loaded, loaded, idle, so drift falls on both alike" in {
    BlockOrder shouldBe List(Mode.Idle, Mode.Loaded, Mode.Loaded, Mode.Idle)
  }

  "Summary.of" should "use the benchmark runner's nearest-rank percentiles" in {
    val summary = Summary.of(Vector(400.0, 100.0, 300.0, 200.0))

    summary.count shouldBe 4
    summary.p50Ms shouldBe 200.0
    summary.p95Ms shouldBe 400.0
    summary.maxMs shouldBe 400.0
  }

  it should "be all zeros for no samples" in {
    Summary.of(Vector.empty) shouldBe Summary(0, 0.0, 0.0, 0.0)
  }

  "Ratios.of" should "divide loaded by idle, percentile by percentile" in {
    val idle   = Summary(10, 2.0, 4.0, 9.0)
    val loaded = Summary(10, 3.0, 12.0, 30.0)

    Ratios.of(idle, loaded) shouldBe Some(Ratios(p50 = 1.5, p95 = 3.0))
  }

  it should "give no ratio when the idle time is zero" in {
    Ratios.of(Summary(0, 0.0, 0.0, 0.0), Summary(10, 3.0, 12.0, 30.0)) shouldBe None
  }

  "the report" should "state every figure and the load that was present, as RESULT lines" in {
    val result  = Outcome(Summary(10, 2.0, 4.0, 9.0), Summary(10, 3.0, 12.0, 30.0), walksCompleted = 7)
    val options = Options(keys = 300, paceMs = 100, warmupKeys = 200, shape = TreeShape.Small)

    val text = TypingWhileIndexingProfile.report(result, options).mkString("\n")

    text should include("RESULT typing.idle count=10 p50_ms=2.000 p95_ms=4.000 max_ms=9.000")
    text should include("RESULT typing.loaded count=10 p50_ms=3.000 p95_ms=12.000 max_ms=30.000")
    text should include("RESULT typing.loaded_over_idle p50_ratio=1.50 p95_ratio=3.00")
    text should include("walks_completed=7")
    text should include("keys=300")
    text should include("pace_ms=100")
  }

  "a short run" should "type as many keys idle as loaded, and complete at least one walk while typing" in {
    val result = TypingWhileIndexingProfile.run(Options(keys = 20, paceMs = 2, warmupKeys = 5, shape = TreeShape.Small))

    result.idle.count shouldBe 40
    result.loaded.count shouldBe 40
    result.walksCompleted should be >= 1
  }
