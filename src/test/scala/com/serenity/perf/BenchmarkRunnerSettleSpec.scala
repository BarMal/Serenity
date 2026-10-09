package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BenchmarkRunnerSettleSpec extends AnyFlatSpec with Matchers:

  "isSteady" should "accept a window whose halves agree within the tolerance" in {
    BenchmarkRunner.isSteady(Vector.tabulate(20)(i => 0.80 + (i % 3) * 0.01)) shouldBe true
  }

  it should "reject a window that steps down part-way through, as when C2 code is installed" in {
    BenchmarkRunner.isSteady(Vector.fill(10)(1.08) ++ Vector.fill(10)(0.82)) shouldBe false
  }

  it should "ignore a single noisy round" in {
    BenchmarkRunner.isSteady(Vector.fill(20)(0.80).updated(7, 3.0)) shouldBe true
  }

  it should "reject a window shorter than the span that must agree" in {
    BenchmarkRunner.isSteady(Vector.fill(19)(0.80)) shouldBe false
  }
