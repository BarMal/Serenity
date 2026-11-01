package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The warm-up extension keeps a benchmark running until the JIT has finished with it, within a budget: the policy is
  * pure, so these specs pass rounds in rather than running anything.
  */
class WarmupExtensionSpec extends AnyFlatSpec with Matchers:

  import WarmupExtension.*

  private val settings = Settings.default

  private def flat(rounds: Int, perCall: Double = 1.0): Vector[Double] = Vector.fill(rounds)(perCall)

  "The default settings" should "be the ones the study measured" in {
    settings shouldBe Settings(
      roundNanos = 100_000_000L,
      flatRounds = 5,
      flatTolerance = 1.10,
      minInvocations = 10_000,
      windowRounds = 40,
      windowTolerance = 1.20,
      rowCapNanos = 3_000_000_000L,
      runCapNanos = 90_000_000_000L
    )
  }

  they should "be overridable from system properties, one value at a time" in {
    val props = Map("serenity.bench.warmup.rowCapSeconds" -> "1.5", "serenity.bench.warmup.runCapSeconds" -> "20")
    val changed = Settings.fromProperties(props.get)
    changed.rowCapNanos shouldBe 1_500_000_000L
    changed.runCapNanos shouldBe 20_000_000_000L
    changed.copy(rowCapNanos = settings.rowCapNanos, runCapNanos = settings.runCapNanos) shouldBe settings
  }

  "decide" should "stop after five flat rounds once the row has done 10,000 invocations" in {
    decide(settings, flat(5), invocations = 10_000, rowSpentNanos = 0L, runSpentNanos = 0L) shouldBe Verdict.Stop
  }

  it should "go on after five flat rounds when the row has done fewer than 10,000 invocations" in {
    decide(settings, flat(5), invocations = 9_999, rowSpentNanos = 0L, runSpentNanos = 0L) shouldBe Verdict.Continue
  }

  it should "go on when the first five rounds are not flat, however many invocations" in {
    val ramping = Vector(2.0, 1.8, 1.5, 1.3, 1.2)
    decide(settings, ramping, invocations = 50_000, rowSpentNanos = 0L, runSpentNanos = 0L) shouldBe Verdict.Continue
  }

  it should "go on until 40 rounds have been seen" in {
    decide(settings, flat(39), invocations = 100, rowSpentNanos = 0L, runSpentNanos = 0L) shouldBe Verdict.Continue
  }

  it should "stop when the last 40 rounds agree within 20%" in {
    val settled = Vector.fill(10)(3.0) ++ flat(40).zipWithIndex.map((v, i) => v * (1.0 + (i % 3) * 0.02))
    decide(settings, settled, invocations = 100, rowSpentNanos = 0L, runSpentNanos = 0L) shouldBe Verdict.Stop
  }

  it should "go on while the two halves of the last 40 rounds differ by more than 20%" in {
    val stepping = Vector.fill(20)(1.5) ++ Vector.fill(20)(1.0)
    decide(settings, stepping, invocations = 100, rowSpentNanos = 0L, runSpentNanos = 0L) shouldBe Verdict.Continue
  }

  it should "stop at the row cap whatever the rounds say" in {
    val ramping = Vector.tabulate(40)(i => 5.0 - i * 0.1)
    decide(settings, ramping, 100, settings.rowCapNanos, 0L) shouldBe Verdict.Stop
    decide(settings, ramping, 100, settings.rowCapNanos - 1L, 0L) shouldBe Verdict.Continue
  }

  it should "stop once the run budget is spent" in {
    val ramping = Vector.tabulate(40)(i => 5.0 - i * 0.1)
    decide(settings, ramping, 100, 0L, settings.runCapNanos) shouldBe Verdict.Stop
  }

  "A budget" should "be spent across rows and report when nothing is left" in {
    val budget = Budget(settings.runCapNanos)
    budget.spentNanos shouldBe 0L
    budget.charge(30_000_000_000L)
    budget.charge(60_000_000_000L)
    budget.spentNanos shouldBe 90_000_000_000L
    budget.exhausted shouldBe true
  }

  "extend" should "not run a row at all once the budget is spent" in {
    val budget = Budget(1_000L)
    budget.charge(1_000L)
    val calls = new java.util.concurrent.atomic.AtomicInteger
    val _     = extend(settings, budget, () => calls.incrementAndGet(), invocations = 0)
    calls.get shouldBe 0
  }

  it should "keep a ramping row running until its per-call cost has settled" in {
    val quick  = settings.copy(roundNanos = 2_000_000L, runCapNanos = 10_000_000_000L, rowCapNanos = 5_000_000_000L)
    val calls  = new java.util.concurrent.atomic.AtomicInteger
    val budget = Budget(quick.runCapNanos)
    val _      = extend(quick, budget, () => spin(costNanos(calls.incrementAndGet())), invocations = 0)
    val settled = (1 to 20).map(_ => timeOf(() => spin(costNanos(calls.incrementAndGet())))).sorted.apply(10)
    settled should be <= (PlateauNanos * 1.25).toLong
    budget.spentNanos should be > 0L
  }

  it should "leave a row that is already flat after five rounds when it has done 10,000 invocations" in {
    val quick  = settings.copy(roundNanos = 2_000_000L)
    val budget = Budget(quick.runCapNanos)
    val _      = extend(quick, budget, () => spin(PlateauNanos), invocations = 10_000)
    budget.spentNanos should be < (7 * quick.roundNanos)
  }

  private val PlateauNanos = 20_000L

  /** A call that starts ten times as slow as it ends and takes 400 calls to get there, like code the JIT has not got to. */
  private def costNanos(call: Int): Long = PlateauNanos + math.max(0L, (9L * PlateauNanos) - call * 450L)

  private def spin(nanos: Long): Unit =
    val deadline = System.nanoTime() + nanos
    while System.nanoTime() < deadline do Thread.onSpinWait()

  private def timeOf(run: () => Unit): Long =
    val started = System.nanoTime()
    run()
    System.nanoTime() - started
