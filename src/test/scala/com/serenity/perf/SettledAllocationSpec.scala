package com.serenity.perf

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.perf.SettledAllocation.Settling
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The settling rule, over rounds that the spec scripts: the real JIT cannot be made to hold a plateau on cue (see
  * RopeEqualityAllocationSpec, which failed on Windows at a steady 28800 B). Each scripted round takes ten seconds.
  */
class SettledAllocationSpec extends AnyFlatSpec with Matchers:

  private val Plateau = (28800L, 28800L)
  private val Clean   = (0L, 0L)

  private val underTheBound: ((Long, Long)) => Boolean = _._2 <= 256L

  final private class Script(allocationOfRound: Int => (Long, Long)):
    private val rounds = new AtomicInteger(0)
    private val pauses = new AtomicInteger(0)

    def round(): (Long, Long) = allocationOfRound(rounds.incrementAndGet())

    def pause(): Unit = pauses.incrementAndGet()

    def elapsedNanos(): Long = rounds.get * 10_000_000_000L

    def roundsRun: Int = rounds.get

    def pausesTaken: Int = pauses.get

  private def settle(script: Script, acceptable: ((Long, Long)) => Boolean = underTheBound, maxRounds: Int = 60) =
    SettledAllocation.settle(
      script.round,
      acceptable,
      script.pause,
      script.elapsedNanos,
      extensionNanos = 60_000_000_000L,
      maxRounds = maxRounds
    )

  "Settling" should "settle at once on three agreeing rounds under the bound, without pausing" in {
    val script = Script(_ => Clean)

    settle(script) shouldBe Settling.Settled(0L, 0L)
    script.roundsRun shouldBe 3
    script.pausesTaken shouldBe 0
  }

  it should "keep measuring a plateau over the bound and settle where it ends" in {
    val script = Script(round => if round <= 6 then Plateau else Clean)

    settle(script) shouldBe Settling.Settled(0L, 0L)
    script.roundsRun shouldBe 9
    script.pausesTaken should be > 0
  }

  it should "not pass on one low round among rounds over the bound" in {
    val script = Script(round => if round == 5 then Clean else Plateau)

    settle(script) shouldBe Settling.Unsettled(28800L, 28800L, 9, agreedAboveBound = true)
  }

  it should "report a plateau that stays over the bound as not settled under it, after the deadline" in {
    val script = Script(_ => Plateau)

    settle(script) shouldBe Settling.Unsettled(28800L, 28800L, 9, agreedAboveBound = true)
    script.roundsRun shouldBe 9
  }

  it should "accept any agreeing rounds when the caller sets no bound" in {
    val script = Script(_ => Plateau)

    settle(script, acceptable = _ => true) shouldBe Settling.Settled(28800L, 28800L)
    script.roundsRun shouldBe 3
  }

  it should "report rounds that never agree as unsettled" in {
    val script = Script(round => (round.toLong, 0L))

    settle(script, maxRounds = 8) shouldBe Settling.Unsettled(8L, 0L, 8, agreedAboveBound = false)
  }

  "An unsettled result" should "say that the allocation did not settle under the bound, with the last figures" in {
    Settling.Unsettled(552L, 28800L, 9, agreedAboveBound = true).message shouldBe
      "allocation did not settle under the bound in 9 rounds (last round: 552 B and 28800 B)"
  }

  it should "say that the allocation did not settle when the rounds never agreed" in {
    Settling.Unsettled(552L, 28800L, 60, agreedAboveBound = false).message shouldBe
      "allocation did not settle in 60 rounds (last round: 552 B and 28800 B)"
  }
