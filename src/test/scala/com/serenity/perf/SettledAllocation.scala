package com.serenity.perf

import java.lang.management.ManagementFactory
import java.util.concurrent.locks.LockSupport

import org.scalatest.Assertions

/** Bytes the calling thread allocates per call of two workloads once the JIT has stopped changing what they allocate;
  * `None` on a JVM that cannot count per-thread allocation, and a failed spec when the allocation does not settle.
  *
  * Interpreted and C1 code allocates every tuple and `Option` that C2 later removes by escape analysis, and which tier
  * a method is in depends on when the compiler thread got to it. A fixed warm-up therefore compares one workload in one
  * tier with another in a different tier, and a "grows with size" bound fails or passes on that alone. Both workloads
  * run together, so they share compiled code and profile, and are measured only once consecutive rounds agree exactly.
  *
  * Agreement alone can be a plateau: a method held in C1 allocates the same amount round after round until C2 replaces
  * it, and RopeEqualityAllocationSpec failed on a Windows runner at a steady 28800 B. A caller that has a bound passes
  * it as `acceptable`; agreeing rounds over the bound are not a result, so the helper pauses between rounds (the
  * compiler threads get the CPU) and keeps measuring for a minute. A result is still three consecutive rounds that
  * agree and are acceptable, never one low round, and if none comes the spec fails saying it did not settle under the
  * bound, with the last figures. A bound that is really exceeded therefore still fails, a minute later.
  */
object SettledAllocation:

  private val WarmUpCallsPerRound = 5000
  private val RoundsThatMustAgree = 3
  private val MaxRounds           = 60
  private val RoundPauseNanos     = 100_000_000L
  private val ExtensionNanos      = 60_000_000_000L

  enum Settling:
    case Settled(small: Long, large: Long)
    case Unsettled(lastSmall: Long, lastLarge: Long, rounds: Int, agreedAboveBound: Boolean)

    def message: String = this match
      case Unsettled(small, large, rounds, true) =>
        s"allocation did not settle under the bound in $rounds rounds (last round: $small B and $large B)"
      case Unsettled(small, large, rounds, false) =>
        s"allocation did not settle in $rounds rounds (last round: $small B and $large B)"
      case Settled(small, large) => s"settled at $small B and $large B"

  private[perf] def settle(
    round: () => (Long, Long),
    acceptable: ((Long, Long)) => Boolean,
    pause: () => Unit,
    elapsedNanos: () => Long,
    extensionNanos: Long = ExtensionNanos,
    roundsThatMustAgree: Int = RoundsThatMustAgree,
    maxRounds: Int = MaxRounds
  ): Settling =
    @annotation.tailrec
    def next(
      previous: (Long, Long),
      agreeing: Int,
      ran: Int,
      roundsLeft: Int,
      aboveBoundSince: Option[Long]
    ): Settling =
      val current   = round()
      val streak    = if current == previous then agreeing + 1 else 1
      val agreed    = streak >= roundsThatMustAgree
      val now       = elapsedNanos()
      val sinceThen = aboveBoundSince.orElse(Option.when(agreed)(now))
      if agreed && acceptable(current) then Settling.Settled(current._1, current._2)
      else if sinceThen.exists(now - _ >= extensionNanos) then
        Settling.Unsettled(current._1, current._2, ran + 1, agreedAboveBound = true)
      else if sinceThen.isEmpty && roundsLeft <= 1 then
        Settling.Unsettled(current._1, current._2, ran + 1, agreedAboveBound = false)
      else
        if sinceThen.isDefined then pause()
        next(current, streak, ran + 1, if sinceThen.isDefined then roundsLeft else roundsLeft - 1, sinceThen)
    next((-1L, -1L), 0, 0, maxRounds, None)

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  def perCall(
    small: () => Any,
    large: () => Any,
    acceptable: ((Long, Long)) => Boolean = _ => true
  ): Option[(Long, Long)] =
    allocationBean.map { bean =>
      val threadId = Thread.currentThread().threadId()

      def allocatedByOneCall(work: () => Any): Long =
        val start = bean.getThreadAllocatedBytes(threadId)
        work()
        bean.getThreadAllocatedBytes(threadId) - start

      def smallestOfFive(work: () => Any): Long = (1 to 5).map(_ => allocatedByOneCall(work)).min

      def round(): (Long, Long) =
        (1 to WarmUpCallsPerRound).foreach { _ =>
          small()
          large()
        }
        (smallestOfFive(small), smallestOfFive(large))

      val started = System.nanoTime()
      settle(
        round,
        acceptable,
        () => LockSupport.parkNanos(RoundPauseNanos),
        () => System.nanoTime() - started
      ) match
        case Settling.Settled(small, large) => (small, large)
        case unsettled: Settling.Unsettled  => Assertions.fail(unsettled.message)
    }

end SettledAllocation
