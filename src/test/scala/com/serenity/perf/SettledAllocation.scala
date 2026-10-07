package com.serenity.perf

import java.lang.management.ManagementFactory

/** Bytes the calling thread allocates per call of two workloads once the JIT has stopped changing what they allocate;
  * `None` on a JVM that cannot count per-thread allocation.
  *
  * Interpreted and C1 code allocates every tuple and `Option` that C2 later removes by escape analysis, and which tier
  * a method is in depends on when the compiler thread got to it. A fixed warm-up therefore compares one workload in one
  * tier with another in a different tier, and a "grows with size" bound fails or passes on that alone. Both workloads
  * run together, so they share compiled code and profile, and are measured only once consecutive rounds agree exactly.
  */
object SettledAllocation:

  private val WarmUpCallsPerRound = 5000
  private val RoundsThatMustAgree = 3
  private val MaxRounds           = 60

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  def perCall(small: () => Any, large: () => Any): Option[(Long, Long)] =
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

      @annotation.tailrec
      def settle(previous: (Long, Long), agreeing: Int, roundsLeft: Int): (Long, Long) =
        val current = round()
        val streak  = if current == previous then agreeing + 1 else 1
        if streak >= RoundsThatMustAgree || roundsLeft <= 1 then current
        else settle(current, streak, roundsLeft - 1)

      settle((-1L, -1L), 0, MaxRounds)
    }

end SettledAllocation
