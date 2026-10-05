package com.serenity.text

import java.lang.management.ManagementFactory

/** Bytes the current thread allocates running `work` once, measured after a warm-up so the sample reflects JIT-compiled
  * code; `None` on a JVM that cannot count per-thread allocation.
  */
object AllocationProbe:

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  def allocatedBytes(work: => Any): Option[Long] =
    allocationBean.map { bean =>
      val threadId = Thread.currentThread().threadId()
      (1 to 200).foreach(_ => work)
      val before = bean.getThreadAllocatedBytes(threadId)
      work
      bean.getThreadAllocatedBytes(threadId) - before
    }
