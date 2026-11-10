package com.serenity.ui.layout

import java.lang.management.ManagementFactory

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `uncoveredWithin` runs for every frame that keeps rows, so the memory it asks for is part of a typing frame's cost.
  */
class PixelRectAllocationSpec extends AnyFlatSpec with Matchers:

  private val bean = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]

  private def allocatedBy(work: => Unit): Long =
    val before = bean.getThreadAllocatedBytes(Thread.currentThread.threadId)
    work
    bean.getThreadAllocatedBytes(Thread.currentThread.threadId) - before

  "Working out what a frame's kept rows leave" should "ask for no more than 24 KB for forty unjoined kept rows, where sorting and merging their edges as boxed values took 32 KB" in {
    val holes  = (0 until 40).filter(_ % 7 != 3).map(row => PixelRect(0, row * 15, 600, 15)).toList
    val bounds = PixelRect(-(1 << 29), -(1 << 29), 1 << 30, 1 << 30)
    (1 to 2000).foreach(_ => PixelRect.uncoveredWithin(bounds, holes))
    val calls = 500
    val bytes = allocatedBy((1 to calls).foreach(_ => PixelRect.uncoveredWithin(bounds, holes)))
    (bytes / calls) should be < 24000L
  }
