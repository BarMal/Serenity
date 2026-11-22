package com.serenity.perf

import com.serenity.diagnostics.FrameTimings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The present's hooks against the app's key-latency trace (#2380). A paint shows the frames published before it
  * started, so the frame must be published before its paint starts: the benchmark's two threads must not race to it.
  */
class SdlPresentWatchSpec extends AnyFlatSpec with Matchers:

  private def tracedKey(timings: FrameTimings): Unit =
    val trace = timings.keyLatency
    trace.setEnabled(true)
    trace.keyReceived(System.currentTimeMillis())
    trace.keyEnqueued()
    trace.keysDequeued(1)
    trace.dispatchStarted()
    trace.keysApplied(1)
    trace.damageEmitted()
    trace.frameWoke()
    trace.frameModelRead()

  "A present" should "show the key of the frame it published, once the swap returns" in {
    val timings = FrameTimings()
    tracedKey(timings)
    val watch = SdlPresentWatch(System.nanoTime, () => (), Some(timings))

    watch.beginPresent()
    watch.afterSwap()
    timings.keyLatency.frameEnded()

    timings.keyLatency.drain().keys should have size 1
  }
