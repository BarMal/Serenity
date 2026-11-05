package com.serenity.diagnostics

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EdtLagSpec extends AnyFlatSpec with Matchers:

  private val Ms = 1_000_000L

  "EdtLagTrace" should "report the p50, p95 and max of what it recorded, in milliseconds, and start afresh" in {
    val trace = EdtLagTrace()
    trace.setEnabled(true)
    (1 to 20).foreach(i => trace.record(i * Ms))

    val stats = trace.drain()
    (stats.count, stats.p50Ms, stats.p95Ms, stats.maxMs) shouldBe ((20, 10.0, 19.0, 20.0))
    trace.drain().count shouldBe 0
  }

  it should "record nothing while it is off, and forget what it held when it is switched" in {
    val trace = EdtLagTrace()
    trace.record(5 * Ms)
    trace.drain().count shouldBe 0

    trace.setEnabled(true)
    trace.record(5 * Ms)
    trace.setEnabled(false)
    trace.drain().count shouldBe 0
  }

  it should "keep only the latest samples, so a window nobody drains cannot grow without bound" in {
    val trace = EdtLagTrace()
    trace.setEnabled(true)
    (1 to EdtLagTrace.MaxSamples + 500).foreach(_ => trace.record(Ms))
    trace.drain().count shouldBe EdtLagTrace.MaxSamples
  }

  "EdtLagProbe.sample" should "record how long the posted task waited before the event thread ran it" in {
    val now     = AtomicLong(10 * Ms)
    val pending = AtomicReference[Option[Runnable]](None)
    val trace   = EdtLagTrace()
    trace.setEnabled(true)

    EdtLagProbe.sample(task => pending.set(Some(task)), trace, () => now.get())
    trace.drain().count shouldBe 0

    now.set(47 * Ms)
    pending.get().foreach(_.run())
    val stats = trace.drain()
    (stats.count, stats.maxMs) shouldBe ((1, 37.0))
  }
