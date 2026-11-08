package com.serenity.host

import java.nio.file.Paths
import java.time.Instant

import scala.concurrent.duration.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RetentionPolicySpec extends AnyFlatSpec with Matchers:

  private val Gib    = 1024L * 1024L * 1024L
  private val limits = RetentionLimits(idle = 10.minutes, perHostBytes = 3 * Gib, totalBytes = 4 * Gib)
  private val t0     = Instant.parse("2026-11-03T09:00:00Z")

  private def key(name: String): HostKey = HostKey(Paths.get("/work").resolve(name))

  private def attached(name: String, bytes: Long = Gib): HostStatus =
    HostStatus(key(name), HostLink.Attached, bytes)

  private def detached(name: String, since: Instant, bytes: Long = Gib): HostStatus =
    HostStatus(key(name), HostLink.Detached(since), bytes)

  private def stops(decision: RetentionDecision): List[(HostKey, StopReason)] =
    decision.stops.map(order => (order.host, order.reason))

  "RetentionPolicy" should "never stop an attached host, however long it has run or however much it holds" in {
    val decision = RetentionPolicy.decide(limits, t0.plusSeconds(86400), List(attached("a", 9 * Gib)))

    decision.stops shouldBe Nil
  }

  it should "keep a detached host until its idle time is reached" in {
    val decision = RetentionPolicy.decide(limits, t0.plusSeconds(599), List(detached("a", t0)))

    decision.stops shouldBe Nil
  }

  it should "stop a detached host exactly when its idle time is reached" in {
    val decision = RetentionPolicy.decide(limits, t0.plusSeconds(600), List(detached("a", t0)))

    stops(decision) shouldBe List((key("a"), StopReason.IdleExpired))
  }

  it should "stop a host that slept past its expiry at the first check after waking" in {
    val decision = RetentionPolicy.decide(limits, t0.plusSeconds(8 * 3600), List(detached("a", t0)))

    stops(decision) shouldBe List((key("a"), StopReason.IdleExpired))
  }

  it should "stop a detached host at once when the idle time is zero" in {
    val decision = RetentionPolicy.decide(limits.copy(idle = Duration.Zero), t0, List(detached("a", t0)))

    stops(decision) shouldBe List((key("a"), StopReason.IdleExpired))
  }

  it should "stop a detached host over the per-host cap even before it is idle, and keep one exactly at the cap" in {
    val decision = RetentionPolicy.decide(
      limits,
      t0.plusSeconds(1),
      List(detached("big", t0, 3 * Gib + 1), detached("exact", t0, 3 * Gib))
    )

    stops(decision) shouldBe List((key("big"), StopReason.OverHostCap))
  }

  it should "only warn when an attached host is over the per-host cap" in {
    val decision = RetentionPolicy.decide(limits, t0, List(attached("a", 4 * Gib)))

    decision.stops shouldBe Nil
    decision.notices shouldBe List(RetentionNotice.AttachedOverHostCap(key("a"), 4 * Gib, 3 * Gib))
  }

  it should "stop the least recently detached host first when the total is over its cap" in {
    val hosts = List(
      detached("newest", t0.plusSeconds(120), 2 * Gib),
      detached("oldest", t0, 2 * Gib),
      detached("middle", t0.plusSeconds(60), 2 * Gib)
    )

    val decision = RetentionPolicy.decide(limits.copy(totalBytes = 3 * Gib), t0.plusSeconds(130), hosts)

    stops(decision) shouldBe List(
      (key("oldest"), StopReason.OverTotalCap),
      (key("middle"), StopReason.OverTotalCap)
    )
  }

  it should "stop only as many hosts as bring the total under the cap" in {
    val hosts = List(
      detached("a", t0, 2 * Gib),
      detached("b", t0.plusSeconds(1), 2 * Gib),
      detached("c", t0.plusSeconds(2), 1 * Gib)
    )

    val decision = RetentionPolicy.decide(limits, t0.plusSeconds(10), hosts)

    stops(decision) shouldBe List((key("a"), StopReason.OverTotalCap))
  }

  it should "count an attached host towards the total but never stop it" in {
    val hosts = List(attached("live", 3 * Gib), detached("old", t0, 2 * Gib))

    val decision = RetentionPolicy.decide(limits, t0.plusSeconds(10), hosts)

    stops(decision) shouldBe List((key("old"), StopReason.OverTotalCap))
    decision.notices shouldBe Nil
  }

  it should "warn about the machine total when only attached hosts remain over it" in {
    val hosts = List(attached("a", 3 * Gib), attached("b", 3 * Gib))

    val decision = RetentionPolicy.decide(limits, t0, hosts)

    decision.stops shouldBe Nil
    decision.notices shouldBe List(RetentionNotice.TotalOverCap(6 * Gib, 4 * Gib))
  }

  it should "leave a host already stopped for another reason out of the total" in {
    val hosts = List(
      detached("expired", t0.minusSeconds(3600), 3 * Gib),
      detached("fresh", t0, 2 * Gib)
    )

    val decision = RetentionPolicy.decide(limits, t0.plusSeconds(1), hosts)

    stops(decision) shouldBe List((key("expired"), StopReason.IdleExpired))
  }

  it should "stop nothing when no host is running" in {
    RetentionPolicy.decide(limits, t0, Nil) shouldBe RetentionDecision(Nil, Nil)
  }

  "RetentionLimits" should "default to a 10 minute idle time and 3 GiB per host" in {
    val defaults = RetentionLimits.defaults(physicalMemoryBytes = 64 * Gib)

    defaults.idle shouldBe 10.minutes
    defaults.perHostBytes shouldBe 3 * Gib
  }

  it should "cap the total at 4 GiB, or at a quarter of the memory when that is less" in {
    RetentionLimits.defaults(16 * Gib).totalBytes shouldBe 4 * Gib
    RetentionLimits.defaults(64 * Gib).totalBytes shouldBe 4 * Gib
    RetentionLimits.defaults(8 * Gib).totalBytes shouldBe 2 * Gib
  }
