package com.serenity.testkit

import java.nio.file.Paths
import java.time.Instant
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{CopyOnWriteArrayList, CountDownLatch, TimeUnit}

import scala.concurrent.duration.{DurationInt, DurationLong}
import scala.jdk.CollectionConverters.ListHasAsScala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import StallSampler.Stall

class StallSamplerSpec extends AnyFlatSpec with Matchers:

  "Lateness" should "be the time a sleep took beyond its interval" in {
    StallSampler.lateness(100.millis, 1_000L, 1_000L + 1_350.millis.toNanos) shouldBe 1_250.millis
  }

  it should "be zero for a sleep that ended early" in {
    StallSampler.lateness(100.millis, 0L, 90.millis.toNanos) shouldBe 0.millis
  }

  "A stall line" should "carry the wake time, the JVM uptime and the lateness" in {
    StallSampler.render(Stall(Instant.parse("2026-10-10T03:21:11.123Z"), 61_500.millis, 1_234.millis)) shouldBe
      "2026-10-10T03:21:11.123Z uptime=61500ms late=1234ms"
  }

  "The log location" should "come from the property and be absent when it is unset or blank" in {
    StallSampler.pathFrom(Some("jvm-diag/stalls.log")) shouldBe Some(Paths.get("jvm-diag/stalls.log"))
    StallSampler.pathFrom(Some("  ")) shouldBe None
    StallSampler.pathFrom(None) shouldBe None
  }

  "Sampling" should "record only the wakes later than the threshold" in {
    val clock    = AtomicLong(0L)
    val delays   = List(0L, 100L, 120L, 500L, 0L).iterator
    val recorded = CopyOnWriteArrayList[Stall]()
    val finished = CountDownLatch(1)
    def sleep(millis: Long): Unit =
      if delays.hasNext then clock.addAndGet((millis + delays.next()).millis.toNanos)
      else
        finished.countDown()
        Thread.sleep(1)
    val sampler = StallSampler.start(
      100.millis,
      250.millis,
      () => clock.get,
      sleep,
      stall =>
        val _ = recorded.add(stall)
    )

    finished.await(10, TimeUnit.SECONDS) shouldBe true
    sampler.stop()

    recorded.asScala.toList.map(_.lateness) shouldBe List(500.millis)
  }
