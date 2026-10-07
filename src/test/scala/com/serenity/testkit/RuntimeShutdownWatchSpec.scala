package com.serenity.testkit

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.annotation.tailrec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import RuntimeShutdownWatch.{Finding, Sample}

class RuntimeShutdownWatchSpec extends AnyFlatSpec with Matchers:

  private def workers(names: String*): Sample = Sample(names.toSet, Map.empty)

  "Comparing two samples" should "report a worker whose interrupt flag has just been set, with its stack" in {
    val flagged = Sample(Set("io-compute-0"), Map("io-compute-0" -> List("com.example.Culprit.run")))

    RuntimeShutdownWatch.findings(workers("io-compute-0"), flagged, Set.empty) shouldBe
      List(Finding.InterruptFlagSet("io-compute-0", List("com.example.Culprit.run")))
  }

  it should "report each interrupted worker once" in {
    val flagged = Sample(Set("io-compute-0"), Map("io-compute-0" -> Nil))

    RuntimeShutdownWatch.findings(flagged, flagged, Set("io-compute-0")) shouldBe Nil
  }

  it should "report the pool vanishing when its last worker is gone" in {
    RuntimeShutdownWatch.findings(workers("io-compute-0", "io-compute-1"), workers(), Set.empty) shouldBe
      List(Finding.PoolVanished(Set("io-compute-0", "io-compute-1")))
  }

  it should "stay silent while a replacement worker takes over from a thread that became a blocker" in {
    RuntimeShutdownWatch.findings(
      workers("io-compute-0", "io-compute-1"),
      workers("io-compute-1", "io-compute-2"),
      Set.empty
    ) shouldBe Nil
  }

  it should "stay silent before any worker has been seen" in {
    RuntimeShutdownWatch.findings(workers(), workers(), Set.empty) shouldBe Nil
  }

  "Rendering a finding" should "name the thread and the code it was running" in {
    val text = RuntimeShutdownWatch.render(Finding.InterruptFlagSet("io-compute-3", List("a.B.c", "d.E.f")))

    text should include("[RUNTIME-SHUTDOWN]")
    text should include("io-compute-3")
    text should include("at a.B.c")
    text should include("at d.E.f")
  }

  it should "say that the pool vanished and which workers it had" in {
    val text = RuntimeShutdownWatch.render(Finding.PoolVanished(Set("io-compute-1", "io-compute-0")))

    text should include("[RUNTIME-SHUTDOWN]")
    text should include("io-compute-0, io-compute-1")
  }

  "Sampling live threads" should "see a worker that interrupted itself and capture where it was" in {
    val flagged = CountDownLatch(1)
    val release = AtomicBoolean(false)
    val worker = Thread(
      () =>
        Thread.currentThread.interrupt()
        flagged.countDown()
        spinUntil(release)
      ,
      "watched-compute-0"
    )
    worker.setDaemon(true)
    worker.start()
    try
      flagged.await(10, TimeUnit.SECONDS) shouldBe true

      val sample = RuntimeShutdownWatch.liveSample("watched-compute")

      sample.workers should contain("watched-compute-0")
      sample.interrupted.get("watched-compute-0").map(_.nonEmpty) shouldBe Some(true)
    finally
      release.set(true)
      worker.join()
  }

  "The watcher" should "emit what it finds until it is told to stop" in {
    val samples  = AtomicReference(List(workers("io-compute-0"), workers("io-compute-0"), workers()))
    val emitted  = AtomicReference(List.empty[String])
    val vanished = CountDownLatch(1)

    val watcher = RuntimeShutdownWatch.start(
      sample = () => samples.getAndUpdate(_.drop(1)).headOption.getOrElse(workers()),
      emit = text =>
        emitted.updateAndGet(_ :+ text)
        vanished.countDown()
    )

    vanished.await(10, TimeUnit.SECONDS) shouldBe true
    watcher.stop()
    emitted.get.headOption.getOrElse("") should include("io-compute-0")
  }

  @tailrec
  private def spinUntil(done: AtomicBoolean): Unit =
    if !done.get then
      Thread.onSpinWait()
      spinUntil(done)

end RuntimeShutdownWatchSpec
