package com.serenity.testkit

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, DurationLong}

import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import RuntimeShutdownWatch.{Finding, Sample}

class RuntimeShutdownWatchSpec extends AnyFlatSpec with Matchers:

  private def at(millis: Long, names: String*): Sample = Sample(millis.millis, names.toSet, Map.empty)

  private def workers(names: String*): Sample = at(0, names*)

  private def fold(samples: List[Sample]): List[Finding] =
    samples
      .foldLeft((RuntimeShutdownWatch.State.initial, List.empty[Finding])) {
        case ((state, found), sample) =>
          val (next, fresh) = RuntimeShutdownWatch.advance(state, sample)
          (next, found ::: fresh)
      }
      ._2

  "Advancing over samples" should "report a worker whose interrupt flag has just been set, with its stack" in {
    val flagged = Sample(Duration.Zero, Set("io-compute-0"), Map("io-compute-0" -> List("com.example.Culprit.run")))

    fold(List(workers("io-compute-0"), flagged)) shouldBe
      List(Finding.InterruptFlagSet("io-compute-0", List("com.example.Culprit.run")))
  }

  it should "report each interrupted worker once" in {
    val flagged = Sample(Duration.Zero, Set("io-compute-0"), Map("io-compute-0" -> Nil))

    fold(List(flagged, flagged, flagged)) shouldBe List(Finding.InterruptFlagSet("io-compute-0", Nil))
  }

  it should "report the pool vanishing once its last worker has been gone for the whole window" in {
    fold(List(at(0, "io-compute-0", "io-compute-1"), at(1), at(250), at(501))) shouldBe
      List(Finding.PoolVanished(Set("io-compute-0", "io-compute-1")))
  }

  it should "stay silent while the pool is empty for less than the window" in {
    fold(List(at(0, "io-compute-0"), at(1), at(250), at(500))) shouldBe Nil
  }

  it should "stay silent when workers return after a transient empty sample" in {
    fold(
      List(at(0, "io-compute-0", "io-compute-1"), at(1), at(2, "io-compute-2"), at(900, "io-compute-2"))
    ) shouldBe Nil
  }

  it should "measure the window from the start of the latest emptiness, not from an earlier one" in {
    fold(
      List(at(0, "io-compute-0"), at(1), at(2, "io-compute-1"), at(600, "io-compute-1"), at(601), at(700))
    ) shouldBe Nil
  }

  it should "report a sustained emptiness only once" in {
    fold(List(at(0, "io-compute-0"), at(1), at(600), at(700), at(5000))) shouldBe
      List(Finding.PoolVanished(Set("io-compute-0")))
  }

  it should "report a second vanish after the pool came back" in {
    fold(
      List(at(0, "io-compute-0"), at(1), at(600), at(700, "io-compute-1"), at(701), at(1300))
    ) shouldBe List(Finding.PoolVanished(Set("io-compute-0")), Finding.PoolVanished(Set("io-compute-1")))
  }

  it should "stay silent while a replacement worker takes over from a thread that became a blocker" in {
    fold(List(workers("io-compute-0", "io-compute-1"), workers("io-compute-1", "io-compute-2"))) shouldBe Nil
  }

  it should "stay silent before any worker has been seen" in {
    fold(List(at(0), at(1000), at(5000))) shouldBe Nil
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

  "Sampling the global pool" should "not see an interrupted thread of another runtime" in {
    val flagged = CountDownLatch(1)
    val release = AtomicBoolean(false)
    val worker = Thread(
      () =>
        Thread.currentThread.interrupt()
        flagged.countDown()
        spinUntil(release)
      ,
      "owned-compute-0"
    )
    worker.setDaemon(true)
    worker.start()
    try
      flagged.await(10, TimeUnit.SECONDS) shouldBe true

      val sample = RuntimeShutdownWatch.liveSample()

      sample.workers should not contain "owned-compute-0"
      sample.interrupted should not contain key("owned-compute-0")
    finally
      release.set(true)
      worker.join()
  }

  it should "not see the threads of a runtime built by the test kit, and the shutdown of that runtime is silent" in {
    val runtime = OwnedRuntime.build()
    val names =
      try (1 to 8).map(_ => IO(Thread.currentThread.getName).unsafeRunSync()(using runtime)).toSet
      finally runtime.shutdown()

    names.foreach(_ should not startWith "io-compute")
    RuntimeShutdownWatch.liveSample().workers.intersect(names) shouldBe empty
  }

  "The watcher" should "emit what it finds until it is told to stop" in {
    val samples  = AtomicReference(List(workers("io-compute-0"), workers("io-compute-0")))
    def now      = System.nanoTime.nanos
    val emitted  = AtomicReference(List.empty[String])
    val vanished = CountDownLatch(1)

    val watcher = RuntimeShutdownWatch.start(
      sample = () => samples.getAndUpdate(_.drop(1)).headOption.getOrElse(Sample(now, Set.empty, Map.empty)),
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
