package com.serenity.testkit

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.events.{Event, Ordinal, TestFailed, TestStarting, TestSucceeded}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Args, DoNotDiscover, Reporter}

class TestHangBoundSpec extends AnyFlatSpec with Matchers:

  private val bound = 5.minutes
  private val grace = 1.minute

  /** A reporter on a clock the spec moves by hand; everything it does is recorded. */
  final private class Rig(boundFor: Option[String] => FiniteDuration = _ => bound):
    val nanos       = AtomicLong(0L)
    val pausedMs    = AtomicLong(0L)
    val interrupted = ConcurrentLinkedQueue[String]()
    val emitted     = ConcurrentLinkedQueue[String]()
    val aborted     = ConcurrentLinkedQueue[String]()

    val reporter = TestHangBoundReporter(
      boundFor,
      () => nanos.get,
      () => pausedMs.get,
      thread => interrupted.add(thread),
      alert => s"REPORT $alert",
      emitted.add(_),
      aborted.add(_),
      grace,
      None
    )

    def advance(by: FiniteDuration): Unit = nanos.addAndGet(by.toNanos)

    def starts(test: String, suite: String = "SomeSpec", thread: String = "ScalaTest-running-SomeSpec"): Unit =
      reporter(TestStarting(Ordinal(0), suite, suite, Some(s"com.serenity.$suite"), test, test, threadName = thread))

    def succeeds(test: String, suite: String = "SomeSpec"): Unit =
      reporter(TestSucceeded(Ordinal(0), suite, suite, Some(s"com.serenity.$suite"), test, test, Vector.empty))

  "A test inside its bound" should "be left alone" in {
    val rig = Rig()
    rig.starts("finishes in time")
    rig.advance(bound - 1.second)
    rig.reporter.check()
    rig.interrupted shouldBe empty
    rig.emitted shouldBe empty
  }

  "A test past its bound" should "be interrupted on its own thread, with the test named in the report" in {
    val rig = Rig()
    rig.starts("never finishes", suite = "HangingSpec", thread = "ScalaTest-running-HangingSpec")
    rig.advance(bound + 1.second)
    rig.reporter.check()
    rig.interrupted.asScala.toList shouldBe List("ScalaTest-running-HangingSpec")
    val report = rig.emitted.asScala.mkString("\n")
    report should include("HangingSpec")
    report should include("never finishes")
    report should include("REPORT")
  }

  it should "be interrupted once, not again on every check" in {
    val rig = Rig()
    rig.starts("never finishes")
    rig.advance(bound + 1.second)
    rig.reporter.check()
    rig.advance(10.seconds)
    rig.reporter.check()
    rig.interrupted.size shouldBe 1
    rig.emitted.size shouldBe 1
  }

  it should "not be interrupted once it has finished" in {
    val rig = Rig()
    rig.starts("slow but finishes")
    rig.advance(bound - 1.second)
    rig.succeeds("slow but finishes")
    rig.advance(1.hour)
    rig.reporter.check()
    rig.interrupted shouldBe empty
  }

  it should "count only the time the JVM was running, so a collector pause cannot fail it" in {
    val rig = Rig()
    rig.starts("paused by the collector")
    rig.advance(bound + 1.minute)
    rig.pausedMs.set(2.minutes.toMillis)
    rig.reporter.check()
    rig.interrupted shouldBe empty
    rig.advance(2.minutes)
    rig.reporter.check()
    rig.interrupted.size shouldBe 1
  }

  it should "use the bound its suite is registered with" in {
    val rig = Rig(suite => if suite.contains("com.serenity.LongSpec") then 15.minutes else bound)
    rig.starts("long", suite = "LongSpec")
    rig.starts("ordinary", suite = "OtherSpec", thread = "ScalaTest-running-OtherSpec")
    rig.advance(bound + 1.second)
    rig.reporter.check()
    rig.interrupted.asScala.toList shouldBe List("ScalaTest-running-OtherSpec")
  }

  "A test that ignores the interrupt" should "abort the run naming it once the grace has passed" in {
    val rig = Rig()
    rig.starts("uninterruptible", suite = "StuckSpec")
    rig.advance(bound + 1.second)
    rig.reporter.check()
    rig.advance(grace - 1.second)
    rig.reporter.check()
    rig.aborted shouldBe empty
    rig.advance(2.seconds)
    rig.reporter.check()
    rig.aborted.size shouldBe 1
    rig.aborted.peek should (include("StuckSpec") and include("uninterruptible"))
  }

  it should "not abort a test that stopped after the interrupt" in {
    val rig = Rig()
    rig.starts("interruptible")
    rig.advance(bound + 1.second)
    rig.reporter.check()
    rig.succeeds("interruptible")
    rig.advance(grace + 1.minute)
    rig.reporter.check()
    rig.aborted shouldBe empty
  }

  "The bounds" should "default to five minutes, above the slowest single test measured on any runner" in {
    // Slowest single test: FallbackRunsSpec, 117.6 s on Linux CI (#2557 audit), the next 53.3 s; macOS runs about
    // 3.4 times slower, so about 180 s for the next, under 300 s.
    TestHangBound.Default shouldBe 5.minutes
  }

  it should "give FallbackRunsSpec, whose one test took up to 400 s on macOS, more than the default" in {
    TestHangBound.boundFor(Some("com.serenity.ui.skia.FallbackRunsSpec")) should be > 400.seconds
    TestHangBound.boundFor(Some("com.serenity.SomeOtherSpec")) shouldBe TestHangBound.Default
    TestHangBound.boundFor(None) shouldBe TestHangBound.Default
  }

  it should "only name suites that exist, so a rename cannot silently drop an override" in
    TestHangBound.SuiteBounds.keys.foreach(name => Class.forName(name, false, getClass.getClassLoader))

  /** Runs `suite` on a thread of its own, as ScalaTest does, under a reporter watching it with a short bound. */
  private def runWatched(suite: org.scalatest.Suite, watchedBound: FiniteDuration): (List[Event], List[String]) =
    val events  = ConcurrentLinkedQueue[Event]()
    val emitted = ConcurrentLinkedQueue[String]()
    val watcher = TestHangBoundReporter(
      _ => watchedBound,
      () => System.nanoTime,
      () => 0L,
      TestHangBound.interruptThread,
      alert => s"REPORT $alert",
      emitted.add(_),
      cause => emitted.add(s"ABORT $cause"),
      30.seconds,
      Some(50.millis)
    )
    val recorder: Reporter = event =>
      events.add(event)
      watcher(event)
    val done = CountDownLatch(1)
    val body: Runnable = () =>
      try suite.run(None, Args(recorder))
      finally done.countDown()
    val runner = Thread(body, "ScalaTest-running-Watched")
    runner.start()
    done.await(30, TimeUnit.SECONDS) shouldBe true
    (events.asScala.toList, emitted.asScala.toList)

  "A test that hangs for ever" should "fail within the bound, with its name in the failure" in {
    val started           = System.nanoTime()
    val (events, emitted) = runWatched(HangingSuite(), 500.millis)
    val elapsed           = (System.nanoTime() - started).nanos
    val failed            = events.collect { case failure: TestFailed => failure }
    failed.map(_.testName) shouldBe List("A hanging suite should wait on an IO that never completes")
    elapsed should be < 20.seconds
    emitted.mkString("\n") should include("wait on an IO that never completes")
    emitted.mkString("\n") should not include "ABORT"
  }

  "A test that finishes inside the bound" should "pass, even one running most of the way to it" in {
    val (events, emitted) = runWatched(NearBoundSuite(), 3.seconds)
    events.collect { case failure: TestFailed => failure } shouldBe empty
    events.collect { case ok: TestSucceeded => ok.testName } shouldBe List(
      "A slow suite should sleep most of the bound"
    )
    emitted shouldBe empty
  }

end TestHangBoundSpec

@DoNotDiscover
private class HangingSuite extends AnyFlatSpec:
  "A hanging suite" should "wait on an IO that never completes" in
    IO.never[Unit].unsafeRunSync()

@DoNotDiscover
private class NearBoundSuite extends AnyFlatSpec:
  "A slow suite" should "sleep most of the bound" in
    IO.sleep(2.seconds).unsafeRunSync()
