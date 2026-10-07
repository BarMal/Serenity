package com.serenity.testkit

import java.util.concurrent.CountDownLatch

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.events.{AlertProvided, NoteProvided, Ordinal}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HangReportSpec extends AnyFlatSpec with Matchers:

  private val blocked = HangReport.ThreadStack(
    "pool-7-thread-3-ScalaTest-running-StuckSpec",
    List("java.lang.Object.wait0(Native Method)", "com.serenity.StuckSpec.await(StuckSpec.scala:12)")
  )

  private def alert(message: String) =
    AlertProvided(new Ordinal(0), message, None, None, None, None, None, "t", 0L)

  @annotation.tailrec
  private def awaitState(thread: Thread, state: Thread.State, deadline: Long): Unit =
    if thread.getState != state && System.nanoTime() < deadline then
      Thread.onSpinWait()
      awaitState(thread, state, deadline)

  private def awaitWaiting(thread: Thread, deadline: Long): Unit = awaitState(thread, Thread.State.WAITING, deadline)

  private def fiveSecondsFromNow = System.nanoTime() + 5_000_000_000L

  @annotation.tailrec
  private def awaitCaptured(
    matching: HangReport.ThreadStack => Boolean,
    deadline: Long
  ): Option[HangReport.ThreadStack] =
    val found = HangReport.threadStacks().find(matching)
    if found.isDefined || System.nanoTime() >= deadline then found
    else
      Thread.onSpinWait()
      awaitCaptured(matching, deadline)

  private def parked(name: String) =
    HangReport.ThreadStack(
      name,
      List("java.base/jdk.internal.misc.Unsafe.park(Native Method)", "cats.effect.unsafe.WorkerThread.run"),
      state = "WAITING"
    )

  "A hang report" should "name the alert and show where each running suite is blocked" in {
    val text = HangReport.render("Test running for 120 seconds: StuckSpec: waits", List(blocked), Nil)

    text should include("[HANG-REPORT] Test running for 120 seconds: StuckSpec: waits")
    text should include("thread pool-7-thread-3-ScalaTest-running-StuckSpec")
    text should include("at com.serenity.StuckSpec.await(StuckSpec.scala:12)")
  }

  it should "report a deadlock the JVM detected" in {
    HangReport.render("alert", Nil, List("a", "b")) should include("JVM-detected deadlock between: a, b")
  }

  it should "cap the frames shown per thread" in {
    val deep = blocked.copy(frames = List.tabulate(500)(i => s"frame$i"))

    val text = HangReport.render("alert", List(deep), Nil)

    text should include("frame39")
    text should not include "frame40"
  }

  it should "stop at the ScalaTest runner frames, which are the same for every test" in {
    val stack =
      blocked.copy(frames = List("com.serenity.StuckSpec.await", "org.scalatest.Engine.run", "sbt.Execute.work"))

    val text = HangReport.render("alert", List(stack), Nil)

    text should include("com.serenity.StuckSpec.await")
    text should not include "org.scalatest.Engine.run"
    text should not include "sbt.Execute.work"
  }

  "The hang reporter" should "emit a report for an alert and ignore every other event" in {
    val emitted = new java.util.concurrent.atomic.AtomicReference(List.empty[String])
    val reporter = new HangReporter(
      text => emitted.updateAndGet(_ :+ text),
      () => List(blocked),
      () => Nil,
      () => RuntimeWatch.Liveness.Responsive,
      identity,
      _ => ()
    )

    reporter(NoteProvided(new Ordinal(0), "just a note", None, None, None, None, None, "t", 0L))
    reporter(alert("Test running for 60 seconds: S: t"))

    emitted.get.map(_.linesIterator.next()) shouldBe List("[HANG-REPORT] Test running for 60 seconds: S: t")
  }

  it should "see the stack of a thread running a suite" in {
    val release = new java.util.concurrent.CountDownLatch(1)
    val runner  = new Thread(() => release.await(), "pool-1-thread-1-ScalaTest-running-FakeSpec")
    runner.start()
    try
      awaitWaiting(runner, System.nanoTime() + 5_000_000_000L)
      val seen = HangReport.suiteThreadStacks().find(_.name.endsWith("ScalaTest-running-FakeSpec"))
      seen.map(_.frames.exists(_.contains("CountDownLatch"))) shouldBe Some(true)
    finally
      release.countDown()
      runner.join()
  }

  "A hang report of the runtime's threads" should "say what each thread waits on, who holds it, and what it holds" in {
    val stuck = HangReport.ThreadStack(
      "io-compute-2",
      List("com.serenity.Cache.get(Cache.scala:12)"),
      state = "BLOCKED",
      waitingOn = Some(HangReport.LockWait("java.lang.Object@1f", Some("io-compute-blocker-7"))),
      held = List("com.serenity.Cache@2a")
    )

    val text = HangReport.render("alert", List(stuck), Nil)

    text should include("thread io-compute-2 [BLOCKED] waiting on java.lang.Object@1f held by io-compute-blocker-7")
    text should include("holds com.serenity.Cache@2a")
    text should include("at com.serenity.Cache.get(Cache.scala:12)")
  }

  it should "list idle runtime threads on one line rather than printing a stack for each" in {
    val text = HangReport.render("alert", List(parked("io-compute-blocker-3"), parked("io-compute-1")), Nil)

    text should include("idle: io-compute-1, io-compute-blocker-3")
    text should not include "thread io-compute-1"
    text should not include "WorkerThread.run"
  }

  it should "stop a compute thread's stack where the fiber run loop takes over" in {
    val busy = HangReport.ThreadStack(
      "io-compute-0",
      List("com.serenity.Parse.run(Parse.scala:3)", "cats.effect.IOFiber.runLoop(IOFiber.scala:340)", "below"),
      state = "RUNNABLE"
    )

    val text = HangReport.render("alert", List(busy), Nil)

    text should include("com.serenity.Parse.run")
    text should not include "below"
  }

  "Choosing which threads to report" should "keep suite and runtime threads, and any thread blocked, holding a lock or initialising" in {
    val unrelated = HangReport.ThreadStack("pool-1-thread-1", List("java.lang.Object.wait0(Native Method)"), "WAITING")

    HangReport.isRelevant(blocked) shouldBe true
    HangReport.isRelevant(parked("io-compute-blocker-12")) shouldBe true
    HangReport.isRelevant(unrelated) shouldBe false
    HangReport.isRelevant(unrelated.copy(state = "BLOCKED")) shouldBe true
    HangReport.isRelevant(unrelated.copy(held = List("java.lang.Object@1"))) shouldBe true
    HangReport.isRelevant(unrelated.copy(frames = List("com.serenity.Thing$.<clinit>(Thing.scala:4)"))) shouldBe true
    HangReport.isRelevant(unrelated.copy(frames = List("scala.runtime.LazyVals$Waiting.await"))) shouldBe true
  }

  "Capturing the JVM's threads" should "show a thread blocked on a monitor and the thread that owns it" in {
    val monitor  = new Object
    val acquired = new CountDownLatch(1)
    val release  = new CountDownLatch(1)
    val owner = new Thread(
      () =>
        monitor.synchronized {
          acquired.countDown()
          release.await()
        },
      "hang-report-owner"
    )
    val waiter = new Thread(() => monitor.synchronized(()), "hang-report-waiter")
    owner.start()
    acquired.await()
    waiter.start()
    try
      awaitState(waiter, Thread.State.BLOCKED, fiveSecondsFromNow)
      val stacks = HangReport.threadStacks()

      val seenWaiter = stacks.find(_.name == "hang-report-waiter")
      seenWaiter.map(_.state) shouldBe Some("BLOCKED")
      seenWaiter.flatMap(_.waitingOn).flatMap(_.owner) shouldBe Some("hang-report-owner")
      stacks.find(_.name == "hang-report-owner").map(_.held.nonEmpty) shouldBe Some(true)
    finally
      release.countDown()
      owner.join()
      waiter.join()
  }

  it should "show a compute thread stuck inside an IO" in {
    val release = new CountDownLatch(1)
    val stuck   = IO(release.await()).unsafeToFuture()
    try
      val seen = awaitCaptured(
        stack => stack.name.startsWith("io-compute") && stack.frames.exists(_.contains("CountDownLatch.await")),
        fiveSecondsFromNow
      )
      seen.map(_.state) shouldBe Some("WAITING")
    finally
      release.countDown()
      scala.concurrent.Await.result(stuck, scala.concurrent.duration.Duration(5, "s"))
  }
end HangReportSpec
