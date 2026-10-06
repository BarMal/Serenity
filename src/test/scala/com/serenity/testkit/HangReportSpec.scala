package com.serenity.testkit

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
  private def awaitWaiting(thread: Thread, deadline: Long): Unit =
    if thread.getState != Thread.State.WAITING && System.nanoTime() < deadline then
      Thread.onSpinWait()
      awaitWaiting(thread, deadline)

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
    val emitted  = new java.util.concurrent.atomic.AtomicReference(List.empty[String])
    val reporter = new HangReporter(text => emitted.updateAndGet(_ :+ text), () => List(blocked), () => Nil)

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
end HangReportSpec
