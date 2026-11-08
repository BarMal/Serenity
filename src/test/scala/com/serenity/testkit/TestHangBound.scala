package com.serenity.testkit

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.{Executors, TimeUnit}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import org.scalatest.Reporter
import org.scalatest.events.{
  Event,
  SuiteAborted,
  SuiteCompleted,
  TestCanceled,
  TestFailed,
  TestPending,
  TestStarting,
  TestSucceeded
}

/** How long any single test may run before the suite run gives up on it.
  *
  * Applied to every suite by [[TestHangBoundReporter]], which `build.sbt` registers with ScalaTest's `-C`: no suite
  * extends or mixes in anything. A test that outlives its bound has its thread interrupted, which fails it, under its
  * own name, with a thread dump in the log; the run goes on with the next test.
  */
object TestHangBound:

  /** Above the slowest single test measured on any runner. The audit of #2557 found `FallbackRunsSpec`'s one test at
    * 117.6 s on Linux and the next slowest at 53.3 s; macOS ran that suite up to 400 s, 3.4 times Linux, which puts the
    * next slowest near 180 s. Under the job guards (40 and 52 minutes) it turns a hang from losing the whole job, with
    * no test name, into one failed test.
    */
  val Default: FiniteDuration = 5.minutes

  /** Suites with a test the default would fail on a slow runner. A suite is named by its class so that this stays a
    * table, not a trait every such suite must remember to mix in; [[TestHangBoundSpec]] checks each name resolves.
    */
  val SuiteBounds: Map[String, FiniteDuration] = Map(
    // One test shapes every allowlisted code point on every font: 117.6 s on Linux, up to 400 s on macOS.
    "com.serenity.ui.skia.FallbackRunsSpec" -> 15.minutes
  )

  def boundFor(suiteClassName: Option[String]): FiniteDuration =
    suiteClassName.flatMap(SuiteBounds.get).getOrElse(Default)

  /** The thread ScalaTest named in the test's start event; a test that has already finished and a thread already gone
    * are both nothing to interrupt.
    */
  def interruptThread(name: String): Unit =
    Thread.getAllStackTraces.keySet.asScala.find(_.getName == name).foreach(_.interrupt())

end TestHangBound

/** Interrupts any test that runs past its [[TestHangBound]], and aborts the run, naming the test, if it ignores the
  * interrupt.
  *
  * Time is counted as [[HangBound]] counts it: wall time less the time the collectors had the JVM stopped, so a long
  * pause on a loaded runner cannot fail a test that was about to finish. A test under `TestControl` runs in real time
  * on its own thread and is bound like any other; the mocked clock is not read.
  *
  * Caution, here be imagine dragons: that an interrupt unblocks a test depends on the test blocking in something
  * interruptible. `unsafeRunSync` on a Cats Effect `IO` is (the bound's own spec runs an `IO.never` to check); a busy
  * loop or an uninterruptible region is not, and the abort after `grace` is for those.
  *
  * @param report
  *   renders the log entry for an overdue test, given the line that names it
  * @param period
  *   how often to check, or `None` to leave [[check]] to the caller
  */
final class TestHangBoundReporter(
    boundFor: Option[String] => FiniteDuration,
    nanoTime: () => Long,
    paused: HangBound.PausedTime,
    interrupt: String => Unit,
    report: String => String,
    emit: String => Unit,
    abort: String => Unit,
    grace: FiniteDuration,
    period: Option[FiniteDuration]
) extends Reporter:

  def this() = this(
    TestHangBound.boundFor,
    () => System.nanoTime,
    HangBound.PausedTime.jvm,
    TestHangBound.interruptThread,
    alert => HangReport.render(alert, HangReport.threadStacks(), HangReport.deadlockedThreadNames()),
    RuntimeWatch.print,
    RuntimeWatch.abortRun,
    1.minute,
    Some(5.seconds)
  )

  final private case class Key(suiteId: String, testName: String)

  final private case class Running(
      suiteName: String,
      suiteClassName: Option[String],
      testName: String,
      thread: String,
      startedNanos: Long,
      pausedAtStartMillis: Long,
      interruptedAtNanos: Option[Long]
  ):
    def label: String = s"$suiteName: $testName"

  private val running = AtomicReference(Map.empty[Key, Running])
  private val ticking = AtomicBoolean(false)

  override def apply(event: Event): Unit = event match
    case started: TestStarting =>
      running.updateAndGet(
        _.updated(
          Key(started.suiteId, started.testName),
          Running(
            started.suiteName,
            started.suiteClassName,
            started.testName,
            started.threadName,
            nanoTime(),
            paused.millis(),
            None
          )
        )
      )
      startTicking()
    case done: TestSucceeded  => finished(done.suiteId, done.testName)
    case done: TestFailed     => finished(done.suiteId, done.testName)
    case done: TestCanceled   => finished(done.suiteId, done.testName)
    case done: TestPending    => finished(done.suiteId, done.testName)
    case done: SuiteCompleted => forgetSuite(done.suiteId)
    case done: SuiteAborted   => forgetSuite(done.suiteId)
    case _                    => ()

  private def finished(suiteId: String, testName: String): Unit =
    running.updateAndGet(_ - Key(suiteId, testName))
    ()

  private def forgetSuite(suiteId: String): Unit =
    running.updateAndGet(_.filterNot((key, _) => key.suiteId == suiteId))
    ()

  private def startTicking(): Unit =
    period.foreach { every =>
      if ticking.compareAndSet(false, true) then
        val scheduler = Executors.newSingleThreadScheduledExecutor { task =>
          val thread = Thread(task, "test-hang-bound")
          thread.setDaemon(true)
          thread
        }
        scheduler.scheduleWithFixedDelay(
          () =>
            Try(check()); ()
          ,
          every.toMillis,
          every.toMillis,
          TimeUnit.MILLISECONDS
        )
        ()
    }

  /** Looks at every test now running: interrupts the overdue, aborts the run for one that outlived its interrupt. */
  def check(): Unit =
    val now       = nanoTime()
    val pausedNow = paused.millis()
    running.get.foreach { (key, test) =>
      val unpaused = (now - test.startedNanos).nanos - (pausedNow - test.pausedAtStartMillis).millis
      test.interruptedAtNanos match
        case None if unpaused >= boundFor(test.suiteClassName) => interruptOverdue(key, test, unpaused, now)
        case Some(at) if (now - at).nanos >= grace             => abortStuck(test)
        case _                                                 => ()
    }

  private def interruptOverdue(key: Key, test: Running, unpaused: FiniteDuration, now: Long): Unit =
    val marked = running.updateAndGet(_.updatedWith(key)(_.map(_.copy(interruptedAtNanos = Some(now)))))
    if marked.contains(key) then
      emit(
        report(
          s"[TEST-HANG] ${test.label} has run for ${unpaused.toSeconds} s without the JVM paused, past its " +
            s"${boundFor(test.suiteClassName).toSeconds} s bound; interrupting ${test.thread}"
        )
      )
      interrupt(test.thread)

  private def abortStuck(test: Running): Unit =
    val cause = s"[TEST-HANG] ${test.label} is still running ${grace.toSeconds} s after it was interrupted"
    emit(report(cause))
    abort(cause)

end TestHangBoundReporter
