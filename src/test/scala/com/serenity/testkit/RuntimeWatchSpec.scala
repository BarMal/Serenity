package com.serenity.testkit

import java.lang.Thread.UncaughtExceptionHandler
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

import cats.effect.unsafe.IORuntime
import org.scalatest.events.{AlertProvided, Ordinal}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import RuntimeWatch.Liveness

class RuntimeWatchSpec extends AnyFlatSpec with Matchers:

  private val oneSecond = 1.second

  private def withRuntime[A](test: IORuntime => A): A =
    val runtime = IORuntime.builder().build()
    try test(runtime)
    finally runtime.shutdown()

  private def neverRunsAnything: IORuntime =
    val dormant = new ExecutionContext:
      def execute(runnable: Runnable): Unit     = ()
      def reportFailure(cause: Throwable): Unit = ()
    IORuntime.builder().setCompute(dormant, () => ()).build()

  private def slowTestAlert =
    AlertProvided(new Ordinal(0), "Test running for 60 seconds: S: t", None, None, None, None, None, "t", 0L)

  @tailrec
  private def spinUntil(done: AtomicBoolean): Unit =
    if !done.get then
      Thread.onSpinWait()
      spinUntil(done)

  "The liveness probe" should "find a running runtime responsive" in withRuntime { runtime =>
    RuntimeWatch.probe(runtime, 10.seconds) shouldBe Liveness.Responsive
  }

  it should "report a shut-down runtime as unresponsive" in {
    val dead = IORuntime.builder().build()
    dead.shutdown()

    RuntimeWatch.probe(dead, oneSecond) shouldBe a[Liveness.Unresponsive]
  }

  it should "report a runtime whose compute pool never runs anything as unresponsive, within the deadline" in {
    val started = System.nanoTime()

    val verdict = RuntimeWatch.probe(neverRunsAnything, oneSecond)

    verdict shouldBe Liveness.Unresponsive("no answer to a trivial IO within 1 second")
    (System.nanoTime() - started) should be < 10_000_000_000L
  }

  "The uncaught-exception handler" should "print the thread name and the full stack trace" in {
    val emitted = AtomicReference(List.empty[String])
    val handler = RuntimeWatch.uncaughtHandler(text => emitted.updateAndGet(_ :+ text), None)

    handler.uncaughtException(Thread("io-compute-3"), new IllegalStateException("pool gone"))

    val text = emitted.get.mkString
    text should include("[RUNTIME-DEATH] uncaught java.lang.IllegalStateException on thread io-compute-3")
    text should include("pool gone")
    text should include("com.serenity.testkit.RuntimeWatchSpec")
  }

  it should "still pass the failure to the handler that was installed before it" in {
    val seen                               = AtomicReference(List.empty[String])
    val previous: UncaughtExceptionHandler = (thread, _) => seen.updateAndGet(_ :+ thread.getName)
    val handler                            = RuntimeWatch.uncaughtHandler(_ => (), Some(previous))

    handler.uncaughtException(Thread("elsewhere"), new RuntimeException)

    seen.get shouldBe List("elsewhere")
  }

  "Installing the watch" should "make the JVM-wide handler report to stdout, and keep returning the same runtime" in {
    val first = RuntimeWatch.install()

    RuntimeWatch.install() should be theSameInstanceAs first
    Thread.getDefaultUncaughtExceptionHandler should not be null
  }

  "The pool description" should "count workers and blockers, and name interrupted threads" in {
    def thread(name: String, interrupted: Boolean = false) =
      HangReport.ThreadStack(name, Nil, "WAITING", interrupted = interrupted)
    val stacks = List(
      thread("io-compute-0"),
      thread("io-compute-1", interrupted = true),
      thread("io-compute-blocker-4"),
      thread("ScalaTest-main")
    )

    val text = RuntimeWatch.describePool(probedIsCurrent = true, stacks).mkString("\n")

    text should include("io-compute workers: 2 [2 WAITING]")
    text should include("io-compute-blocker threads: 1")
    text should include("interrupted runtime threads: io-compute-1")
    text should include("still the global one")
  }

  it should "say when the probed runtime is not the global one any more" in {
    RuntimeWatch.describePool(probedIsCurrent = false, Nil).mkString should include("shut down and the global replaced")
  }

  "The unresponsive report" should "name the runtime, describe its pool, and dump every thread including io-* ones" in
    withRuntime { runtime =>
      val report = RuntimeWatch.unresponsiveReport("no answer", runtime, runtime)

      report should include("GLOBAL IORuntime unresponsive: no answer")
      report should include("io-compute workers:")
      report should include(s"thread ${Thread.currentThread.getName}")
      report should include("thread io-compute-0")
      report should include("at ")
    }

  "Capturing threads" should "flag a thread whose interrupt flag is set" in {
    val done    = AtomicBoolean(false)
    val spinner = Thread(() => spinUntil(done), "interrupted-spinner")
    spinner.start()
    try
      spinner.interrupt()

      HangReport.allThreadStacks().find(_.name == "interrupted-spinner").map(_.interrupted) shouldBe Some(true)
      HangReport.renderDump(HangReport.allThreadStacks()) should include("thread interrupted-spinner [")
    finally
      done.set(true)
      spinner.join()
  }

  "The hang reporter" should "abort the run with the cause when the runtime is unresponsive" in {
    val emitted = AtomicReference(List.empty[String])
    val aborted = AtomicReference(Option.empty[String])
    val reporter = new HangReporter(
      text => emitted.updateAndGet(_ :+ text),
      () => Nil,
      () => Nil,
      () => Liveness.Unresponsive("no answer"),
      cause => s"GLOBAL IORuntime unresponsive: $cause",
      cause => aborted.set(Some(cause))
    )

    reporter(slowTestAlert)

    emitted.get.last shouldBe "GLOBAL IORuntime unresponsive: no answer"
    aborted.get shouldBe Some("no answer")
  }

  it should "leave a responsive runtime alone" in {
    val aborted = AtomicReference(Option.empty[String])
    val reporter = new HangReporter(
      _ => (),
      () => Nil,
      () => Nil,
      () => Liveness.Responsive,
      identity,
      cause => aborted.set(Some(cause))
    )

    reporter(slowTestAlert)

    aborted.get shouldBe None
  }
end RuntimeWatchSpec
