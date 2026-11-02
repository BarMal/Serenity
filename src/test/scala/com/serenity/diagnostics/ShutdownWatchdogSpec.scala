package com.serenity.diagnostics

import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShutdownWatchdogSpec extends AnyFlatSpec with Matchers:

  private def watchdog(ledger: ShutdownLedger = ShutdownLedger.recording()): ShutdownWatchdog = new ShutdownWatchdog(
    ledger
  )

  private val mainSource: String = Files.readString(Paths.get("src", "main", "scala", "Main.scala"))

  private def awaitTrue(within: FiniteDuration)(condition: => Boolean): Boolean =
    val deadline = System.nanoTime() + within.toNanos
    while !condition && System.nanoTime() < deadline do Thread.sleep(20)
    condition

  "The shutdown bounds" should "be the owner's: 10 s for the hook, with the dump 2 s before it, at 8 s" in {
    ShutdownWatchdog.HookTimeout shouldBe 10.seconds
    ShutdownWatchdog.DumpBeforeTimeout shouldBe 2.seconds
    (ShutdownWatchdog.HookTimeout - ShutdownWatchdog.DumpBeforeTimeout) shouldBe 8.seconds
  }

  "ShutdownWatchdog.describe" should "name the open shutdown steps and give every thread's state and full stack" in {
    val text = ShutdownWatchdog.describe(
      "the hook has run for 8 s",
      List("window", "paint executor"),
      List(
        ShutdownWatchdog.ThreadSnapshot("main", daemon = false, "WAITING", List("a.B.c(B.java:1)", "a.B.d(B.java:2)")),
        ShutdownWatchdog.ThreadSnapshot("AWT-EventQueue-0", daemon = false, "RUNNABLE", Nil)
      )
    )

    val lines = text.linesIterator.toList
    lines.head shouldBe "[SHUTDOWN] the hook has run for 8 s"
    lines should contain("[SHUTDOWN] steps that started releasing and have not finished: window, paint executor")
    lines should contain("\"main\" (WAITING)")
    lines should contain("\tat a.B.c(B.java:1)")
    lines should contain("\tat a.B.d(B.java:2)")
    lines should contain("\"AWT-EventQueue-0\" (RUNNABLE)")
  }

  it should "mark a daemon thread" in {
    val text = ShutdownWatchdog.describe(
      "x",
      Nil,
      List(ShutdownWatchdog.ThreadSnapshot("io-compute-1", daemon = true, "TIMED_WAITING", Nil))
    )

    text should include("\"io-compute-1\" (TIMED_WAITING, daemon)")
  }

  it should "say so when no step is open, since that points away from a stuck release" in {
    val text = ShutdownWatchdog.describe("x", Nil, Nil)

    text should include("no step is releasing")
  }

  "ShutdownWatchdog.captureThreads" should "include the calling thread with a real stack" in {
    val mine = ShutdownWatchdog.captureThreads().find(_.name == Thread.currentThread().getName)

    mine.map(_.stack.exists(_.contains("ShutdownWatchdogSpec"))) shouldBe Some(true)
  }

  "ShutdownWatchdog.armHalt" should "report and halt with the given exit code once the bound has passed" in {
    val halted   = new AtomicReference[Option[Int]](None)
    val reported = new AtomicReference[String]("")

    watchdog()
      .armHalt(200.millis, exitCode = 70, report = text => reported.set(text), halt = code => halted.set(Some(code)))
      .unsafeRunSync()

    awaitTrue(5.seconds)(halted.get.isDefined) shouldBe true
    halted.get shouldBe Some(70)
    reported.get should include("[SHUTDOWN]")
  }

  it should "name the steps its own ledger has open" in {
    val ledger   = ShutdownLedger.recording()
    val reported = new AtomicReference[String]("")
    ledger.started("window")

    watchdog(ledger).armHalt(100.millis, 70, text => reported.set(text), _ => ()).unsafeRunSync()

    awaitTrue(5.seconds)(reported.get.nonEmpty) shouldBe true
    reported.get should include("steps that started releasing and have not finished: window")
  }

  it should "do nothing when disarmed before the bound" in {
    val halted = new AtomicReference[Option[Int]](None)

    val dog = watchdog()
    (dog.armHalt(300.millis, 70, _ => (), code => halted.set(Some(code))) *> dog.disarm).unsafeRunSync()
    Thread.sleep(900)

    halted.get shouldBe None
  }

  it should "be replaced by arming again, so only the newest bound counts" in {
    val halted = new AtomicReference[Option[Int]](None)

    val dog = watchdog()
    (dog.armHalt(300.millis, 70, _ => (), code => halted.set(Some(code))) *>
      dog.armHalt(30.seconds, 71, _ => (), code => halted.set(Some(code))))
      .unsafeRunSync()
    Thread.sleep(900)
    dog.disarm.unsafeRunSync()

    halted.get shouldBe None
  }

  "Main" should "bound the shutdown hook, dump before it runs out, and halt a quit that does not finish" in {
    mainSource should include("ShutdownWatchdog.withHookTimeout(")
    mainSource should include("watchdog.installSignalDump()")
    mainSource should include("watchdog.armHalt(")
  }

  it should "disarm the halt before a restart, since the next editor runs in the same JVM" in {
    val restart = mainSource.substring(mainSource.indexOf("case Some(mode) =>"))

    restart.indexOf("watchdog.disarm") should (be >= 0 and be < restart.indexOf("launchUntilSettled("))
  }
