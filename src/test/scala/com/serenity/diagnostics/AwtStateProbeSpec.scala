package com.serenity.diagnostics

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AwtStateProbeSpec extends AnyFlatSpec with Matchers:

  "AwtStateProbe.describe" should "give each window's class and whether it is displayable and showing" in {
    val lines = AwtStateProbe.describe(
      "after 8 s",
      AwtStateProbe.State(
        List(
          AwtStateProbe.WindowState("javax.swing.JFrame", displayable = true, showing = false),
          AwtStateProbe.WindowState("java.awt.Window", displayable = false, showing = false)
        ),
        pendingEvent = None
      )
    )

    lines.head shouldBe "[SHUTDOWN] awt after 8 s: 2 window(s), no event waiting on the event queue"
    lines should contain("[SHUTDOWN]   javax.swing.JFrame displayable=true showing=false")
    lines should contain("[SHUTDOWN]   java.awt.Window displayable=false showing=false")
  }

  it should "name the event waiting at the head of the event queue" in {
    val lines = AwtStateProbe.describe("x", AwtStateProbe.State(Nil, Some("java.awt.event.InvocationEvent")))

    lines.head should include("0 window(s)")
    lines.head should include("event waiting: java.awt.event.InvocationEvent")
  }

  "AwtStateProbe.current" should "read the toolkit without throwing, whatever display the machine has" in {
    noException should be thrownBy AwtStateProbe.current()
  }

  it should "not look at the event queue when no window exists, as that would start a toolkit nobody started" in {
    val state = AwtStateProbe.current(
      windows = () => Nil,
      pendingEvent = () => fail("the event queue was read although no window exists")
    )

    state shouldBe AwtStateProbe.State(Nil, None)
  }

  it should "report the event waiting when a window exists" in {
    val window = AwtStateProbe.WindowState("javax.swing.JFrame", displayable = true, showing = true)
    val state  = AwtStateProbe.current(windows = () => List(window), pendingEvent = () => Some("some.Event"))

    state shouldBe AwtStateProbe.State(List(window), Some("some.Event"))
  }

  "ShutdownWatchdog.armHalt" should "report two AWT snapshots, the second after the first, before it halts" in {
    val snapshots = new AtomicInteger(0)
    val reported  = new AtomicReference[String]("")
    val haltedIn  = new CountDownLatch(1)
    val source = () =>
      val n = snapshots.incrementAndGet()
      AwtStateProbe.State(List(AwtStateProbe.WindowState(s"Window$n", displayable = true, showing = n == 1)), None)

    new ShutdownWatchdog(ShutdownLedger.recording())
      .armHalt(
        bound = 300.millis,
        exitCode = 70,
        report = reported.set,
        halt = _ => haltedIn.countDown(),
        awtState = source,
        snapshotGap = 100.millis
      )
      .unsafeRunSync()

    haltedIn.await(5, TimeUnit.SECONDS) shouldBe true
    snapshots.get shouldBe 2
    val text = reported.get
    text.indexOf("Window1") should (be >= 0 and be < text.indexOf("Window2"))
  }

  it should "still report and halt when the AWT state cannot be read" in {
    val reported = new AtomicReference[String]("")
    val haltedIn = new CountDownLatch(1)

    new ShutdownWatchdog(ShutdownLedger.recording())
      .armHalt(
        bound = 200.millis,
        exitCode = 70,
        report = reported.set,
        halt = _ => haltedIn.countDown(),
        awtState = () => throw new IllegalStateException("no toolkit"),
        snapshotGap = 50.millis
      )
      .unsafeRunSync()

    haltedIn.await(5, TimeUnit.SECONDS) shouldBe true
    reported.get should include("awt state unavailable: no toolkit")
  }

  it should "not halt when disarmed while the AWT read is running, even if that read swallows the interrupt" in {
    val halted   = new AtomicReference[Option[Int]](None)
    val reading  = new CountDownLatch(1)
    val disarmed = new CountDownLatch(1)
    val finished = new CountDownLatch(1)
    val reported = new AtomicInteger(0)
    val dog      = new ShutdownWatchdog(ShutdownLedger.recording())
    val swallowsInterrupt = () =>
      reading.countDown()
      try disarmed.await()
      catch case _: InterruptedException => ()
      AwtStateProbe.State(Nil, None)

    dog
      .armHalt(
        bound = 50.millis,
        exitCode = 70,
        report = _ =>
          reported.incrementAndGet()
          finished.countDown()
        ,
        halt = code =>
          halted.set(Some(code))
          finished.countDown()
        ,
        awtState = swallowsInterrupt,
        snapshotGap = 50.millis
      )
      .unsafeRunSync()
    reading.await(5, TimeUnit.SECONDS) shouldBe true
    dog.disarm.unsafeRunSync()
    disarmed.countDown()

    finished.await(1, TimeUnit.SECONDS) shouldBe false
    halted.get shouldBe None
    reported.get shouldBe 0
  }
