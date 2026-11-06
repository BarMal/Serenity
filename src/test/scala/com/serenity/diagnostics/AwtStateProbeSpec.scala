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
