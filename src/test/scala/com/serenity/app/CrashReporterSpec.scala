package com.serenity.app

import java.util.concurrent.atomic.AtomicReference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CrashReporterSpec extends AnyFlatSpec with Matchers:

  "CrashReporter" should "format uncaught exception messages with the thread name" in {
    val thread = Thread(() => (), "serenity-edt")

    CrashReporter.message(thread).shouldBe("[RUNTIME] Uncaught exception on thread serenity-edt")
  }

  it should "delegate uncaught exceptions to the supplied recorder" in {
    val recorded = AtomicReference[Option[(String, Throwable)]](None)
    val error    = RuntimeException("idle crash")
    val thread   = Thread(() => (), "AWT-EventQueue-0")

    val handler = CrashReporter.handler((message, throwable) => recorded.set(Some((message, throwable))))

    handler.uncaughtException(thread, error)

    recorded
      .get()
      .shouldBe(
        Some(
          ("[RUNTIME] Uncaught exception on thread AWT-EventQueue-0", error)
        )
      )
  }
