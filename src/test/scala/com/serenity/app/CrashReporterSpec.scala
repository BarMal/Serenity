package com.serenity.app

import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.status.Status
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

  // A resource file, not Scala -- scalac never sees it, so a malformed logback.xml (e.g. a stray "--" inside an XML
  // comment, forbidden by the XML spec even though it's fine in a Scala doc comment) only surfaces at runtime, the
  // moment CrashReporter.install's SLF4J call triggers logback's own config parsing. This drives the packaged
  // logback.xml through the same Joran configurator logback uses at startup and asserts it reports zero errors.
  it should "parse the packaged logback.xml without error" in {
    val context      = LoggerContext()
    val configurator = JoranConfigurator()
    configurator.setContext(context)
    configurator.doConfigure(getClass.getClassLoader.getResource("logback.xml"))

    val errors = context.getStatusManager.getCopyOfStatusList.asScala.filter(_.getLevel == Status.ERROR)
    errors shouldBe empty
  }
