package com.serenity.diagnostics

import java.nio.file.Path
import java.time.Instant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CrashReportSpec extends AnyFlatSpec with Matchers:

  private val identity = RuntimeIdentity.of("1.2.3", "abc1234", _ => None)
  private val at       = Instant.parse("2026-10-06T08:30:00Z")
  private val logs     = Path.of(System.getProperty("java.io.tmpdir"), "serenity-logs")

  "CrashReport.render" should "name the build, the time, the headline and where the logs are" in {
    val report = CrashReport.render(identity, at, "[RUNTIME] render loop failed", None, logs)

    report should include("Serenity 1.2.3")
    identity.lines.foreach(line => report should include(line))
    report should include("2026-10-06T08:30:00Z")
    report should include("[RUNTIME] render loop failed")
    report should include(logs.toString)
  }

  it should "include the exception and its causes with stack traces" in {
    val error  = IllegalStateException("outer failure", IllegalArgumentException("inner cause"))
    val report = CrashReport.render(identity, at, "boom", Some(error), logs)

    report should include("java.lang.IllegalStateException: outer failure")
    report should include("Caused by: java.lang.IllegalArgumentException: inner cause")
    report should include("CrashReportSpec")
  }

  "CrashReport.unrecordedExit" should "say no error was recorded and name the build that was running" in {
    val report = CrashReport.unrecordedExit(Some("Serenity 1.0.0 (old)"), identity, at, logs)

    report should include("without a clean exit")
    report should include("Serenity 1.0.0 (old)")
    report should include(logs.toString)
    identity.lines.foreach(line => report should include(line))
  }

  it should "still read sensibly when the running build was not recorded" in {
    CrashReport.unrecordedExit(None, identity, at, logs) should include("without a clean exit")
  }
