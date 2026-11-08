package com.serenity.host

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompatibilitySpec extends AnyFlatSpec with Matchers:

  private val running = LaunchIdentity(
    argv = List("metals"),
    cwd = "/work/serenity",
    envFingerprint = "env-1",
    initializeFingerprint = "init-1"
  )

  private val protocol = 1

  "Compatibility" should "reuse a host that matches on every field" in {
    Compatibility.check(running, protocol, running, protocol) shouldBe Compatibility.Reusable
  }

  it should "refuse reuse and name each field that differs" in {
    def mismatch(requested: LaunchIdentity, requestedProtocol: Int = protocol) =
      Compatibility.check(running, protocol, requested, requestedProtocol)

    mismatch(running.copy(argv = List("metals", "--x"))) shouldBe Compatibility.Mismatch(List(Difference.Argv))
    mismatch(running.copy(cwd = "/work/other")) shouldBe Compatibility.Mismatch(List(Difference.WorkingDirectory))
    mismatch(running.copy(envFingerprint = "env-2")) shouldBe Compatibility.Mismatch(List(Difference.Environment))
    mismatch(running.copy(initializeFingerprint = "init-2")) shouldBe Compatibility.Mismatch(
      List(Difference.Initialize)
    )
    mismatch(running, requestedProtocol = 2) shouldBe Compatibility.Mismatch(List(Difference.ProtocolVersion))
  }

  it should "list several differences in a fixed order" in {
    val requested = running.copy(argv = Nil, envFingerprint = "env-2")

    Compatibility.check(running, protocol, requested, 2) shouldBe
      Compatibility.Mismatch(List(Difference.Argv, Difference.Environment, Difference.ProtocolVersion))
  }

  "EnvFingerprint" should "not depend on the order of the variables" in {
    val keys = Set("PATH", "JAVA_HOME")

    EnvFingerprint.of(Map("PATH" -> "/bin", "JAVA_HOME" -> "/jdk"), keys) shouldBe
      EnvFingerprint.of(Map("JAVA_HOME" -> "/jdk", "PATH" -> "/bin"), keys)
  }

  it should "change when a chosen variable changes, and ignore the others" in {
    val keys = Set("PATH")
    val base = EnvFingerprint.of(Map("PATH" -> "/bin", "TERM" -> "xterm"), keys)

    EnvFingerprint.of(Map("PATH" -> "/usr/bin", "TERM" -> "xterm"), keys) should not be base
    EnvFingerprint.of(Map("PATH" -> "/bin", "TERM" -> "dumb"), keys) shouldBe base
  }

  it should "tell an unset variable from one set to nothing" in {
    val keys = Set("JAVA_HOME")

    EnvFingerprint.of(Map.empty, keys) should not be EnvFingerprint.of(Map("JAVA_HOME" -> ""), keys)
  }

  it should "not confuse values that merely run together" in {
    val keys = Set("A", "B")

    EnvFingerprint.of(Map("A" -> "x=1", "B" -> "2"), keys) should not be
      EnvFingerprint.of(Map("A" -> "x", "B" -> "1=2"), keys)
  }
