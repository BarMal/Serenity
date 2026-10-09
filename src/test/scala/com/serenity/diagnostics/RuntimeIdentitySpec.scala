package com.serenity.diagnostics

import com.serenity.BuildInfo
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RuntimeIdentitySpec extends AnyFlatSpec with Matchers:

  private val properties = Map(
    "os.name"                       -> "Linux",
    "os.version"                    -> "6.1.0",
    "os.arch"                       -> "amd64",
    "java.vm.name"                  -> "OpenJDK 64-Bit Server VM",
    "java.runtime.version"          -> "21.0.4+7",
    "java.vendor"                   -> "Eclipse Adoptium",
    RuntimeIdentity.ToolkitProperty -> "X11 (no Wayland session)"
  )

  private val identity = RuntimeIdentity.of("1.2.3", "abc1234", properties.get)

  "RuntimeIdentity.of" should "read the operating system, JVM and toolkit from the properties" in {
    identity shouldBe RuntimeIdentity(
      version = "1.2.3",
      commit = "abc1234",
      os = "Linux 6.1.0 (amd64)",
      jvm = "OpenJDK 64-Bit Server VM 21.0.4+7 (Eclipse Adoptium)",
      toolkit = "X11 (no Wayland session)"
    )
  }

  it should "say unknown for anything the properties do not hold" in {
    val bare = RuntimeIdentity.of("1.2.3", "abc1234", _ => None)
    bare.os should include("unknown")
    bare.jvm should include("unknown")
    bare.toolkit shouldBe "unknown"
  }

  "RuntimeIdentity.summary" should "carry the version, commit, OS, JVM and toolkit on one line" in {
    identity.summary should not include "\n"
    List("1.2.3", "abc1234", "Linux 6.1.0 (amd64)", "OpenJDK 64-Bit Server VM 21.0.4+7", "X11").foreach { part =>
      identity.summary should include(part)
    }
  }

  "RuntimeIdentity.lines" should "list each fact on a line of its own, labelled" in {
    identity.lines shouldBe List(
      "Version: 1.2.3",
      "Commit: abc1234",
      "OS: Linux 6.1.0 (amd64)",
      "JVM: OpenJDK 64-Bit Server VM 21.0.4+7 (Eclipse Adoptium)",
      "Toolkit: X11 (no Wayland session)"
    )
  }

  "RuntimeIdentity.current" should "describe this build" in {
    val current = RuntimeIdentity.current
    current.version shouldBe BuildInfo.version
    current.commit shouldBe BuildInfo.commit
  }
