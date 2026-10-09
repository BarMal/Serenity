package com.serenity.app

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BuildLogLinesSpec extends AnyFlatSpec with Matchers:

  private val build =
    BuildLogLines.Build("1.2.0+3-abcd1234", "abcd1234ef567890", "2026-10-01T12:00:00+00:00", "nightly")

  private val properties = Map(
    "os.name"              -> "Linux",
    "os.version"           -> "6.1.0",
    "os.arch"              -> "amd64",
    "java.runtime.version" -> "21.0.4+7",
    "java.vm.vendor"       -> "Eclipse Adoptium",
    "jpackage.app-path"    -> "/opt/serenity/bin/Serenity"
  )

  "BuildLogLines.buildLine" should "carry version, commit, commit time, channel, OS, Java and launcher on one line" in {
    BuildLogLines.buildLine(build, properties.get) shouldBe
      "[BUILD] Serenity 1.2.0+3-abcd1234 (abcd1234ef567890, 2026-10-01T12:00:00+00:00) nightly; " +
      "Linux 6.1.0/amd64; Java 21.0.4+7 Eclipse Adoptium; launcher=/opt/serenity/bin/Serenity"
  }

  it should "name the launcher java when the app was not started by jpackage" in {
    BuildLogLines.buildLine(build, properties.removed("jpackage.app-path").get) should endWith("launcher=java")
  }

  it should "say unknown for system properties that are missing or empty" in {
    val line = BuildLogLines.buildLine(build, key => if key == "os.name" then Some("") else None)
    line should include("unknown unknown/unknown")
    line should include("Java unknown unknown")
  }

  "BuildLogLines.jvmArgumentsLine" should "list the heap and GC flags a support report needs, in order" in {
    val args = List("-XX:MaxRAMPercentage=25", "-XX:G1PeriodicGCInterval=60000", "-XX:+UseG1GC", "-Xss4m")
    BuildLogLines.jvmArgumentsLine(args) shouldBe
      "[JVM] arguments: -XX:MaxRAMPercentage=25 -XX:G1PeriodicGCInterval=60000 -XX:+UseG1GC -Xss4m"
  }

  it should "keep the CDS archive and crash-file paths the packaged launcher expands" in {
    val args = List("-XX:SharedArchiveFile=/home/u/.cache/serenity-1.0.0.jsa", "-XX:ErrorFile=/home/u/hs_err.log")
    BuildLogLines.jvmArgumentsLine(args) shouldBe
      "[JVM] arguments: -XX:SharedArchiveFile=/home/u/.cache/serenity-1.0.0.jsa -XX:ErrorFile=/home/u/hs_err.log"
  }

  it should "drop system properties, agents and class paths, which can carry private paths or secrets" in {
    val args =
      List("-Dsome.token=secret", "-javaagent:/x/agent.jar", "-agentlib:jdwp=x", "-Djava.class.path=/x", "-Xmx2g")
    BuildLogLines.jvmArgumentsLine(args) shouldBe "[JVM] arguments: -Xmx2g"
  }

  it should "drop boot class path options" in {
    BuildLogLines.jvmArgumentsLine(List("-Xbootclasspath/a:/x", "-Xmx1g")) shouldBe "[JVM] arguments: -Xmx1g"
  }

  it should "say none when no argument is worth reporting" in {
    BuildLogLines.jvmArgumentsLine(List("-Dx=y")) shouldBe "[JVM] arguments: none"
    BuildLogLines.jvmArgumentsLine(Nil) shouldBe "[JVM] arguments: none"
  }

  "BuildLogLines.current" should "describe this build and this JVM" in {
    val lines = BuildLogLines.current(List("-Xmx1g"))
    lines should have size 2
    lines.head should startWith("[BUILD] Serenity ")
    lines(1) shouldBe "[JVM] arguments: -Xmx1g"
  }
