package com.serenity

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestSuiteStyleSpec extends AnyFlatSpec with Matchers:

  private val testRoot = Paths.get("src", "test", "scala", "com", "serenity")

  "Normal test suites" should "avoid debug-era names and direct console output" in {
    val violations = scalaTestSources.flatMap { path =>
      val relative = testRoot.relativize(path).toString.replace('\\', '/')
      val content  = Files.readString(path)

      List(
        Option.when(relative.contains("Debug"))(s"$relative uses a debug-era file name"),
        Option.when(DebugSuitePattern.findFirstIn(content).nonEmpty)(s"$relative uses a debug-era suite name"),
        Option.when(ConsoleOutputPattern.findFirstIn(content).nonEmpty)(
          s"$relative writes to stdout during normal tests"
        ),
        Option.when(DisabledAnnotationPattern.findFirstIn(content).nonEmpty)(
          s"$relative disables a normal test suite"
        ),
        Option.when(DisabledTestCallPattern.findFirstIn(content).nonEmpty)(
          s"$relative disables a normal test case"
        )
      ).flatten
    }

    violations shouldBe Nil
  }

  private def scalaTestSources: List[Path] =
    Using.resource(Files.walk(testRoot)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path))
        .filter(path => path.toString.endsWith(".scala"))
        .filterNot(path => testRoot.relativize(path).toString.replace('\\', '/').startsWith("perf/"))
        .toList
    }

  private val DebugSuitePattern         = (raw"\bclass\s+" + "Deb" + "ug" + raw"\w*Sp" + "ec" + raw"\b").r
  private val ConsoleOutputPattern      = ("println" + raw"\s*\(").r
  private val DisabledAnnotationPattern = ("@" + "Ignore").r
  private val DisabledTestCallPattern   = (raw"\b" + "ignore" + raw"\s*\(").r
