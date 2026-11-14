package com.serenity.testkit

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Specs take the cell metrics of a font from [[TestMeasure]], so that switching what layout measures with is one
  * change in one place. Only the file that is about `CellMetrics.fromFont` itself, and the helper, call it.
  */
final class TestMeasureUsageSpec extends AnyFlatSpec with Matchers:

  private val allowed = Set("TestMeasure.scala", "CellMetricsSpec.scala", "TestMeasureUsageSpec.scala")

  private def isCode(line: String): Boolean =
    val trimmed = line.trim
    !trimmed.startsWith("//") && !trimmed.startsWith("*") && !trimmed.startsWith("/*")

  "No spec but the helper and the CellMetrics spec" should "call CellMetrics.fromFont" in {
    val callers = Using.resource(Files.walk(Paths.get("src", "test"))) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".scala"))
        .filterNot(path => allowed.contains(path.getFileName.toString))
        .filter(path =>
          Files.readAllLines(path).asScala.exists(line => isCode(line) && line.contains("CellMetrics.fromFont"))
        )
        .map(_.toString)
        .toList
        .sorted
    }
    callers shouldBe Nil
  }
