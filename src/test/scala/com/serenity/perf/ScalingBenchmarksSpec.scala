package com.serenity.perf

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rows that ask how performance scales with many open buffers and split panes (#2478). */
class ScalingBenchmarksSpec extends AnyFlatSpec with Matchers:

  private lazy val rows = ScalingBenchmarks.benchmarks()

  private val expectedNames: Set[String] =
    (for
      open <- List(1, 20)
      kind <- List("code", "prose")
    yield s"buffers.switch_cold_frame.open_$open.$kind").toSet ++
      (for
        panes  <- List(1, 2, 4)
        action <- List("typing_frame", "scroll_line_frame", "page_down_frame")
      yield s"panes.split$panes.$action").toSet +
      "scrollbar.four_panes.all_bars"

  "The scaling rows" should "be the four cold-frame rows, the nine split-frame rows and the late-bar row" in {
    rows.map(_.name).toSet shouldBe expectedNames
    rows.map(_.name).distinct should have size rows.size
  }

  it should "all be on the record-only list, so none gates a build before the owner has read its numbers" in {
    val prefixes = Files
      .readAllLines(Paths.get("scripts", "perf_record_only.txt"))
      .asScala
      .map(_.split("#", 2).head.trim)
      .filter(_.nonEmpty)

    rows.map(_.name).filterNot(name => prefixes.exists(name.startsWith)) shouldBe Nil
  }

  it should "verify, and then run once, without failing" in
    rows.foreach { row =>
      withClue(row.name) {
        row.verify()
        row.run()
      }
    }
