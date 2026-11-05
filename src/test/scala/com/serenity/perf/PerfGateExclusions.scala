package com.serenity.perf

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*

/** The rows `scripts/perf-gate-excluded-rows.txt` keeps off the perf gate. As in `compare_perf_runs.py`, a name covers
  * itself and its `.variant` rows.
  */
private[perf] object PerfGateExclusions:

  private lazy val entries: List[String] =
    Files
      .readAllLines(Paths.get("scripts", "perf-gate-excluded-rows.txt"))
      .asScala
      .toList
      .map(_.split("#", 2).head.trim)
      .filter(_.nonEmpty)

  def covers(name: String): Boolean = entries.exists(entry => name == entry || name.startsWith(entry + "."))
