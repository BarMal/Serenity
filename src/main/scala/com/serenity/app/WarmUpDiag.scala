package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import cats.effect.IO

/** DIAGNOSTIC, do not merge: stamps each phase of StartupWarmUp.run so a slow first call shows where its time went. */
object WarmUpDiag:

  private val started = System.nanoTime()
  private val file    = Path.of("target", "test-reports", "diag-startup-warmup.txt")

  def mark(label: String): IO[Unit] = IO.blocking(write(label))

  private def write(label: String): Unit =
    val sinceJvmMs = java.lang.management.ManagementFactory.getRuntimeMXBean.getUptime
    val sinceUseMs = (System.nanoTime() - started) / 1000000
    val line =
      s"jvmUptimeMs=$sinceJvmMs sinceFirstUseMs=$sinceUseMs thread=${Thread.currentThread.getName} $label\n"
    try
      val _ = Files.createDirectories(file.getParent)
      val _ = Files.write(
        file,
        line.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    catch case _: java.io.IOException => ()
