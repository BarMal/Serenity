package com.serenity.app

import org.slf4j.LoggerFactory

/** Installs process-wide crash diagnostics for exceptions outside Cats Effect supervision. */
object CrashReporter:

  private val LoggerName = "com.serenity.app.CrashReporter"

  /** Build the log message used for an uncaught exception on a JVM thread. */
  def message(thread: Thread): String =
    s"[RUNTIME] Uncaught exception on thread ${thread.getName}"

  /** Build an uncaught-exception handler around a testable recording function. */
  def handler(record: (String, Throwable) => Unit): Thread.UncaughtExceptionHandler =
    (thread, error) => record(message(thread), error)

  /** Register Serenity's default JVM uncaught-exception handler. */
  def install(): Unit =
    val logger = LoggerFactory.getLogger(LoggerName)
    Thread.setDefaultUncaughtExceptionHandler(handler(logger.error))
