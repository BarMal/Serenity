package com.serenity.app

import org.slf4j.LoggerFactory

/** Installs process-wide crash diagnostics for exceptions outside Cats Effect supervision. */
object CrashReporter:

  private val LoggerName = "com.serenity.app.CrashReporter"

  def message(thread: Thread): String =
    s"[RUNTIME] Uncaught exception on thread ${thread.getName}"

  def handler(record: (String, Throwable) => Unit): Thread.UncaughtExceptionHandler =
    (thread, error) => record(message(thread), error)

  def install(): Unit =
    val logger = LoggerFactory.getLogger(LoggerName)
    Thread.setDefaultUncaughtExceptionHandler(handler(logger.error))
