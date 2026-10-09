package com.serenity.app

import java.time.Instant

import com.serenity.diagnostics.{CrashRecord, CrashReport, RuntimeIdentity}
import org.slf4j.LoggerFactory

/** Installs process-wide crash diagnostics for exceptions outside Cats Effect supervision. */
object CrashReporter:

  private val LoggerName = "com.serenity.app.CrashReporter"

  def message(thread: Thread): String =
    s"[RUNTIME] Uncaught exception on thread ${thread.getName}"

  def handler(record: (String, Throwable) => Unit): Thread.UncaughtExceptionHandler =
    (thread, error) => record(message(thread), error)

  /** Logs the crash, then leaves a crash file for the next launch to report. */
  def recordingTo(store: CrashRecord, identity: RuntimeIdentity, now: () => Instant)(
    log: (String, Throwable) => Unit
  ): (String, Throwable) => Unit =
    (message, error) =>
      log(message, error)
      val _ = store.writeCrash(CrashReport.render(identity, now(), message, Some(error), store.directory))

  def install(store: CrashRecord, identity: RuntimeIdentity): Unit =
    val logger = LoggerFactory.getLogger(LoggerName)
    Thread.setDefaultUncaughtExceptionHandler(
      handler(recordingTo(store, identity, () => Instant.now())(logger.error))
    )
